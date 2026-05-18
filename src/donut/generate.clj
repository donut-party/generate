(ns donut.generate
  "Write code generators that can be executed from the REPL"
  (:require
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [clojure.walk :as walk]
   [donut.sugar.utils :as dsu]
   [rewrite-clj.zip :as rz]))

(def ^:dynamic *error-handler* nil)
(def ^:dynamic *info-handler* nil)

;;------
;; generator helpers
;;------

(defn point-path
  [point]
  (get-in point [:destination :path]))

;;---
;; using rewrite-clj to modify source
;;---

(defn find-value-parent
  [loc value]
  (rz/up (rz/find-value loc rz/next value)))

(def nav-substitutions
  {::left      rz/left
   ::right     rz/right
   ::up        rz/up
   ::down      rz/down
   ::prev      rz/prev
   ::next      rz/next
   ::leftmost  rz/leftmost
   ::rightmost rz/rightmost
   map?        rz/map?
   list?       rz/list?
   seq?        rz/seq?
   set?        rz/set?
   vector?     rz/vector?})

(defn append-child-newline
  [loc _]
  (rz/append-child loc (rz/node (rz/of-string "\n"))))

(defn pred
  "Navigate tree by pred"
  [p]
  (fn find-path-pred [loc]
    (rz/find loc
             rz/next
             (get nav-substitutions p p))))

(defn find-path
  "navigates to a location using the values in a vector"
  [loc path]
  (let [path (map (fn [x] (get nav-substitutions x x)) path)]
    (reduce (fn [loc nav-item]
              (let [new-loc (if (fn? nav-item)
                              ;; functions are rewrite-clj navigates
                              (nav-item loc)
                              ;; non-function values are used to navigate to that value's
                              ;; parent. we use the value's parent because most edits are
                              ;; meant to append to a list, vector, or map, and the value is
                              ;; typically a symbol that's used as a kind of anchor
                              (find-value-parent loc nav-item))]
                (if (nil? new-loc)
                  (reduced {::error nav-item})
                  new-loc)))
            loc
            path)))

(defn edit-at-path
  [{:keys [modify] :as point}]
  (let [{:keys [path edits loc node-to-insert]} modify
        modify-loc (find-path loc path)]
    (if-let [error-nav-item (::error modify-loc)]
      (*error-handler* point (ex-info "could not navigate to loc to modify" {:nav-item error-nav-item}))
      (reduce (fn [loc edit] (edit loc node-to-insert))
              modify-loc
              edits))))

(defn assoc-modify-node-to-insert
  [{:keys [content] :as point}]
  (let [{:keys [template form]} content
        node-to-insert          (if template
                                  (rz/node (rz/of-string template))
                                  form)]
    (assoc-in point [:modify :node-to-insert] node-to-insert)))

(defn modify-node
  "updates a node with rewrite-clj using point"
  [point]
  (-> point
      assoc-modify-node-to-insert
      edit-at-path))

;;------
;; point string rendering
;;------

(defn render-modify-point
  [{:keys [::read] :as point}]
  (-> point
      (assoc-in [:modify :loc] (read point))
      modify-node
      rz/root-string))

(defn render-file-point
  [{:keys [content] :as _point}]
  (let [{:keys [template form]} content]
    (if template template (str form))))

(defn render-point
  [{:keys [modify] :as point}]
  (try
    (*info-handler* point)
    (if modify
      (render-modify-point point)
      (render-file-point point))
    (catch Exception e
      (*error-handler* point e))))

;;------
;; point writers
;;------

(defn write-point-to-file
  [{:keys [contents] :as point}]
  (let [file-path (point-path point)]
    (.mkdirs (java.io.File. (.getParent (java.io.File. file-path))))
    (spit file-path contents)))

(defn write-point
  [{:keys [::write] :as point}]
  (try
    (*info-handler* point)
    (write point)
    (catch Exception e
      (*error-handler* point e))))

;;------
;; generators
;;------

(comment
  {:destination {:path   "{{top|file}}/backend/endpoint_routes.cljc"
                 :anchor 'st:begin-ns-routes}
   :data        {}}

  {:destination {:namespace "{{top|ns}}.backend.endpoint.{{endpoint-name}}"
                 :anchor    'st:begin-ns-routes}
   :content     [:foo :bar]}

  {:destination {:namespace "{{top|ns}}.cross.endpoint-routes"
                 :anchor 'st:begin-ns-routes}
   :content     "..."})

;; substitution rendering
(defn ->ns
  "Given a string or symbol, presumably representing a file path, return a string
  that represents the equivalent namespace."
  [f]
  (-> f (str) (str/replace "/" ".") (str/replace "_" "-")))

(defn ->file
  "Given a string or symbol, presumably representing a namespace, return a string
  that represents the equivalent file system path."
  [n]
  (-> n (str) (str/replace "." "/") (str/replace "-" "_")))

(defn ->subst-map
  "Given a hash map of substitution data, return a hash map of string
  substitutions. For any unqualified keys that have string or symbol values,
  compute a `|ns` version that could be used as a namespace and a `|file`
  version that could be used as a filename. These are done fairly simply as seen
  above."
  [substitutions]
  (reduce-kv (fn [m k v]
               (let [n (namespace k)
                     s (str (some-> n (str "/"))
                            (name k))]
                 (cond-> (assoc m (str "{{" s "}}") (str v))
                   (and (nil? n) (or (string? v) (symbol? v)))
                   (assoc (str "{{" s "|ns}}")   (->ns   v)
                          (str "{{" s "|file}}") (->file v)))))
             {}
             substitutions))

(defn- render-template
  "Given a string and a subst-map hash map, return the string with all
  substitutions performed."
  [template subst-map]
  (reduce (fn [s [expression replacement]]
            (str/replace s expression replacement))
          template
          subst-map))

(defn- render-destination-namespace
  [{:keys [namespace dir extension]}]
  (str (when dir (str dir "/"))
       (->file (name namespace))
       (when extension (str "." extension))))

(defn- render-destination-path
  [{:keys [path dir]}]
  (str (when dir (str dir "/"))
       path))

(defn- render-point-strings
  "performs string substitutions on all string values in :data map"
  [{:keys [data] :as point}]
  (let [subst-map (->subst-map data)]
    (walk/postwalk (fn [x]
                     (if (string? x)
                       (render-template x subst-map)
                       x))
                   point)))

(defn- render-destination-values
  "renders all values in :destination value"
  [{:keys [destination] :as point}]
  (let [{:keys [path namespace]} destination]
    (when (and path namespace)
      (throw (ex-info "You can only specify one of :path or :namespace for a :destination"
                      {:path path, :namespace namespace})))

    (assoc point :destination (cond-> {}
                                path      (assoc :path (render-destination-path destination))
                                namespace (assoc :path (render-destination-namespace destination))))))

(def Content
  [:or
   [:map [:template :string]]
   [:map [:form :any]]])

(def PathDestination
  [:map
   [:path :string]
   [:dir {:optional true} :string]
   [:data {:optional true} :map]])

(def NamespaceDestination
  [:map
   [:namespace [:or :symbol :string :keyword]]
   [:extension :string]
   [:dir {:optional true} :string]
   [:data {:optional true} :string]])

(def Modify
  [:map
   [:path [:vector :any]]
   [:edits [:vector fn?]]])

(def GeneratorPoint
  [:map
   [:id          {:optional false} :any]
   [:description {:optional false} :string]
   [:destination {:optional false} [:or PathDestination NamespaceDestination]]
   [:modify      {:optional true}  Modify]
   [:content     Content]
   [:data        {:optional true} [:map-of :keyword :any]]])

(def Generator
  [:map
   [:points [:vector GeneratorPoint]]
   [:data-schema {:optional true} :any]
   [:data {:optional true} :map]])

(defmulti generator (fn [generator-name _data] generator-name))

(defn point-info-logger
  [point]
  (log/info (select-keys point [:id :description :destination])))

(defn point-error-logger
  [point error]
  (log/error error (select-keys point [::generator-name :destination :id])))

(defn point-error-handler
  [point error]
  (throw (ex-info "Error handling point"
                  (select-keys point [::generator-name :destination :id])
                  error)))

(defn mk-event-handlers
  [{:keys [::opts]}]
  (let [event-handlers (:event-handlers opts)]
    (cond
      (= event-handlers :clojure.tools.logging) {:info  point-info-logger
                                                 :error point-error-logger}
      (nil? event-handlers)                     {:info  (constantly nil)
                                                 :error point-error-handler}
      (and (fn? (:info event-handlers))
           (fn? (:error event-handlers)))       event-handlers
      :else                                     (throw (ex-info "invalid event handlers" event-handlers)))))

(defn set-read-write-opts
  "makes reading and writeable configurable to allow testing"
  [data]
  (merge-with #(or %1 %2)
              data
              {::read  (comp rz/of-file point-path)
               ::write write-point-to-file}))

(defn test-read-fn
  [point-source-map]
  (fn [{:keys [id]}]
    (let [source (get point-source-map id)]
      (if (string? source)
        (rz/of-string source)
        (-> ""
            (rz/of-string)
            (rz/append-child source))))))

(defn test-write
  [{:keys [contents] :as point}]
  {:file-path (point-path point)
   :contents  contents})

(defn test-read-write
  "creates a reader and writer for tests"
  [point-source-map]
  {::read  (test-read-fn point-source-map)
   ::write test-write})

(defn generate
  [generator-name data]
  (let [{points         :points
         generator-data :data
         :as            gen} (generator generator-name data)
        {:keys [info error]} (mk-event-handlers data)]
    (dsu/validate-with-throw Generator gen)
    (binding [*info-handler*  info
              *error-handler* error]
      (mapv (fn [point]
              (let [updated-point (-> point
                                      (merge {::generator-name generator-name}
                                             (set-read-write-opts data))
                                      (update :data merge generator-data data)
                                      render-point-strings
                                      render-destination-values)
                    updated-point (assoc updated-point :contents (render-point updated-point))]
                (write-point updated-point)))
            points))))

(comment
  (generate :donut/endpoint {:endpoint-name 'my.endpoint}))
