(ns donut.generate
  "Write code generators that can be executed from the REPL"
  (:require
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [clojure.walk :as walk]
   [donut.sugar.utils :as dsu]
   [rewrite-clj.node :as rn]
   [rewrite-clj.zip :as rz]))

;;------
;; generator helpers
;;------

(defn point-path
  [point]
  (get-in point [:destination :path]))

;;---
;; using rewrite-clj to modify source
;;---

(defn find-parent
  [value]
  (fn [loc]
    (rz/up (rz/find-value loc rz/next value))))

(defn find-value
  [value]
  (fn [loc]
    (rz/find-value loc rz/next value)))

(defn nav-item->nav-fn
  [nav-item]
  (find-value nav-item))

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
  ([loc]
   (rz/append-child loc (rn/newlines 1)))
  ([loc _]
   (append-child-newline loc)))

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
              (let [nav-item (if (fn? nav-item)
                               ;; functions are rewrite-clj navigates
                               nav-item
                               ;; non-function values are used to navigate to that value's
                               ;; parent. we use the value's parent because most edits are
                               ;; meant to append to a list, vector, or map, and the value is
                               ;; typically a symbol that's used as a kind of anchor
                               (nav-item->nav-fn nav-item))
                    new-loc  (nav-item loc)]
                (if (nil? new-loc)
                  (reduced {::error nav-item})
                  new-loc)))
            loc
            path)))

(defn edit-at-path
  [{:keys [modify]} {:keys [handle-error] :as ctx}]
  (let [{:keys [path edits loc node-to-insert]} modify
        modify-loc (find-path loc path)]
    (if-let [error-nav-item (::error modify-loc)]
      (handle-error (assoc ctx
                           :event-id :edit-at-path
                           :error (ex-info "could not navigate to loc to modify" {:nav-item error-nav-item})))
      (reduce (fn [loc edit] (edit loc node-to-insert))
              modify-loc
              edits))))

(defn node-merge
  "merges in all nodes from a map into loc"
  [loc map-node]
  (let [mloc (rz/of-node map-node)
        initial-loc (if (empty? (rz/sexpr loc)) loc (append-child-newline loc))]
    (reduce rz/append-child
            initial-loc
            (rn/children map-node))))

(defn assoc-modify-node-to-insert
  [{:keys [content] :as point}]
  (let [{:keys [template form]} content
        node-to-insert          (if template
                                  (rz/node (rz/of-string template))
                                  (rn/coerce form))]
    (assoc-in point [:modify :node-to-insert] node-to-insert)))

(defn modify-node
  "updates a node with rewrite-clj using point"
  [point ctx]
  (-> point
      assoc-modify-node-to-insert
      (edit-at-path ctx)))

;;------
;; point string rendering
;;------

(defn render-modify-point
  [point {:keys [read-point] :as ctx}]
  (-> point
      (assoc-in [:modify :loc] (read-point point))
      (modify-node ctx)
      rz/root-string))

(defn render-file-point
  [{:keys [content] :as _point} _ctx]
  (let [{:keys [template form]} content]
    (if template template (str form))))

(defn render-point
  [{:keys [modify] :as point} {:keys [handle-info handle-error] :as ctx}]
  (let [updated-ctx (assoc ctx :event-id :render-point)]
    (try
      (handle-info updated-ctx)
      (if modify
        (render-modify-point point ctx)
        (render-file-point point ctx))
      (catch Exception e
        (handle-error (assoc updated-ctx :error e))))))

;;------
;; point writers
;;------

(defn write-point
  [point {:keys [handle-info handle-error] :as ctx}]
  (let [write-ctx (assoc ctx :event-id :write-point)]
    (try
      (handle-info write-ctx)
      ((:write-point ctx) point)
      (catch Exception e
        (handle-error (assoc write-ctx :error e))))))

;;------
;; generators
;;------

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

;;---
;; point destination
;;---
;; useful for loggers

(def extension-regex #"\.\w+$")

(defn- strip-extension
  [s]
  (str/replace s extension-regex ""))

(defn rendered-point-ns
  [point]
  (-> point :destination :path strip-extension ->ns))

(defn rendered-point-file-path
  [point]
  (let [path      (-> point :destination :path)
        extension (re-find extension-regex path)]
    (str (-> path strip-extension ->file)
         extension)))

;;---
;; schemas
;;---

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

;;---
;; interface configuration
;;---

;; file read/write
(defn read-point-file
  [point]
  (-> point point-path rz/of-file))

(defn write-point-file
  [{:keys [contents] :as point}]
  (let [file-path (point-path point)]
    (when-let [parent (.getParent (java.io.File. file-path))]
      (.mkdirs (java.io.File. parent)))
    (spit file-path contents)))

;; test read/write
(defn read-point-test-fn
  "produces a point reading function using point-source-map, where
  point-source-map maps point ids to the template that should be used"
  [point-source-map]
  (fn [{:keys [id]}]
    (let [source (get point-source-map id)]
      (if (string? source)
        (rz/of-string source)
        (-> ""
            (rz/of-string)
            (rz/append-child source))))))

(defn write-point-test
  [{:keys [contents] :as point}]
  {:file-path (point-path point)
   :contents  contents})

(defn handle-error-rethrow
  [{:keys [error]}]
  (throw error))

(defn log-opts
  [{:keys [point] :as opts}]
  (-> opts
      (select-keys [:event-id :generator-name])
      (assoc :point-id (:id point))))

(defn handle-info-log
  [opts]
  (log/info (log-opts opts)))

(defn handle-error-log
  [{:keys [error] :as opts}]
  (log/error error (log-opts opts)))


(defn init-ctx
  [generator-name data {:keys [handle-info handle-error read-point write-point]}]
  {:generator-name generator-name
   :data           data
   :handle-info    (or handle-info (constantly nil))
   :handle-error   (or handle-error handle-error-rethrow)
   :read-point     (or read-point read-point-file)
   :write-point    (or write-point write-point-file)})

;;---
;; interface
;;---

(defmulti generator (fn [generator-name _data] generator-name))

(defn generate
  ([generator-name data]
   (generate generator-name data {}))
  ([generator-name data opts]
   (let [{points         :points
          generator-data :data
          :as            gen} (generator generator-name data)
         ctx                  (init-ctx generator-name data opts)]
     (dsu/validate-with-throw Generator gen)
     (mapv (fn [point]
             (let [updated-point (-> point
                                     (update :data merge generator-data data)
                                     render-point-strings
                                     render-destination-values)
                   ctx-w-point   (assoc ctx :point updated-point)
                   updated-point (assoc updated-point :contents (render-point updated-point ctx-w-point))]
               (write-point updated-point ctx-w-point)))
           points))))

(comment
  (generate :donut/endpoint {:endpoint-name 'my.endpoint}))
