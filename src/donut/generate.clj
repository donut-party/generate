(ns donut.generate
  "Write code generators that can be executed from the REPL"
  (:require
   [clojure.walk :as walk]
   [clojure.string :as str]
   [rewrite-clj.custom-zipper.core :as rcz]
   [rewrite-clj.node.whitespace :as rnw]
   [rewrite-clj.zip :as rz]
   [rewrite-clj.zip.whitespace :as rzw]))

;;------
;; generator helpers
;;------

(defn point-path
  [point]
  (get-in point [:destination :path]))

;;---
;; using rewrite-clj to modify existing files
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

(def actions-map
  {:append-child   rz/append-child
   :append-newline (fn [loc _] (rz/append-child loc (rnw/newlines 1)))})

(defn find-path
  [loc path]
  (let [path (map (fn [x] (get nav-substitutions x x)) path)]
    (reduce (fn [loc nav-item]
              (if (fn? nav-item)
                (rz/find loc rz/next nav-item)
                (find-value-parent loc nav-item)))
            loc
            path)))

(defn insert-below-anchor
  "Adds a form below an \"anchor\", where an anchor is something like
  #_group:name"
  [loc anchor form]
  ;; need to use rz/up because "anchors" exist in source as forms like
  ;; `#_pref:name`. we're finding the value `pref:name`, which exists in a
  ;; comment node, so we need to navigate up to the comment node
  (if-let [anchor-loc (find-value-parent loc anchor)]
    (let [left-node  (rz/node (rcz/left anchor-loc))
          whitespace (and (:whitespace left-node) left-node)]
      (-> anchor-loc
          (rcz/insert-right form)
          (rz/right)
          (rzw/insert-newline-left)
          (rcz/insert-left whitespace)
          ;; navigate back to anchor
          (rcz/left)
          (rcz/left)
          (rcz/left)))
    (throw (ex-info "Could not find anchor node" {:anchor anchor}))))

(defn insert-at-path
  [loc path actions form]
  (reduce (fn [loc action]
            ((action actions-map) loc form))
          (find-path loc path)
          actions))

(defn write-node
  [loc {:keys [content] :as point}]
  (let [{:keys [template form]}       content
        {:keys [path actions anchor]} (get-in point [:destination :rewrite])
        node-to-insert                (if template
                                        (rz/node (rz/of-string template))
                                        form)]
    (if anchor
      (insert-below-anchor loc anchor node-to-insert)
      (insert-at-path loc path actions node-to-insert))))

;;------
;; point writers
;;------

(defn rewrite-point
  [point]
  (let [file-path (point-path point)]
    (spit file-path (-> file-path
                        rz/of-file
                        (write-node point)
                        rz/root-string))))

(defn write-file-point
  [{:keys [content] :as point}]
  (let [file-path               (point-path point)
        {:keys [template form]} content]
    (.mkdirs (java.io.File. (.getParent (java.io.File. file-path))))
    (spit file-path (if template template (str form)))))

(defn write-point
  [{:keys [destination] :as point}]
  (if (:rewrite destination)
    (rewrite-point point)
    (write-file-point point)))

;;------
;; generators
;;------

;; config
(defmulti generator-config (fn [generator-name _data] generator-name))

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
       (->file namespace)
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
  (let [{:keys [path namespace rewrite]} destination]
    (when (and path namespace)
      (throw (ex-info "You can only specify one of :path or :namespace for a :destination"
                      {:path path, :namespace namespace})))

    (assoc point :destination (cond-> {}
                                path      (assoc :path (render-destination-path destination))
                                namespace (assoc :path (render-destination-namespace destination))
                                rewrite   (assoc :rewrite rewrite)))))

(def GeneratorPoint
  [:map
   [:destination {:optional? false}
    [:map
     [:path :string]
     [:namespace]
     [:extension]
     [:dir {:optional? true} :string]
     [:data :map]]]
   [:content {:optional? false}]
   [:data {:optional? false} :string]])

(defn generate
  [generator-name data]
  (let [{points         :points
         generator-data :data} (generator-config generator-name data)]
    (doseq [point points]
      (write-point (-> point
                       (update :data merge generator-data data)
                       render-point-strings
                       render-destination-values)))))

(comment
  (generate :donut/endpoint {:endpoint-name 'my.endpoint}))
