(ns donut.generate
  "Write code generators that can be executed from the REPL"
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]
   [donut.sugar.utils :as dsu]
   [rewrite-clj.node.whitespace :as rnw]
   [rewrite-clj.zip :as rz]))

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

(defn insert-at-path
  [loc path actions form]
  (reduce (fn [loc action]
            ((action actions-map) loc form))
          (find-path loc path)
          actions))

(defn modify-node
  "updates a node with rewrite-clj using point"
  [loc {:keys [content modify] :as _point}]
  (let [{:keys [template form]} content
        {:keys [path actions]}  modify
        node-to-insert          (if template
                                  (rz/node (rz/of-string template))
                                  form)]
    (insert-at-path loc path actions node-to-insert)))

;;------
;; point string rendering
;;------

(defn render-modify-point
  [point]
  (let [file-path (point-path point)]
    (-> file-path
        rz/of-file
        (modify-node point)
        rz/root-string)))

(defn render-file-point
  [{:keys [content] :as _point}]
  (let [{:keys [template form]} content]
    (if template template (str form))))

(defn render-point
  [{:keys [modify] :as point}]
  (if modify
    (render-modify-point point)
    (render-file-point point)))

;;------
;; point writers
;;------

(defn write-modify-point
  "handle points that specify a modification"
  [point]
  (let [file-path (point-path point)]
    (spit file-path (render-point point))))

(defn write-file-point
  "handle poitns that specify a whole file"
  [point]
  (let [file-path (point-path point)]
    (.mkdirs (java.io.File. (.getParent (java.io.File. file-path))))
    (spit file-path (render-point point))))

(defn write-point
  [{:keys [modify] :as point}]
  (if modify
    (write-modify-point point)
    (write-file-point point)))

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
   [:namespace :string]
   [:extension :string]
   [:dir {:optional true} :string]
   [:data {:optional true} :string]])

(def ModifyPath
  [:map
   [:path [:vector :any]]
   [:actions [:vector (into [:enum] (keys actions-map))]]])

(def ModifyAnchor
  [:map
   [:anchor :keyword]])

(def GeneratorPoint
  [:map
   [:destination {:optional false} [:or PathDestination NamespaceDestination]]
   [:modify      {:optional true}  [:or ModifyPath ModifyAnchor]]
   [:content     Content]
   [:data        {:optional true} [:map-of :keyword :any]]])

(def Generator
  [:map
   [:points [:vector GeneratorPoint]]
   [:data {:optional true} :map]])

(defmulti generator (fn [generator-name _data] generator-name))

(defn generate
  [generator-name data]
  (let [{points         :points
         generator-data :data
         :as            gen} (generator generator-name data)]
    (dsu/validate-with-throw Generator gen)
    (doseq [point points]
      (write-point (-> point
                       (update :data merge generator-data data)
                       render-point-strings
                       render-destination-values)))))

(comment
  (generate :donut/endpoint {:endpoint-name 'my.endpoint}))
