(ns donut.generate
  "Write code generators that can be executed from the REPL"
  (:require
   [clojure.walk :as walk]
   [clojure.string :as str]
   [rewrite-clj.custom-zipper.core :as rcz]
   [rewrite-clj.custom-zipper.utils :as rcu]
   [rewrite-clj.zip :as rz]
   [rewrite-clj.zip.whitespace :as rzw]))

;;------
;; generator helpers
;;------

(defn point-path
  [point]
  (get-in point [:destination :path]))

;; rewriting

(defn find-anchor
  [loc anchor]
  (rz/up (rz/find-value loc rz/next anchor)))


(def navs
  {:left      rz/left
   :right     rz/right
   :up        rz/up
   :down      rz/down
   :prev      rz/prev
   :next      rz/next
   :leftmost  rz/leftmost
   :rightmost rz/rightmost})

(def actions
  {:append-child rz/append-child})

(defn find-nav
  [loc nav]
  (reduce (fn [loc nav-item]
            (if (and (keyword? nav-item)
                     (= "donut.generate.nav" (namespace nav-item)))
              ((get navs (keyword (name nav-item))) loc)
              (rz/find loc rz/next (if-not (fn? nav-item)
                                     #(= (rz/sexpr %) nav-item)
                                     nav-item))))
          loc
          nav))

(defn insert-below-anchor
  [loc anchor form]
  ;; need to use rz/up because "anchors" exist in source as forms like
  ;; `#_pref:name`. we're finding the value `pref:name`, which exists in a
  ;; comment node, so we need to navigate up to the comment node
  (if-let [anchor-loc (find-anchor loc anchor)]
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
  [loc path action form]
  ((action actions) (find-nav loc path) form))

(defn write-node
  [loc {:keys [content] :as point}]
  (let [{:keys [template form]}      content
        {:keys [path action anchor]} (get-in point [:destination :rewrite])
        node-to-insert               (if template
                                       (rz/node (rz/of-string template))
                                       form)]
    (if anchor
      (insert-below-anchor loc anchor node-to-insert)
      (insert-at-path loc path action node-to-insert))))

;;------
;; point writers
;;------


(defn write-anchor-point
  [point]
  (prn "write anchor point")
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
    (write-anchor-point point)
    (write-file-point point)))

;;------
;; generators
;;------

;; specs
(defmulti generator-points (fn [generator-name _data] generator-name))


{:destination {:path   "{{top/file}}/backend/endpoint_routes.cljc"
               :anchor 'st:begin-ns-routes}
 :data        {}}

{:destination {:namespace "{{top/ns}}.backend.endpoint.{{endpoint-name}}"
               :anchor    'st:begin-ns-routes}
 :content     [:foo :bar]}

{:destination {:namespace "{{top/ns}}.cross.endpoint-routes"
               :anchor 'st:begin-ns-routes}
 :content     "..."}

;;---
;; NEW STUFF
;;---

(defn ->ns
  "Given a string or symbol, presumably representing a
  file path, return a string that represents the
  equivalent namespace."
  [f]
  (-> f (str) (str/replace "/" ".") (str/replace "_" "-")))

(defn ->file
  "Given a string or symbol, presumably representing a
  namespace, return a string that represents the
  equivalent file system path."
  [n]
  (-> n (str) (str/replace "." "/") (str/replace "-" "_")))

(defn ->subst-map
  "Given a hash map of substitution data, return a hash map of
  string substitutions, suitable for `tools.build.api/copy-dir`.
  For any unqualified keys that have string or symbol values,
  compute a `/ns` version that could be used as a namespace and
  a `/file` version that could be used as a filename. These are
  done fairly simply as seen above."
  [data]
  (reduce-kv (fn [m k v]
               (let [n (namespace k)
                     s (str (when n (str n "/")) (name k))]
                 (cond-> (assoc m (str "{{" s "}}") (str v))
                   (and (nil? n) (or (string? v) (symbol? v)))
                   (assoc (str "{{" s "/ns}}")   (->ns   v)
                          (str "{{" s "/file}}") (->file v)))))
             {}
             data))

(defn- substitute
  "Given a string and a substitution hash map, return the
  string with all substitutions performed."
  [s data]
  (reduce (fn [s [from to]] (str/replace s from to)) s data))

(defn- parse-destination-namespace
  [{:keys [namespace dir extension]}]
  (str (when dir (str dir "/"))
       (->file namespace)
       (when extension (str "." extension))))

(defn- parse-destination-path
  [{:keys [path dir]}]
  (str (when dir (str dir "/"))
       path))

(defn- substitute-all
  [{:keys [data] :as m}]
  (let [subst-map (->subst-map data)]
    (walk/postwalk #(if (string? %) (substitute % subst-map) %)
                   m)))

(defn- parse-destination
  [{:keys [destination] :as spec}]
  (let [{:keys [path namespace rewrite]} destination]
    (when (and path namespace)
      (throw (ex-info "You can only specify one of :path or :namespace for a :destination"
                      {:path path, :namespace namespace})))

    (assoc spec :destination (cond-> {}
                               path      (assoc :path (parse-destination-path destination))
                               namespace (assoc :path (parse-destination-namespace destination))
                               rewrite   (assoc :rewrite rewrite)))))

(def GeneratorPoint
  [:map
   [:destination {:optional? false}
    [:map
     [:path string?]
     [:namespace]
     [:extension]
     [:dir {:optional? true} string?]]]
   [:content {:optional? false}]
   [:data {:optional? false} map?]])

(defn generate
  [generator-name data]
  (let [{points         :points
         generator-data :data} (generator-points generator-name data)]
    (doseq [point points]
      (write-point (-> point
                       (update :data merge generator-data data)
                       substitute-all
                       parse-destination)))))

(comment
  (generate :donut/endpoint {:endpoint-name 'my.endpoint}))
