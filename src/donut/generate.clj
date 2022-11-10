(ns donut.generate
  "Write code generators that can be executed from the REPL"
  (:require
   [cljstache.core :as cs]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [rewrite-clj.custom-zipper.core :as rcz]
   [rewrite-clj.custom-zipper.utils :as rcu]
   [rewrite-clj.zip :as rz]
   [rewrite-clj.zip.whitespace :as rzw]))

;;------
;; generator helpers
;;------

;; paths TODO this isn't that great
(defn point-path-segments
  [{:keys [path]} {:keys [path-base] :as opts
                   :or {path-base []}}]
  (into path-base (if (fn? path)
                    (path opts)
                    path)))

(defn point-path
  [point opts]
  (str/join java.io.File/separator (point-path-segments point opts)))

;; rewriting

(defn find-anchor
  [loc anchor]
  (rz/up (rz/find-value loc rz/next anchor)))

(defn insert-below-anchor
  [loc anchor form]
  ;; need to use rz/up because "anchors" exist in source as forms like
  ;; `#_pref:name`. we're finding the value `pref:name`, which exists in a
  ;; comment node, so we need to navigate up to the comment node
  (let [anchor-loc (find-anchor loc anchor)
        left-node  (rz/node (rcz/left anchor-loc))
        whitespace (and (:whitespace left-node) left-node)]
    (-> anchor-loc
        (rcz/insert-right form)
        (rz/right)
        (rzw/insert-newline-left)
        (rcz/insert-left whitespace)
        ;; navigate back to anchor
        (rcz/left)
        (rcz/left)
        (rcz/left))))

(defn insert-forms-below-anchor
  [loc anchor forms]
  (reduce (fn [node form]
            (insert-below-anchor node anchor form))
          loc
          (reverse forms)))

(defn clear-right [loc]
  (rcu/remove-right-while loc (constantly true)))

(defn clear-right-anchor
  [loc anchor]
  (clear-right (find-anchor loc anchor)))

;;------
;; point generators
;;------

;; specs

(s/def ::path (s/or :path-segments (s/coll-of string?)
                    :path-fn fn?))
(s/def ::strategy keyword?)
(s/def ::rewrite fn?)
(s/def ::template string?)

(defmulti generate-point-type :strategy)

(defmethod generate-point-type ::rewrite-file [_]
  (s/keys :req-un [::path ::rewrite ::strategy]))

(defmethod generate-point-type ::create-file [_]
  (s/keys :req-un [::path ::template ::strategy]))

(s/def ::point (s/multi-spec generate-point-type :strategy))
(s/def ::points (s/map-of keyword? ::point))

;; methods

(defmulti generate-point (fn [{:keys [strategy]} _opts] strategy))

(defmethod generate-point ::rewrite-file
  [{:keys [rewrite] :as point} opts]
  (let [file-path (point-path point opts)]
    (spit file-path (rz/root-string (rewrite (rz/of-file file-path) opts)))))

(defmethod generate-point ::create-file
  [{:keys [template] :as point} opts]
  (let [file-path (point-path point opts)]
    (.mkdirs (java.io.File. (str/join "/" (butlast (point-path-segments point opts)))))
    (spit file-path (cs/render template opts))))

;;------
;; generators
;;------

;; specs

(s/def ::generator-name keyword?)
(s/def ::generator-point-names (s/coll-of keyword?))
(s/def ::generator-pair (s/tuple ::generator-name ::generator-point-names))
(s/def ::opts fn?)
(s/def ::generator (s/keys :req-un [::points]
                           :opt-un [::opts]))

(s/def ::generator*-arg (s/or :generator-name ::generator-name
                              :generator-pair ::generator-pair
                              :generator      ::generator))

;; methods / fns

(defmulti generator identity)

(defn generator*
  [pkg]
  (let [conformed (s/conform ::generator*-arg pkg)]
    (when (= :clojure.spec.alpha/invalid conformed)
      (throw (ex-info "Invalid generator" {:generator pkg
                                           :spec    (s/explain-data ::generator*-arg pkg)})))
    (let [[ptype] conformed]
      (case ptype
        :generator-name (generator pkg)
        :generator-pair (update (generator (first pkg)) select-keys (second pkg))
        :generator      pkg))))

;;------
;; generate
;;------
(defn generate
  [generator & args]
  (let [{:keys [opts points]} (generator* generator)
        opts                  ((or opts identity) args)]
    (doseq [point (vals points)]
      (generate-point point opts))))


(defn generate
  [generator-name generator-data])

{:destination {:path   "{{top/file}}/backend/endpoint_routes.cljc"
               :anchor 'st:begin-ns-routes}
 :data        {}}

{:destination {:namespace "{{top/ns}}.backend.endpoint.{{endpoint-name}}"
               :anchor    'st:begin-ns-routes}
 :content     {:form [:foo :bar]}}

{:destination {:namespace "{{top/ns}}.cross.endpoint-routes"
               :anchor 'st:begin-ns-routes}
 :content     {:template "..."}}

;;---
;; NEW STUFF
;;---

(defn- ->ns
  "Given a string or symbol, presumably representing a
  file path, return a string that represents the
  equivalent namespace."
  [f]
  (-> f (str) (str/replace "/" ".") (str/replace "_" "-")))

(defn- ->file
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

(defn parse-destination-namespace
  [{:keys [namespace target-dir extension]} subst-map]
  (substitute (str (when target-dir (str target-dir "/"))
                   (->file (substitute namespace subst-map))
                   (when extension (str "." extension)))
              subst-map))

(defn parse-destination-path
  [{:keys [path target-dir]} subst-map]
  (substitute (cond->> path
                target-dir (str target-dir "/"))
              subst-map))

(defn parse-destination
  [{:keys [destination data] :as spec}]
  (let [{:keys [path namespace anchor]} destination
        subst-map                       (->subst-map data)]
    (when (and path namespace)
      (throw (ex-info "You can only specify one of :path or :namespace for a :destination"
                      {:path path, :namespace namespace})))

    (assoc spec :destination (cond-> {}
                               path      (assoc :path (parse-destination-path destination subst-map))
                               namespace (assoc :path (parse-destination-namespace destination subst-map))
                               anchor    (assoc :anchor anchor)))))

(generate :donut/endpoint {:endpoint-name 'my.endpoint})
