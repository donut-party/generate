(ns donut.generate.examples
  (:require
   [clojure.java.shell :as sh]
   [donut.generate :as dg]))

(defmethod dg/generator ::add-endpoint
  [_ {:keys [top endpoint-name]}]
  (let [endpoint-ns (symbol (str (dg/->ns top)
                                 ".backend.endpoint."
                                 endpoint-name
                                 "-endpoint"))]
    {:points
     [;; generate the endpoint file
      {:destination {:namespace "{{endpoint-ns}}"
                     :extension "clj"
                     :dir       "test-generated-files"}
       :content     {:template "(ns {{endpoint-ns}})
;; content goes here"}
       }

      ;; add a namespace require
      ;; shows using a content form
      {:destination {:path "{{top|file}}/cross/endpoint_routes.cljc"
                     :dir  "test-generated-files"}
       :modify      {:path    ['ns :require]
                     :actions [:append-child]}
       :content     {:form [endpoint-ns :as endpoint-name]}}

      ;; add a route entry
      {:destination {:path "{{top|file}}/cross/endpoint_routes.cljc"
                     :dir  "test-generated-files"}
       :modify      {:path    ['routes vector?]
                     :actions [:append-child]}
       :content     {:template "[\"{{route-prefix}}/{{endpoint-name}}\"
   {:name     {{endpoint-name-kw}}
    :ent-type {{endpoint-name-kw}}
    :id-key   {{endpoint-name-kw}}/id}
   #?(:clj {{endpoint-name}}/collection-handlers)]"}}]

     :data-schema [:map
                   [:endpoint-name {:optional? false}]]
     :data        {:route-prefix     ""
                   :endpoint-ns      endpoint-ns
                   :endpoint-name-kw (keyword endpoint-name)}}))

(defn try-generate
  "When you run this it generates these files:
  - test-generated-files/generate_test/cross/endpoint_routes.cljc
  - test-generated-files/generate_test/backend/user_endpoints.clj"
  []
  (let [current-directory (System/getProperty "user.dir")
        output-directory  (str current-directory "/test-generated-files")
        source-directory  (str current-directory "/dev-resources/test-generated-files")]
    ;; sets up directories for example purposes; you generally won't have to do
    ;; this in real usage
    (sh/sh "rm" "-rf" output-directory)
    (sh/sh "cp" "-r" source-directory output-directory)
    (dg/generate ::add-endpoint {:endpoint-name 'users
                                 :top           "generate-test"})))
