(ns donut.generate-test
  (:require
   [clojure.java.shell :as sh]
   [clojure.test :refer [deftest is]]
   [donut.generate :as dg]))

(deftest destination-parser
  (is (= {:destination {:path "src/my/project/cross/endpoint_routes.cljc"}
          :data        {:top 'my.project}}
         (#'dg/parse-destination
          {:destination {:path "{{top/file}}/cross/endpoint_routes.cljc"
                         :dir  "src"}
           :data        {:top 'my.project}})

         (#'dg/parse-destination
          {:destination {:namespace "{{top/ns}}.cross.endpoint-routes"
                         :extension "cljc"
                         :dir       "src"}
           :data        {:top 'my.project}}))))


;; testing an actual generator

(defmethod dg/generator-points :donut/endpoint
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
       :content     "(ns {{endpoint-ns}})
;; content goes here"
       }

      ;; update the routes namespaces
      {:destination {:namespace "{{top/file}}/cross/endpoint_routes.cljc"
                     :dir       "test-generated-files"
                     :anchor    'donut:endpoint-namespaces}
       :content     [endpoint-ns :as endpoint-name]}

      ;; update the routes
      {:destination {:namespace "{{top/file}}/cross/endpoint_routes.cljc"
                     :dir       "test-generated-files"
                     :anchor    'donut:routes}
       :content     "[\"{{route-prefix}}/{{endpoint-name}}\"
{:name     {{endpoint-name-kw}}
 :ent-type {{endpoint-name-kw}}
 :id-key   {{endpoint-name-kw}}/id}
#?(:clj {{endpoint-name}}/collection-handlers)]"}]

     :data-schema [:map
                   [:endpoint-name {:optional? false}]]
     :data        {:endpoint-ns      endpoint-ns
                   :endpoint-name-kw (keyword endpoint-name)}}))

(deftest test-generator
  (let [current-directory (System/getProperty "user.dir")
        output-directory  (str current-directory "/test-generated-files")
        source-directory  (str current-directory "/resources/test-generated-files")]
    (sh/sh "rm" "-rf" output-directory)
    (sh/sh "cp" "-r" source-directory output-directory)
    (dg/generate :donut/endpoint {:endpoint-name 'users})

    (is (= ""
           (slurp (str output-directory "/generate_test/cross/endpoint_routes.cljc"))))
    (is (= ""
           (slurp (str output-directory "/generate_test/backend/endpoint/users.clj"))))))
