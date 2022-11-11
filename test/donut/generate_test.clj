(ns donut.generate-test
  (:require
   [clojure.java.shell :as sh]
   [clojure.test :refer [deftest is]]
   [donut.generate :as dg]
   [rewrite-clj.zip :as rz]))

(deftest destination-parser-test
  (is (= {:destination {:path "src/my/project/cross/endpoint_routes.cljc"}
          :data        {:top 'my.project}}
         (-> {:destination {:path "{{top/file}}/cross/endpoint_routes.cljc"
                            :dir  "src"}
              :data        {:top 'my.project}}
             (#'dg/substitute-all)
             (#'dg/parse-destination))

         (-> {:destination {:namespace "{{top/ns}}.cross.endpoint-routes"
                            :extension "cljc"
                            :dir       "src"}
              :data        {:top 'my.project}}
             (#'dg/substitute-all)
             (#'dg/parse-destination)))))


(deftest rewrite-find-path-test
  (is (= '(:require)
         (-> (rz/of-string "(ns foo (:require))")
             (dg/find-path '[ns :require])
             (rz/sexpr))))

  (is (= []
         (-> (rz/of-string "(ns foo (:require []))")
             (dg/find-path ['ns :require vector?])
             (rz/sexpr)))))
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
       :content     {:template "(ns {{endpoint-ns}})
;; content goes here"}
       }

      ;; update the routes namespaces
      {:destination {:path    "{{top/file}}/cross/endpoint_routes.cljc"
                     :dir     "test-generated-files"
                     :rewrite {:path    ['ns :require]
                               :actions [:append-child]}}
       :content     {:form [endpoint-ns :as endpoint-name]}}

      ;; update the routes
      {:destination {:path    "{{top/file}}/cross/endpoint_routes.cljc"
                     :dir     "test-generated-files"
                     :rewrite {:path    ['routes vector?]
                               :actions [:append-child]}}
       :content     {:template "[\"{{route-prefix}}/{{endpoint-name}}\"
   {:name     {{endpoint-name-kw}}
    :ent-type {{endpoint-name-kw}}
    :id-key   {{endpoint-name-kw}}/id}
   #?(:clj {{endpoint-name}}/collection-handlers)]"}}]

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
    (dg/generate :donut/endpoint {:endpoint-name 'users
                                  :top           "generate-test"})

    (is (= "(ns generate-test.cross.endpoint-routes
  (:require [generate-test.backend.endpoint.users-endpoint :as users]))

(def routes
  [[\"{{route-prefix}}/users\"
   {:name     :users
    :ent-type :users
    :id-key   :users/id}
   #?(:clj users/collection-handlers)]])
"
           (slurp (str output-directory "/generate_test/cross/endpoint_routes.cljc"))))
    (is (= "(ns generate-test.backend.endpoint.users-endpoint)
;; content goes here"
           (slurp (str output-directory "/generate_test/backend/endpoint/users_endpoint.clj"))))))
