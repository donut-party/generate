(ns donut.generate-test
  (:require
   [clojure.test :refer [deftest is]]
   [donut.generate :as dg]))

(deftest destination-parser
  (is (= {:destination {:path "src/my/project/cross/endpoint_routes.cljc"}
          :data        {:top 'my.project}}
         (#'dg/parse-destination
          {:destination {:path       "{{top/file}}/cross/endpoint_routes.cljc"
                         :target-dir "src"}
           :data        {:top 'my.project}})

         (#'dg/parse-destination
          {:destination {:namespace  "{{top/ns}}.cross.endpoint-routes"
                         :extension  "cljc"
                         :target-dir "src"}
           :data        {:top 'my.project}}))))
