(ns donut.generate-test
  (:require
   [clojure.java.shell :as sh]
   [clojure.test :refer [deftest is testing]]
   [donut.generate :as dg]
   [rewrite-clj.zip :as rz]))

;;---
;; ->ns tests
;;---

(deftest ->ns-test
  (testing "converts file path separators to dots"
    (is (= "foo.bar.baz" (dg/->ns "foo/bar/baz"))))
  (testing "converts underscores to hyphens"
    (is (= "my-ns" (dg/->ns "my_ns"))))
  (testing "handles combined path and underscores"
    (is (= "my-app.some-ns.core" (dg/->ns "my_app/some_ns/core"))))
  (testing "accepts symbols"
    (is (= "foo.bar" (dg/->ns 'foo/bar))))
  (testing "no-op for already valid namespace"
    (is (= "foo.bar" (dg/->ns "foo.bar")))))

;;---
;; ->file tests
;;---

(deftest ->file-test
  (testing "converts dots to path separators"
    (is (= "foo/bar/baz" (dg/->file "foo.bar.baz"))))
  (testing "converts hyphens to underscores"
    (is (= "my_ns" (dg/->file "my-ns"))))
  (testing "handles combined namespace and hyphens"
    (is (= "my_app/some_ns/core" (dg/->file "my-app.some-ns.core"))))
  (testing "accepts symbols"
    (is (= "foo/bar" (dg/->file 'foo.bar))))
  (testing "->ns and ->file are inverses"
    (let [path "my_app/some_ns/core"]
      (is (= path (dg/->file (dg/->ns path)))))))

;;---
;; ->subst-map tests
;;---

(deftest ->subst-map-test
  (testing "creates basic substitution entry"
    (let [result (dg/->subst-map {:top "myapp"})]
      (is (= "myapp" (get result "{{top}}")))))

  (testing "creates |ns and |file variants for string values"
    (let [result (dg/->subst-map {:top "my_app/core"})]
      (is (= "my-app.core" (get result "{{top|ns}}")))
      (is (= "my_app/core" (get result "{{top|file}}")))))

  (testing "creates |ns and |file variants for symbol values"
    (let [result (dg/->subst-map {:top 'my_app})]
      (is (= "my-app" (get result "{{top|ns}}")))
      (is (= "my_app" (get result "{{top|file}}")))))

  (testing "does not add |ns and |file variants for non-string/symbol values"
    (let [result (dg/->subst-map {:count 42})]
      (is (contains? result "{{count}}"))
      (is (not (contains? result "{{count|ns}}")))
      (is (not (contains? result "{{count|file}}")))))

  (testing "handles namespaced keys"
    (let [result (dg/->subst-map {:my/key "val"})]
      (is (= "val" (get result "{{my/key}}"))))))

;;---
;; render-template (via ->subst-map + render indirectly) tests
;;---

;; render-template is private, so we test it via render-point-strings behavior

(deftest render-point-strings-test
  (testing "substitutes template variables in string values"
    (let [point {:data        {:top "my-app"}
                 :destination {:path "{{top}}/routes.clj"}
                 :content     {:template "ns {{top|ns}}"}}
          result (#'dg/render-point-strings point)]
      (is (= "my-app/routes.clj" (get-in result [:destination :path])))
      (is (= "ns my-app" (get-in result [:content :template])))))

  (testing "substitutes multiple variables"
    (let [point  {:data        {:top "myapp" :endpoint-name "users"}
                  :destination {:path "{{top}}/{{endpoint-name}}.clj"}}
          result (#'dg/render-point-strings point)]
      (is (= "myapp/users.clj" (get-in result [:destination :path])))))

  (testing "leaves non-string values untouched"
    (let [point  {:data {:top "myapp"} :some-num 42}
          result (#'dg/render-point-strings point)]
      (is (= 42 (:some-num result))))))

;;---
;; render-destination-values tests
;;---

(deftest render-destination-values-test
  (testing "uses :path directly"
    (let [point  {:destination {:path "src/myapp/core.clj"}}
          result (#'dg/render-destination-values point)]
      (is (= "src/myapp/core.clj" (get-in result [:destination :path])))))

  (testing "converts :namespace to :path"
    (let [point  {:destination {:namespace "myapp.core" :extension "clj"}}
          result (#'dg/render-destination-values point)]
      (is (= "myapp/core.clj" (get-in result [:destination :path])))))

  (testing "prepends :dir to :path"
    (let [point  {:destination {:path "core.clj" :dir "src"}}
          result (#'dg/render-destination-values point)]
      (is (= "src/core.clj" (get-in result [:destination :path])))))

  (testing "prepends :dir to :namespace path"
    (let [point  {:destination {:namespace "myapp.core" :extension "clj" :dir "src"}}
          result (#'dg/render-destination-values point)]
      (is (= "src/myapp/core.clj" (get-in result [:destination :path])))))

  (testing "throws when both :path and :namespace specified"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"only specify one"
         (#'dg/render-destination-values
          {:destination {:path "a.clj" :namespace "a.b"}})))))

;;---
;; point-path tests
;;---

(deftest point-path-test
  (is (= "src/core.clj"
         (dg/point-path {:destination {:path "src/core.clj"}})))
  (is (nil? (dg/point-path {:destination {}}))))

;;---
;; rendering tests
;;---

(deftest render-test
  (is (= {:destination {:path "src/my/project/cross/endpoint_routes.cljc"}
          :data        {:top 'my.project}}
         (-> {:destination {:path "{{top|file}}/cross/endpoint_routes.cljc"
                            :dir  "src"}
              :data        {:top 'my.project}}
             (#'dg/render-point-strings)
             (#'dg/render-destination-values))

         (-> {:destination {:namespace "{{top|ns}}.cross.endpoint-routes"
                            :extension "cljc"
                            :dir       "src"}
              :data        {:top 'my.project}}
             (#'dg/render-point-strings)
             (#'dg/render-destination-values)))))

;;---
;; find-path
;;---

(deftest rewrite-find-path-test
  (is (= '(:require)
         (-> (rz/of-string "(ns foo (:require))")
             (dg/find-path '[ns :require])
             (rz/sexpr))))

  (is (= []
         (-> (rz/of-string "(ns foo (:require []))")
             (dg/find-path ['ns :require vector?])
             (rz/sexpr)))))

;;---
;; find-value-parent
;;---

(deftest find-value-parent-test
  (let [root-node (rz/of-string "(def routes #_anchor [])")]
    (is (= '(def routes [])
           (-> root-node
               (dg/find-value-parent 'routes)
               (rz/sexpr))
           (-> root-node
               (dg/find-value-parent 'def)
               (rz/sexpr))))))


;;---
;; modify-node
;;---


(deftest modify-node-test
  (testing "works with path and actions"
    (is (= '(def routes [:foo])
           (-> (rz/of-string "(def routes [])")
               (dg/modify-node {:content {:form :foo}
                                :modify  {:path ['routes vector?]
                                          :actions [:append-child]}})
               (rz/root)
               (rz/of-node)
               (rz/sexpr))))

    (is (= "(def routes 
  [:foo 
:bar])"
           (-> (rz/of-string "(def routes 
  [:foo])")
               (dg/modify-node {:content {:form :bar}
                                :modify  {:path ['routes vector?]
                                          :actions [:append-newline :append-child]}})
               (rz/root-string))))))

;;--- 
;; testing an actual generator
;;--- 

(defmethod dg/generator :donut/endpoint
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
      {:destination {:path "{{top|file}}/cross/endpoint_routes.cljc"
                     :dir  "test-generated-files"}
       :modify      {:path    ['ns :require]
                     :actions [:append-child]}
       :content     {:form [endpoint-ns :as endpoint-name]}}

      ;; update the routes
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
     :data        {:endpoint-ns      endpoint-ns
                   :endpoint-name-kw (keyword endpoint-name)}}))

(deftest generator-test
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
