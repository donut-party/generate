(ns donut.generate-test
  (:require
   [clojure.java.shell :as sh]
   [clojure.test :refer [deftest is testing]]
   [donut.generate :as dg]
   [rewrite-clj.node :as rn]
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
  (is (= ':require
         (-> (rz/of-string "(ns foo (:require))")
             (dg/find-path '[ns :require])
             (rz/sexpr))))

  (is (= []
         (-> (rz/of-string "(ns foo (:require []))")
             (dg/find-path ['ns :require (dg/pred vector?)])
             (rz/sexpr)))))

;;---
;; find-value-parent
;;---

(deftest find-value-parent-test
  (let [root-node (rz/of-string "(def routes #_anchor [])")]
    (is (= '(def routes [])
           (-> root-node
               ((dg/find-parent 'routes))
               (rz/sexpr))
           (-> root-node
               ((dg/find-parent 'def))
               (rz/sexpr))))))


;;---
;; modify-node
;;---

(deftest modify-node-test
  (testing "works with path and edits"
    (is (= '(def routes [:foo])
           (-> (dg/modify-node {:content {:form :foo}
                                :modify  {:path  ['routes (dg/pred vector?)]
                                          :edits [rz/append-child]
                                          :loc   (rz/of-string "(def routes [])")}}
                               {})
               (rz/root)
               (rz/of-node)
               (rz/sexpr))))

    (is (= "(def routes 
  [:foo 
:bar])"
           (-> (dg/modify-node {:content {:form :bar}
                                :modify  {:path  ['routes (dg/pred vector?)]
                                          :edits [dg/append-child-newline rz/append-child]
                                          :loc   (rz/of-string "(def routes 
  [:foo])")}}
                               {})
               (rz/root-string))))))


;; TODO needs to work with template and with form
(deftest node-merge-test
  (testing "works with template"
    (is (= "(def kvs {:foo  {:x :y} 
:bar  {:a :b}})"
           (-> (dg/modify-node {:content {:template "{:foo {:x :y}
:bar {:a :b}}"}
                                :modify  {:path  ['kvs (dg/pred map?)]
                                          :edits [dg/node-merge]
                                          :loc   (rz/of-string "(def kvs {})")}}
                               {})
               (rz/root)
               (rz/of-node)
               (rz/string))))


    (is (= "(def kvs {:a :b 
:foo  {:x :y} 
:bar  {:a :b}})"
           (-> (dg/modify-node {:content {:template "{:foo {:x :y}
:bar {:a :b}}"}
                                :modify  {:path  ['kvs (dg/pred map?)]
                                          :edits [dg/node-merge]
                                          :loc   (rz/of-string "(def kvs {:a :b})")}}
                               {})
               (rz/root)
               (rz/of-node)
               (rz/string)))))

  (testing "works with form"
    (is (= "(def kvs {:foo  {:x :y} , :bar  {:a :b}})"
           (-> (dg/modify-node {:content {:form {:foo {:x :y}
                                                 :bar {:a :b}}}
                                :modify  {:path  ['kvs (dg/pred map?)]
                                          :edits [dg/node-merge]
                                          :loc   (rz/of-string "(def kvs {})")}}
                               {})
               (rz/root)
               (rz/of-node)
               (rz/string))))

    (is (= "(def kvs {:a :b 
:foo  {:x :y} , :bar  {:a :b}})"
           (-> (dg/modify-node {:content {:form {:foo {:x :y}
                                                 :bar {:a :b}}}
                                :modify  {:path  ['kvs (dg/pred map?)]
                                          :edits [dg/node-merge]
                                          :loc   (rz/of-string "(def kvs {:a :b})")}}
                               {})
               (rz/root)
               (rz/of-node)
               (rz/string))))))

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
     [ ;; generate the endpoint file
      {:id          ::endpoint-file
       :description "writes endpoint file"
       :destination {:namespace "{{endpoint-ns}}"
                     :extension "clj"
                     :dir       "test-generated-files"}
       :content     {:template "(ns {{endpoint-ns}})
;; content goes here"}
       }

      ;; update the routes namespaces
      {:id          ::add-route-ns-require
       :description "adds a ns alias to :require"
       :destination {:path "{{top|file}}/cross/endpoint_routes.cljc"
                     :dir  "test-generated-files"}
       :modify      {:path  ['ns (dg/find-parent :require)]
                     :edits [rz/append-child]}
       :content     {:form [endpoint-ns :as endpoint-name]}}

      ;; update the routes
      {:id          ::add-route
       :description "adds route definition to routes"
       :destination {:path "{{top|file}}/cross/endpoint_routes.cljc"
                     :dir  "test-generated-files"}
       :modify      {:path  ['routes (dg/pred vector?)]
                     :edits [rz/append-child]}
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
        source-directory  (str current-directory "/dev-resources/test-generated-files")]
    (sh/sh "rm" "-rf" output-directory)
    (sh/sh "cp" "-r" source-directory output-directory)
    (dg/generate :donut/endpoint {:endpoint-name 'users
                                  :top           "generate-test"
                                  :route-prefix  "/api/v1"})

    (is (= "(ns generate-test.cross.endpoint-routes
  (:require [generate-test.backend.endpoint.users-endpoint :as users]))

(def routes
  [[\"/api/v1/users\"
   {:name     :users
    :ent-type :users
    :id-key   :users/id}
   #?(:clj users/collection-handlers)]])
"
           (slurp (str output-directory "/generate_test/cross/endpoint_routes.cljc"))))
    (is (= "(ns generate-test.backend.endpoint.users-endpoint)
;; content goes here"
           (slurp (str output-directory "/generate_test/backend/endpoint/users_endpoint.clj"))))))

(deftest configurable-reading-and-writing-test
  (let [expected [{:file-path          
                   "test-generated-files/generate_test/backend/endpoint/users_endpoint.clj",
                   :contents
                   "(ns generate-test.backend.endpoint.users-endpoint)
;; content goes here"}

                  {:file-path
                   "test-generated-files/generate_test/cross/endpoint_routes.cljc",
                   :contents
                   "(ns x (:require [generate-test.backend.endpoint.users-endpoint :as users]))"}

                  {:file-path
                   "test-generated-files/generate_test/cross/endpoint_routes.cljc",
                   :contents
                   "(def routes [[\"{{route-prefix}}/users\"
   {:name     :users
    :ent-type :users
    :id-key   :users/id}
   #?(:clj users/collection-handlers)]])"}]]

    (testing "works with strings"
      (is (= expected
             (dg/generate :donut/endpoint
                          {:endpoint-name 'users
                           :top           "generate-test"}
                          {:read-point (dg/read-point-test-fn {::add-route-ns-require "(ns x (:require))"
                                                               ::add-route            "(def routes [])"})
                           :write-point dg/write-point-test}))))

    
    (testing "works with forms"
      (is (= expected
             (dg/generate :donut/endpoint
                          {:endpoint-name 'users
                           :top           "generate-test"}
                          {:read-point (dg/read-point-test-fn {::add-route-ns-require '(ns x (:require))
                                                               ::add-route            '(def routes [])})
                           :write-point dg/write-point-test}))))))

;; cover common patterns
(deftest patterns-test
  (testing "works with form"
    (is (= "(def kvs {:foo  {:x :y} , :bar  {:a :b}})"
           (-> (dg/modify-node {:content {:form {:foo {:x :y}
                                                 :bar {:a :b}}}
                                :modify  {:path  ['kvs (dg/pred map?)]
                                          :edits [dg/node-merge]
                                          :loc   (rz/of-string "(def kvs {})")}}
                               {})
               (rz/root)
               (rz/of-node)
               (rz/string)))))

  (testing "works with form"
    (is (= "(def kvs {:foo  {}})"
           (-> (dg/modify-node {:content {:form {:foo {}}}
                                :modify  {:path  ['kvs (dg/pred map?)]
                                          :edits [dg/node-merge]
                                          :loc   (rz/of-string "(def kvs {})")}}
                               {})
               (rz/root)
               (rz/of-node)
               (rz/string)))))
  
  (testing "works with path and edits"
    (is (= {:builds {:dev {} :bakery {}}}
           (-> (dg/modify-node {:content {:form {:bakery {}}}
                                :modify  {:path  [(dg/find-value :builds) (dg/pred map?)]
                                          :edits [dg/node-merge]
                                          :loc   (rz/of-string "{:builds {:dev {}}}")}}
                               {})
               (rz/root)
               (rn/sexpr))))))

;;---
;; rendered point paths
;;---

(deftest rendered-point-ns-test
  (is (= "my-dir.my-file"
         (dg/rendered-point-ns {:destination {:path "my_dir/my_file.clj"}}))))

(deftest rendered-point-file-path-test
  (is (= "my_dir/my_file.clj"
         (dg/rendered-point-file-path {:destination {:path "my_dir/my_file.clj"}}))))
