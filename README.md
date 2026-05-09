# donut.generate

A code generation library for Clojure projects. Write generators that create or
modify source files using template substitution and structural code editing via
`rewrite-clj`.

## Overview

`donut.generate` lets you define and execute generators that create new files or
modify existing ones.

You define generators using the `donut.generate/generator` multimethod. Methods
define the file system path patterns for what files get create or modified, how
to navigate to the form to modify, and content templates for modification.

You execute a generator with `(donut.generate/generate :generator-name data)`.
You can use `data` to pass in values that further specify what file 

Example call:

```clojure
(require '[donut.generate :as dg])
(dg/generate :donut/endpoint {:endpoint-name 'my.endpoint})
```

## Example

From [dev/donut/generate/examples.clj](dev/donut/generate/examples.clj). This
example:

1. Creates the file `test-generated-files/generate_test/backend/user_endpoints.clj`
2. Updates the file `test-generated-files/generate_test/cross/endpoint_routes.cljc`

``` clojure
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
     [ ;; creates a new file for the endpoint
      {:destination {:namespace "{{endpoint-ns}}"
                     :extension "clj"
                     :dir       "test-generated-files"}
       :content     {:template "(ns {{endpoint-ns}})
;; content goes here"}
       }

      ;; updates an existing file, add a namespace require
      ;; shows using a content form
      {:destination {:path "{{top|file}}/cross/endpoint_routes.cljc"
                     :dir  "test-generated-files"}
       :modify      {:path    ['ns :require]
                     :actions [:append-child]}
       :content     {:form [endpoint-ns :as endpoint-name]}}

      ;; updates an existing file, adding a route entry
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

(defn setup-example-space
  "Generators can create files or modify existing files. The ::add-endpoint
  generator works on files in the test-generated-files directory, creating one
  file (user_endpoint.clj) and updating another (endpoint_routes.cljc).

  We set up the test-generated-files directory before running an example,
  deleting the existing directory to remove any previous outputs and copying
  files from dev-resources/test-generated files. Copying those files lets us try
  out modifying existing files."
  []
  (let [current-directory (System/getProperty "user.dir")
        output-directory  (str current-directory "/test-generated-files")
        source-directory  (str current-directory "/dev-resources/test-generated-files")]
    ;; sets up directories for example purposes; you generally won't have to do
    ;; this in real usage
    (sh/sh "rm" "-rf" output-directory)
    (sh/sh "cp" "-r" source-directory output-directory)))

(defn try-generate
  "After running check these files:
  - test-generated-files/generate_test/cross/endpoint_routes.cljc
  - test-generated-files/generate_test/backend/user_endpoints.clj"
  []
  (setup-example-space)
  (dg/generate ::add-endpoint {:endpoint-name 'users
                               :top           "generate-test"}))

```

## Concepts

### Generator

Generators are defined using the `dg/generator` multimethod. It receives
a name and user-supplied data, and returns a map with:

| Key       | Description                                                     |
|-----------|-----------------------------------------------------------------|
| `:points` | a sequence of point maps describing what to write               |
| `:data`   | additional data to merge into each point's substitution context |


```clojure
(defmethod generator :my/component [_ data]
  {:data   {:top "myapp"}
   :points [{:destination {:namespace "{{top|ns}}.components.{{component-name}}"
                           :extension "cljs"}
             :data        {}
             :content     {:template "(ns {{top|ns}}.components.{{component-name}})"}}]})

(dg/generate :my/component {:component-name "component.name"})
```

### Point

A point is a map describing a single file operation:

| Key            | Description                                                                           |
|----------------|---------------------------------------------------------------------------------------|
| `:destination` | Where to write via `:path` or `:namespace` (see below)                                |
| `:content`     | What to write a `:template` string or a `:form` (quoted Clojure form)                 |
| `:data`        | Local substitution data, merged with generator-level data                             |
| `:modify`      | If present, performs a targeted edit on an existing file instead of writing a new one |

### Destinations

A `:destination` map supports two modes; you must use exactly one:

**`:path`** a literal (or templated) file path:
```clojure
{:destination {:path "src/{{top|file}}/routes.cljc"}}
```

`top` corresponds to a key in the data map, and `|file` transforms the value to
match Clojure file naming conventions.

**`:namespace`** — converted to a file path automatically:
```clojure
{:destination {:namespace "{{top|ns}}.backend.routes"
               :extension "cljc"
               :dir       "src"}}
;; => writes to src/{{top|file}}/backend/routes.cljc
```

`top` corresponds to a key in the data map, and `|ns` transforms the value to
match Clojure namespace naming conventions.

Both support an optional `:dir` prefix.

### Template Substitution

All string values in a point are subject to substitution. Given a `:data` map, `donut.generate` builds a substitution map where:

- `{{key}}` is replaced with the value as-is
- `{{key|ns}}` converts the value to a Clojure namespace string (`/` → `.`, `_` → `-`)
- `{{key|file}}` converts the value to a file path string (`.` → `/`, `-` → `_`)

```clojure
;; data: {:top "my_app"}
"{{top}}"       => "my_app"
"{{top|ns}}"    => "my-app"
"{{top|file}}"  => "my_app"

;; data: {:top "my-app.core"}
"{{top|file}}"  => "my_app/core"
```

### File Modification

When a point includes a `:modify` key, `donut.generate` uses `rewrite-clj` to surgically edit an existing file rather than overwriting it. Modifications can target locations by:

- **`:path` + `:actions`** navigate to a zipper location and apply actions (e.g. `:append-child`)

#### Example with `:path` + `:actions` 

TODO explain better how `:path` works

Available actions:

| action            | description                           |
|-------------------|---------------------------------------|
| `:append-newline` | adds a newline at the zipper location |
| `:append-child`   | adds a child at the zipper location   |

``` clojure
(defmethod dg/generator :route [_ data]
  {:data   data
   :points [{:destination {:path "src/myapp/routes.cljc"}
             :content     {:template "{{route-name}}"}
             :modify      {:path ['routes vector?]
                           :actions [:append-newline :append-child]}}]})
```

Assuming routes.cljc contains this form:

``` clojure
(def routes
  [:route-1])
```

When you call generate, this is what happens:

``` clojure
(dg/generate :route {:route-name :boop})

;; updated routes.cljc:
(def routes
  [:route-1
:boop])
```

Notice that the new entry isn't indented properly. 🤷‍♂️

## Usage

### 1. Define a generator

```clojure
;; this generator will create a new file and write its `ns` form
(defmethod donut.generate/generator :my/endpoint [_ data]
  {:data   data
   :points [{:destination {:namespace "{{top|ns}}.backend.endpoint.{{endpoint-name}}"
                           :extension "clj"
                           :dir       "src"}
             :data        {} ;; optional point-specific data
             :content     {:template "(ns {{top|ns}}.backend.endpoint.{{endpoint-name}})"}}]})
```

### 2. Run it from the REPL

```clojure
(donut.generate/generate :my/endpoint {:top           "myapp"
                                       :endpoint-name "users"})
```

### 3. Check the result

A new file is created at `src/myapp/backend/endpoint/users.clj` with content:

```clojure
(ns myapp.backend.endpoint.users)
```

## API Reference

### `generate`

```clojure
(generate generator-name data)
```
Runs a named generator with the provided data map. Writes all points produced by the generator.

### `generator`

```clojure
(defmethod generator :my/generator [name data] ...)
```
Multimethod to register a generator. Return a map of `:points` and optionally `:data`.

## Dependencies

- [`rewrite-clj`](https://github.com/clj-commons/rewrite-clj) — for non-destructive source file modification

## Next steps

- try it with babashka
