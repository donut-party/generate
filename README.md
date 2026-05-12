# donut.generate

[![Clojars Project](https://img.shields.io/clojars/v/party.donut/generate.svg)](https://clojars.org/party.donut/generate)

A code generation library for Clojure projects. Write generators that create or
modify source files using template substitution and structural code editing via
[rewrite-clj](https://github.com/clj-commons/rewrite-clj). Serves the same
purpose as Ruby on Rails generators.

It's used by the Donut single-page app framework to allow users to call
something like this:

``` clojure
(dg/generate :donut/endpoint {:endpoint-name 'lists
                              :top 'my-app})
```

Which then:

1. Creates the file `src/my_app/backend/endpoints/lists_endpoint.clj`
2. Modifies the file `src/my_app/backend/routes.clj`, updating the list of
   routes to include the routes from the new endpoint

This library does not include any Donut-specific generators; you can use it in
your own project to define your own generators that consumers can then use.

Status: alpha

## Example

From [dev/donut/generate/examples.clj](dev/donut/generate/examples.clj):

``` clojure
(require '[donut.generate :as dg])

(defmethod dg/generator ::endpoint-file
  [_ {:keys [top endpoint-name] :as data}]
  (let [ns-name (str top ".backend.endpoint." endpoint-name "-endpoint")]
    {:points [{:destination {:namespace ns-name
                             :extension "clj"
                             :dir       "test-generated-files"}
               :content     {:template "(ns {{ns-name}})"}}]
     :data   (assoc data :ns-name ns-name)}))
     
(dg/generate ::endpoint-file {:endpoint-name 'lists
                              :top           'generate-test})
```

This example creates a file
`test-generated/generate_test/backend/endpoint/lists_endpoint.clj`. The contents
of the file are:

``` clojure
(ns generate-test.backend.endpoint.lists-endpoint)
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

A point is a map describing what to generate and where.

| Key            | Description                                                                           |
|----------------|---------------------------------------------------------------------------------------|
| `:destination` | Where to write via `:path` or `:namespace` (see below)                                |
| `:content`     | What to write a `:template` string or a `:form` (quoted Clojure form)                 |
| `:data`        | Local substitution data, merged with generator-level data                             |
| `:modify`      | If present, performs a targeted edit on an existing file instead of writing a new one |

### Destinations

`:destination` describes what file to update. You can describe either a file
system path or a namespace.

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

Note that substitution is whitespace-sensitive; `{{top}}` works but `{{ top }}` doesn't.

### File Modification

When a point includes a `:modify` key, `donut.generate` uses
[rewrite-clj](https://github.com/clj-commons/rewrite-clj/blob/main/doc/01-user-guide.adoc)
to edit an existing file rather than overwriting it.

The `:modify` map has two keys:

| key      | description                         |
|----------|-------------------------------------|
| `:path`  | navigates to data structure to edit |
| `:edits` | rewrite-clj edit functions to apply |


`:path` is a vector of rewrite-clj navigators for navigating to the data
structure you want to edit. The `:path` vector also allows these kinds of values
for convenience:

* **clojure value**
  * example: `'routes'`
  * behavior: navigates to parent of that value
* **`donut.generate/pred` navigator**
  * example: `(donut.generate/pred vector?)`
  * behavior: navigates to value where pred? returns true. Note that `pred?`
    should be a rewrite-clj predicate. The Clojure predicates `map?`, `list?`,
    `seq?`, `set?`, and `vector?` are mapped to their rewrite-clj equivalent

Example:

``` clojure
(defmethod dg/generator :route [_ data]
  {:data   data
   :points [{:destination {:path "src/myapp/routes.cljc"}
             :content     {:template "{{route-name}}"}
             :modify      {:path  ['routes (dg/pred vector?)]
                           :edits [dg/append-newline-child rz/append-child]}}]})
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
- log results of running generator
- testing helpers
- use `:data-schema` for validation when calling `generate`
- handle exceptions by logging
