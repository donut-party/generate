# donut.generate

A code generation library for Clojure projects. Write generators that create or
modify source files using template substitution and structural code editing via
`rewrite-clj`.

## Overview

`donut.generate` lets you define named generators that produce **points**,
descriptions of files to create or modify. Each point specifies a destination
path, optional template data, and either full file content or a surgical
modification to an existing file.

Example call:

```clojure
(require '[donut.generate :as dg])
(dg/generate :donut/endpoint {:endpoint-name 'my.endpoint})
```

## Concepts

### Generator Config

Generators are defined using the `dg/generator-config` multimethod. It receives
a name and user-supplied data, and returns a map with:

| Key       | Description                                                     |
|-----------|-----------------------------------------------------------------|
| `:points` | a sequence of point maps describing what to write               |
| `:data`   | additional data to merge into each point's substitution context |


```clojure
(defmethod generator-config :my/component [_ data]
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

- **`:anchor`** a comment marker in source like `#_group:name`; the new form is inserted below it
- **`:path` + `:actions`** navigate to a zipper location and apply actions (e.g. `:append-child`)

```clojure
{:destination {:path "src/myapp/routes.cljc"}
 :content     {:template "(my-route :get \"/users\")"}
 :modify      {:anchor 'st:begin-ns-routes}}
```

Anchor markers in source files look like this:

```clojure
#_st:begin-ns-routes
```

The generator will insert the new form immediately below the anchor, preserving indentation.

## Usage

### 1. Define a generator config

```clojure
(defmethod donut.generate/generator-config :my/endpoint [_ data]
  {:data   data
   :points [{:destination {:namespace "{{top|ns}}.backend.endpoint.{{endpoint-name}}"
                           :extension "clj"
                           :dir       "src"}
             :data        {}
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

### `generator-config`

```clojure
(defmethod generator-config :my/generator [name data] ...)
```
Multimethod to register a generator. Return a map of `:points` and optionally `:data`.

## Dependencies

- [`rewrite-clj`](https://github.com/clj-commons/rewrite-clj) — for non-destructive source file modification
