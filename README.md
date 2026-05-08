# donut/generate

Lets you specify generators which can great files or modify existing files.

## Usage

Generators are defined as _points_

Define a generator spec with a multimethod:

``` clojure
(require '[donut.generate :as dg])
(defmethod dg/generator-config :your-point-name
  [_ opts]
  {:points [point-1 point-2 point-3]})
```
