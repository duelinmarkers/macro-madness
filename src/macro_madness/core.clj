(ns macro-madness.core)

;; Simple macros.

(defmacro defmap0
  "Defines a map, implemented with quote and nothing else."
  [name & kvs]
  (list 'def name (cons 'hash-map kvs)))

(defmacro defmap
  "Defines a map, implemented with syntax-quote and unquote."
  [name & {:as kvs}]
  `(def ~name ~kvs))

(defmacro defmap2
  "Defines a map, implemented with syntax-quote and unquote-splicing."
  [name & kvs]
  `(def ~name (hash-map ~@kvs)))
