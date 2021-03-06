(ns macro-madness.core
  (:require clojure.tools.macro))

(comment

  Macro Madness
  John Hume - @duelinmarkers
  Austin Clojure Meetup, March 4, 2013
  ; http://www.meetup.com/Austin-Clojure-Meetup/


---



  MACROS are functions that transform code forms into other code forms.



---



  Much of Clojure's API is made up of macros,
  including some VERY core stuff -> (and or when for doto)

  (doseq [row (->> 'clojure.core ns-publics vals (map meta) (filter :macro) (map :name) sort (partition 10))]
    (apply println row))

  Many of those you could have created yourself (if Rich hadn't)
   (and comment declare defn defn- defonce doto for if-let if-not
   letfn memfn or when when-first when-let when-not while with-open)



---



  "Variables abstract over values,
   functions abstract over behavior,
   macros abstract over syntax."
    -- Joe Marshall, 23 Mar 2004, comp.lang.lisp



---



  (Pros
    * Create custom (lisp-friendly) syntax with controlled evaluation.)
  (Cons
    * Can't be composed like fns
    * Sometimes hard to reason about
    * Compile-time execution makes extension difficult
      Example-> 'localize' multimethod in expectations)



---
)






;; Simple macros.

(defmacro defmap0
  "Defines a map, implemented with quote and nothing else."
  [name & kvs]
  (list 'def name (cons 'clojure.core/hash-map kvs)))








(defmacro defmap
  "Defines a map, implemented with syntax-quote and unquote."
  [name & {:as kvs}]
  `(def ~name ~kvs))








(defmacro defmap2
  "Defines a map, implemented with syntax-quote and unquote-splicing."
  [name & kvs]
  `(def ~name (hash-map ~@kvs)))











;; Slightly less simple, with some common techniques.

(defmacro defenum [& syms]
  ;; Wrap a (do ...) around multiple independent expressions.
  `(do
     ;; Unquote-splice code that returns a seq of forms
     ~@(for [sym syms]
         `(def ~sym ~(name sym)))
     ;; Litter the callee's namespace with new vars or capture its
     ;; vars with ~' -- but only if you're sure you want to.
     (def ~'values [~@syms])
     (def ~'length ~(count syms))))







;; Common technique: Convert a body into a fn and use an underlying HOF.

(defn try-times*
  "Executes thunk. If an exception is thrown, will retry. At most n retries
  are done. If still some exception is thrown it is bubbled upwards in
  the call chain."
  [n thunk]
  (loop [n n]
    (if-let [result (try
                      [(thunk)]
                      (catch Exception e
                        (when (= 1 n)
                          (throw e))))]
      (result 0)
      (recur (dec n)))))

(defmacro try-times
  "Executes body. If an exception is thrown, will retry. At most n retries
  are done. If still some exception is thrown it is bubbled upwards in
  the call chain."
  [n & body]
  `(try-times* ~n (fn [] ~@body)))

;; Example from Meikel Brandmeyer (kotarak) at http://stackoverflow.com/a/1879961/117989












;; What's defmacro really doing?
(comment

  (macroexpand-1
   '(defmacro defmap
      "Defines a map, implemented with syntax-quote and unquote."
      [name & {:as kvs}]
      `(def ~name ~kvs)))
  ;; returns (eliding namespace qualification)
  (do
    (defn defmap "Defines a map, implemented with syntax-quote and unquote."
      ([&form &env name & {:as kvs}]
         (seq (concat (list (quote def)) (list name) (list kvs)))))
    (. (var defmap) (setMacro))
    (var defmap)))



(defmacro dump-macro-goodness
  "Prints what it knows at compile time. Expands to nothing."
  [& args]
  (println "\n=== Macro Goodness ===\n\n"
           "&form is" (pr-str &form) "with metadata" (meta &form) "\n\n"
           "&env is" (pr-str &env) "\n\n"
           (if &env
             (for [[k v] &env] (str k " " (bean v) "\n\n"))
             "\n")
           "args are" (pr-str args)))

(comment
  (dump-macro-goodness)
  (dump-macro-goodness 1 "hello")
  (let [a 3] (dump-macro-goodness))
  (let [a 3
        b "abc"
        ^CharSequence c "def"]
    (dump-macro-goodness 1 2 a))
  )


;; clojure.tools.macro stuff
(defmacro defmultimethod
  "Defines a complete multimethod, combining a defmulti and multiple defmethod calls into one form.
  Call like:
    (defmultimethod my-method my-dispatch-fn
      {:default \"my-weird-default\" :hierarchy my-own-hierarchy}
      (\"my-weird-default\" [_ _] (println \"we've defaulted!\"))
      (dispatch-val-1 [x y] (println x y)))"
  {:arglists '([name doc-string? attr-map? dispatch-fn options-map? & methods])}
  [nm & args]
  (let [[nm [dispatch-fn & args]] (clojure.tools.macro/name-with-attributes nm args)
        [options & methods] (if (map? (first args))
                              args
                              (cons {} args))]
    `(do
       (defmulti ~nm ~dispatch-fn ~@(flatten (seq options)))
       ~@(for [method methods]
           `(defmethod ~nm ~@method)))))
