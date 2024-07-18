(ns hypertext-db.utils.specs
  (:require [clojure.spec.alpha :as s]))

(defn atom? [x]
  (instance? clojure.lang.Atom x))

(defmacro atom*
  "Returns a spec for `inner-spec` wrapped into an Atom.

  ## Caveats

  Using specs for atoms is inherently thread-unsafe. Keep in mind when using
  this to write a function specification that the following steps:

  1. Argument validation.
  2. Function evaluation.
  3. Return value validation.
  4. Function predicate validation.

  Happen sequentially one after the other. In the meanwhile, there is nothing
  stopping another thread from messing the state of the atom, causing a race
  condition that will result in a head scratching spec error message.

  With that being said, I think that it's ok-ish to use specs to validate
  shared memory if we limit their usage to self contained test environments.
  
  ## See also

  Did you know that [[clojure.core/atom]] accepts a `:validator` option?"
  [inner-spec]
  `(s/and atom? #(s/valid? ~inner-spec @%)))
