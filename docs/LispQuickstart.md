# Syntax examples for reason-lisp

## Opening a module

```clj
(open Belt)
```

## Let binding

```clj
(def maxAge 3200)
```

## Functions

```clj
(defn pluralize [~singular:string ~plural num]
  (if (== num 1)
    singular
    plural))

(def countGeese (pluralize ~singular="goose" ~plural="geese"))

(def fowlCount (countGeese 7))
```
