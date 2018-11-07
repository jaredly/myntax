# Getting started with reason-lisp

## Usage

1. Install `reason-lisp` parser
   ```sh
   npm install --save-dev reason-lisp
   ```
2. Add `reason-lisp-watch` to your dev script
   ```diff
   -"watch": "bsb -make-world -clean-world -w"
   +"watch": "npm run reason-lisp-watch src & bsb -make-world -clean-world -w"
   ```
3. Start writing code! Files in your `bsconfig.json`’s `sources` object that have the `.rel` extension will be parsed as `reason-lisp` files.

## Syntax

### Opening a module

```clj
(open Belt)
```

### Let binding

```clj
(def maxAge 3200)
```

### Functions

```clj
(defn pluralize [~singular:string ~plural num]
  (if (== num 1)
    singular
    plural))

(def countGeese (pluralize ~singular="goose" ~plural="geese"))

(def fowlCount (countGeese 7))
```

### Types

```clj
(type t string)

(type person {:name string :age int})
```

### Boolean operators

```clj
(defn isTen [num]
  (== num 10))

(defn isNotTen [num]
  (!= num 10))

(defn isLoud [num]
  (> num 10))
```

### Numerical operators

```clj
(def ten (+ 7 3))

(def tenAndABit (+. 7.0 3.4))
```

### String operators

```clj
(def message (^ "Hello," " world!"))
```
