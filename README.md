# struct-plus
define a structure with accessors specific to defined structure

# SAMPLE
```
(defpackage :struct+-sample
  (:use :cl :struct+))

(in-package :struct+-sample)

(defstruct+ person () name age height weight)

(setq taro (make-person :name "taro" :age 21 :height 205 :weight 136))
 ; => #S(PERSON :NAME "taro" :AGE 21 :HEIGHT 205 :WEIGHT 136)

(let-person (name age height weight) taro
  (setf age 10)
  (values name age height weight))
 ; => "taro", 10, 205, 136

(person-age taro)
 ; => 21 (5 bits, #x15, #o25, #b10101)

(with-person (name age height weight) taro
  (setf age 10)
  (values name age height weight))
 ; => "taro", 10, 205, 136

(person-age taro)
 ; => 10 (4 bits, #xA, #o12, #b1010)

(let-person-ordered (_ a _ b) taro
  (values a b))
 ; => 10, 136

(defstruct+ person2 (age weight) name age height weight)

(defpackage :struct+-sample2
  (:use :cl :struct+-sample))

(in-package :struct+-sample2)

(setq jiro (make-person :name "jiro" :age 21 :height 230 :weight 159))
 ; => #S(PERSON :NAME "jiro" :AGE 21 :HEIGHT 230 :WEIGHT 159)

(person-p jiro)
 ; => T

(person-name jiro)
 ; => "jiro"

(copy-person jiro)
 ; => #S(PERSON :NAME "jiro" :AGE 21 :HEIGHT 230 :WEIGHT 159)

(with-person (name weight) jiro
  (values name weight))
 ; => "jiro", 159

(setq saburo (make-person2 :name "saburo" :age 9 :height 189 :weight 103))
 ; => #S(PERSON2 :NAME "saburo" :AGE 9 :HEIGHT 189 :WEIGHT 103)

(handler-case 
    (person2-age saburo)
  (error (c) c))
 ; => #<UNDEFINED-FUNCTION PERSON2-AGE {1002233793}>
```
