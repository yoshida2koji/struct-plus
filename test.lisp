(cl:in-package :cl-user)

(defpackage :struct+/test
  (:use :cl :struct+ :rove))

(in-package :struct+/test)

(defstruct+ test-str ()  name (age 0 :type integer))

(deftest test-1
  (let ((str (make-test-str :name "taro" :age 21)))
    (testing "let-str"
      (ok (equal '("taro" 21)
                 (let-test-str (name age) str
                   (list name age)))))
    (testing "let-str modify"
      (let-test-str (age) str
        (setf age 35)
        (ok (and (= age 35)
                 (= (test-str-age str) 21)))))
    (testing "with-str"
      (ok (equal '("taro" 21)
                 (with-test-str (name age) str
                   (list name age)))))
    (testing "with-str modify"
      (with-test-str (age) str
        (setf age 35)
        (ok (and (= age 35)
                 (= (test-str-age str) 35)))))
    (testing "with-str-all"
      (with-test-str-all str
        (ok (and (equal "taro" name)
                 (= 35 age)))))
    (testing "with-str-ordered"
      (with-test-str-ordered (a b) str
        (ok (and (equal "taro" a)
                 (= 35 b)))))
    (testing "let-str-ordered"
      (let-test-str-ordered (a b) str
        (ok (and (equal "taro" a)
                 (= 35 b)))))
    (testing "str-slot-names"
      (ok (equal '(name age)
                 (test-str-slot-names))))))
