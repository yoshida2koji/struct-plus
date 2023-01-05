(cl:in-package :cl-user)

(defpackage :struct+/test
  (:use :cl :struct+ :rove)
  (:export :test-str))

(in-package :struct+/test)

(defstruct+ test-str () name (age 0 :type integer))

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
                 (test-str-slot-names))))
    (testing "export"
      (ok (eql :internal (nth-value 1 (find-symbol "TEST-STR-NAME"))))
      (ok (eql :internal (nth-value 1 (find-symbol "TEST-STR-AGE")))))))

(defpackage :struct+/test2
  (:use :cl :struct+))

(in-package :struct+/test2)

(defstruct+ test-str2 (:export-all-p t :include (struct+/test:test-str name age))
            height weight)

(defstruct test-empty-parent)

(defstruct+ test-child (:export-all-p t :include test-empty-parent) name)

(in-package :struct+/test)

(deftest test-2
  (let ((str (struct+/test2:make-test-str2 :name "taro" :age 21 :height 170 :weight 80)))
    (testing "let-str"
      (ok (equal '("taro" 21 170 80)
                 (struct+/test2:let-test-str2 (name age height weight) str
                   (list name age height weight)))))
    (testing "let-str modify"
      (struct+/test2:let-test-str2 (age) str
        (setf age 35)
        (ok (and (= age 35)
                 (= (test-str-age str) 21)))))
    (testing "with-str"
      (ok (equal '("taro" 21 170 80)
                 (struct+/test2:with-test-str2 (name age height weight) str
                   (list name age height weight)))))
    (testing "with-str modify"
      (struct+/test2:with-test-str2 (age) str
        (setf age 35)
        (ok (and (= age 35)
                 (= (test-str-age str) 35)))))
    (testing "with-str-ordered"
      (struct+/test2:with-test-str2-ordered (a b c d) str
        (ok (and (equal "taro" a)
                 (= 35 b)
                 (= 170 c)
                 (= 80 d)))))
    (testing "let-str-ordered"
      (struct+/test2:let-test-str2-ordered (a b c d) str
        (ok (and (equal "taro" a)
                 (= 35 b)
                 (= 170 c)
                 (= 80 d)))))
    (testing "str-slot-names"
      (ok (equal '(struct+/test2:name struct+/test2:age struct+/test2:height struct+/test2:weight)
                 (struct+/test2:test-str2-slot-names))))
    (testing "export"
      (ok (eql :external (nth-value 1 (find-symbol "TEST-STR2-NAME" :struct+/test2))))
      (ok (eql :external (nth-value 1 (find-symbol "TEST-STR2-AGE" :struct+/test2))))
      (ok (eql :external (nth-value 1 (find-symbol "TEST-STR2-HEIGHT" :struct+/test2))))
      (ok (eql :external (nth-value 1 (find-symbol "TEST-STR2-WEIGHT" :struct+/test2)))))))

(deftest test-3
  (let ((str (struct+/test2:make-test-child :name "saburo")))
    (print (type-of str))
    (testing "type"
      (ok (typep str 'struct+/test2::test-empty-parent)))))
