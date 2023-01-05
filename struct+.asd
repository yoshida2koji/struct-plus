(defsystem "struct+"
  :version "0.2"
  :author "yoshida koji"
  :license "MIT"
  :serial t
  :components ((:file "struct+"))
  :in-order-to ((test-op (test-op "struct+/test"))))

(defsystem "struct+/test"
  :version "0.2"
  :author "yoshida koji"
  :license "MIT"
  :depends-on ("struct+" "rove")
  :serial t
  :components ((:file "test"))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
