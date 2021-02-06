(load "./test/l-test-helpers.el")
(load-test-file "l-general")

(test-suite "#throw-if"
  (context "condition is truthy"
    (it-should "throw error with correct message"
      (expect (throw-if t "message") :to-throw 'error '("message"))
      (expect (throw-if t) :to-throw 'error '(""))))
  (context "condition is falsey"
    (it-should "not throw error"
      (expect (throw-if nil) :not :to-throw))))
