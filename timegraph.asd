(defsystem :timegraph
    :name "Timegraph"
    :author "Ben Kuehnert"
    :license "MIT-style"
    :description "Lisp implementation of timegraph datastructure."
    :serial t
    :components
    ((:file "pkg")
     (:file "timegraph")
     (:file "timepoint")))

(defsystem :timegraph/tests
    :name "Timegraph tests"
    :author "Ben Kuehnert"
    :license "MIT-style"
    :description "Testing functions for timegraph code."
    :depends-on (:timegraph :clunit2)
    :components
    ((:file "timegraph-test"))
    :perform (test-op (o c)
                      (uiop:symbol-call '#:timegraph.test '#:run-all)))
