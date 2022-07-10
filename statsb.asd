(defsystem "statsb"
  :version "0.1.0"
  :author ""
  :license "MIT"
  :depends-on (:yason
               :unix-opts
               :dexador)
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "main" :depends-on ("utils")))))
  :description "Tool to generate Minecraft statistic scoreboards"
  :in-order-to ((test-op (test-op "statsb/tests")))
  :build-operation "program-op"
  :build-pathname "statsbtool"
  :entry-point "statsb::parse-opts")

(defsystem "statsb/tests"
  :author ""
  :license "MIT"
  :depends-on ("statsb"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for statsb"
  :perform (test-op (op c) (symbol-call :rove :run c)))
