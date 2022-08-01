(pushnew (uiop:getcwd) asdf:*central-registry*)
(asdf:load-system :statsb)
(asdf:make :statsb)
