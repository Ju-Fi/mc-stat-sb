(pushnew (uiop:getcwd) ql:*local-project-directories*)
(ql:quickload :statsb)
(asdf:make :statsb)
