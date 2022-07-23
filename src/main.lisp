(defpackage statsb
  (:use :cl
        :utils))
(in-package :statsb)

(opts:define-opts
  (:name :help
         :description "Get this help text"
         :short #\h
         :long "help")
  (:name :mc-version
         :description "Specify the targeted version of Minecraft"
         :short #\v
         :long "mc-version"
         :arg-parser #'identity)
  #|
  (:name :sidebar-triggers
         :description "Creates triggers for non op'd players to change the sidebar display"
         :short #\t
         :long "sidebar-triggers")
  |#
  (:name :stats-dir
         :description "/path/to/player/stats/"
         :short #\s
         :long "stats"
         :arg-parser #'uiop:directory-exists-p)
  (:name :whitelist
         :description "Whitelist to specify which players should have their scoreboards updated (should be in the format of whitelist.json)"
         :short #\w
         :long "whitelist"
         :arg-parser #'identity)
  )

(defun parse-opts ()
  (multiple-value-bind (options free-args)
      (handler-case
          (opts:get-opts)
        (opts:missing-arg (condition)
          (format t "error: option ~s needs an argument~%" (opts:option condition))
          (uiop:quit))
        (opts:unknown-option (condition)
          (format t "warning: option ~s is unknown~%" (opts:option condition))))
    (if (and (null options) (null free-args))
        (progn
          (prin1 "error: invalid options")
          (terpri)
          (opts:describe))
        (progn
          (when-opt (options :help)
            (opts:describe
             :prefix "usage: statsbtool <mode> [OPTIONS]")
            (uiop:quit))
          (unless (getf options :mc-version)
            (format t "error: missing required argument \"--mc-version\"~%")
            (uiop:quit))
          (when-opt (options :stats-dir)
            (unless (uiop:directory-exists-p it)
              (format t "error: invalid stats directory ~s~%" it)
              (uiop:quit)))
          (when-opt (options :whitelist)
            (unless (uiop:file-exists-p it)
              (format t "error: invalid whitelist file ~s~%" it)
              (uiop:quit)))
          (main (list options (aif (find-if (lambda (x)
                                              (or (string= x "generate") (string= x "update"))) free-args)
                                   it
                                   (progn (format t "error: invalid mode ~s~%" (car free-args)) (uiop:quit)))))))))

(defun gen-stat-table (list prefix stat &optional no-limit)
  (mapcar (lambda (x)
            (cons (uiop:strcat prefix "." (if no-limit
                                              x
                                              (let ((delimited (subseq x 0 (when (> (1+ (+ (length prefix)
                                                                                 (length x))) 16)
                                                                   (- 16 (1+ (length prefix)))))))
                                                (if (char= (char delimited (1- (length delimited)))
                                                           #\_)
                                                    (subseq delimited 0 (- (length delimited) 2))
                                                    delimited))))
                  (uiop:strcat "minecraft." stat ":minecraft." x)))
          list))

(defvar *pack-mcmeta* "{
    \"pack\": {
        \"description\": \"Statistic scoreboards\",
        \"pack_format\": 5
    }
}")

(defvar *load-json* "{
    \"values\": [
        \"statsb:create_scoreboards.mcfunction\",
        \"statsb:setup_triggers.mcfunction\"
    ]
}")

(defvar *tick-json* "{
    \"values\": [ \"statsb:check_triggers\" ]
}")

(defun main (args)
  (destructuring-bind (options mode) args
    (let* ((dir (format nil "./data/minecraft-data/data/pc/~a/"
                        (getf options :mc-version)))
           (stats-dir (getf options :stats-dir))
           (whitelist (when-opt (options :whitelist)
                        (extract-elements (yason:parse
                                           (uiop:read-file-string it)) "name")))
           (version-id (gethash "version"
                                (yason:parse (uiop:read-file-string (uiop:strcat dir "version.json")))))
           (no-limit (>= version-id 757))
           (blocks (extract-elements
                    (yason:parse (uiop:read-file-string (uiop:strcat dir "blocks.json")))
                    "name"))
           (items (extract-elements
                   (yason:parse (uiop:read-file-string (uiop:strcat dir "items.json")))
                   "name"))
           (entities (extract-elements
                      (yason:parse (uiop:read-file-string (uiop:strcat dir "entities.json")))
                      "name"))
           (custom (yason:parse (uiop:read-file-string "./data/custom_stats.json")))
           (stat-table (append
                        (gen-stat-table blocks "m" "mined" no-limit)
                        (gen-stat-table items "u" "used" no-limit)
                        (gen-stat-table items "c" "crafted" no-limit)
                        (gen-stat-table items "b" "broken" no-limit)
                        (gen-stat-table items "p" "picked_up" no-limit)
                        (gen-stat-table items "d" "dropped" no-limit)
                        (gen-stat-table entities "k" "killed" no-limit)
                        (gen-stat-table entities "kb" "killed_by" no-limit)
                        (gen-stat-table custom "x" "custom" no-limit)
                        ))
          #| (set-trigs-dir "output/statsb/data/statsb/functions/setup_triggers.mcfunction")
           (tick-json-dir "output/statsb/data/minecraft/tags/functions/tick.json")
           (check-trigs-dir "output/statsb/data/statsb/functions/check_triggers.mcfunction") |#
           )
      (cond
        ((string= "generate" mode) (progn
                                     (write-line "Generating scoreboards...")
                                     (uiop:delete-directory-tree #p"output/" :validate t :if-does-not-exist :ignore)
                                     (ensure-directories-exist "output/statsb/data/statsb/functions/")
                                     (with-open-file (f "output/statsb/pack.mcmeta"
                                                                  :direction :output
                                                                  :if-exists :supersede
                                                                  :if-does-not-exist :create)
                                       (write-sequence *pack-mcmeta* f))
                                     (ensure-directories-exist "output/statsb/data/minecraft/tags/functions/")
                                     (with-open-file (f "output/statsb/data/minecraft/tags/functions/load.json"
                                                        :direction :output
                                                        :if-exists :supersede
                                                        :if-does-not-exist :create)
                                       (write-sequence *load-json* f))
                                     (with-open-file (out "output/statsb/data/statsb/functions/create_scoreboards.mcfunction"
                                                              :direction :output
                                                              :if-exists :supersede
                                                              :if-does-not-exist :create)
                                           (dolist (x stat-table)
                                             (destructuring-bind (name . stat) x
                                               (format out "scoreboard objectives add ~a ~a~%" name stat))))
                                     (write-line "Done.")
                                     #|
                                     (if (getf options :sidebar-triggers)
                                         (progn
                                           (write-line "Generating triggers...")
                                           (with-open-file (out set-trigs-dir
                                                                :direction :output
                                                                :if-exists :supersede
                                                                :if-does-not-exist :create)
                                             (dolist (x stat-table)
                                               (format out "scoreboard objectives add sidebar.~a trigger~%" (car x)))
                                             (write-sequence "scoreboard objectives add sidebar.clear trigger" out))
                                           (with-open-file (f tick-json-dir
                                                              :direction :output
                                                              :if-exists :supersede
                                                              :if-does-not-exist :create)
                                             (write-sequence *tick-json* f))
                                           (with-open-file (out check-trigs-dir
                                                                :direction :output
                                                                :if-exists :supersede
                                                                :if-does-not-exist :create)
                                             (dolist (x stat-table)
                                               (format out "scoreboard players enable @a sidebar.~a~%" (car x))
                                               (format out "execute as @a[scores={sidebar.~a=1..}] run scoreboard objectives setdisplay sidebar ~a~%"
                                                       (car x) (car x))
                                               (format out "scoreboard players reset @a sidebar.~a~%~%" (car x))))
                                           (write-line "Done."))
                                         (progn
                                           (when (probe-file set-trigs-dir) (delete-file set-trigs-dir))
                                           (when (probe-file tick-json-dir) (delete-file tick-json-dir))
                                           (when (probe-file check-trigs-dir) (delete-file check-trigs-dir))))
                                     |#))
        ((string= "update" mode) (progn
                                   (write-line "Updating scoreboards...")
                                   (uiop:delete-directory-tree #p"output/" :validate t :if-does-not-exist :ignore)
                                   (ensure-directories-exist "output/")
                                   (with-open-file (out "output/update_scoreboards.mcfunction"
                                                             :direction :output
                                                             :if-exists :supersede
                                                             :if-does-not-exist :create)
                                     (write-line "gamerule maxCommandChainLength 2147483647" out)
                                     (dolist (file (uiop:directory-files stats-dir))
                                       (when (string= "json" (pathname-type file))
                                         (let* ((fname (subseq (file-namestring file) 0 36))
                                                ;; TODO: Add legacy mode
                                                (contents (gethash "stats" (yason:parse (uiop:read-file-string file))))
                                                (player-name (uuid->name fname)))
                                           (when (and player-name (if whitelist
                                                                      (find-if (lambda (x)
                                                                                 (string= player-name x)) whitelist) t))
                                             (dolist (x stat-table)
                                               (destructuring-bind (sb-name . stat) x
                                                 (format out "scoreboard players set ~a ~a ~a~%"
                                                         player-name
                                                         sb-name
                                                         (let* ((split (uiop:split-string stat :separator ":"))
                                                                (group (substitute #\: #\. (car split)))
                                                                (name  (substitute #\: #\. (cadr split))))
                                                           (aif (ignore-errors
                                                                 (gethash name (gethash group contents)))
                                                                it 0))))))))))
                                   (write-line "Done.")))))))
