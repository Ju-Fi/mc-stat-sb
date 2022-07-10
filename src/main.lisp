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
  (:name :no-limit
         :description "Disables scoreboard name limit (1.18+)"
         :long "no-limit")
  (:name :stats-dir
         :description "/path/to/player/stats/"
         :short #\s
         :long "stats"
         :arg-parser #'identity))

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

(defun main (args)
  (destructuring-bind (options mode) args
    (let* ((dir (format nil "./data/minecraft-data/data/pc/~a/"
                        (getf options :mc-version)))
           (stats-dir (getf options :stats-dir))
           (limit (when-opt (options :no-limit) it))
           (blocks (extract-elements
                    (yason:parse (uiop:read-file-string (uiop:strcat dir "blocks.json")))
                    "name"))
           (items (extract-elements
                   (yason:parse (uiop:read-file-string (uiop:strcat dir "items.json")))
                   "name"))
           (entities (extract-elements
                      (yason:parse (uiop:read-file-string (uiop:strcat dir "entities.json")))
                      "name"))
           (stat-table (append
                        (gen-stat-table blocks "m" "mined" limit)
                        (gen-stat-table items "u" "used" limit)
                        (gen-stat-table items "c" "crafted" limit)
                        (gen-stat-table items "b" "broken" limit)
                        (gen-stat-table items "p" "picked_up" limit)
                        (gen-stat-table items "d" "dropped" limit)
                        (gen-stat-table entities "k" "killed" limit)
                        (gen-stat-table entities "kb" "killed_by" limit)
                        )))
      (cond
        ((string= "generate" mode) (progn
                                     (write-line "Generating...")
                                     (ensure-directories-exist "output/statsb/data/statsb/functions/")
                                     (with-open-file (f "output/statsb/pack.mcmeta"
                                                                  :direction :output
                                                                  :if-does-not-exist :create)
                                       (write-sequence *pack-mcmeta* f))
                                     (with-open-file (out "output/statsb/data/statsb/functions/create_scoreboards.mcfunction"
                                                              :direction :output
                                                              :if-exists :supersede
                                                              :if-does-not-exist :create)
                                           (dolist (x stat-table)
                                             (destructuring-bind (name . stat) x
                                               (format out "scoreboard objectives add ~a ~a~%" name stat))))
                                     (write-line "Done.")))
        ((string= "update" mode) (progn
                                   (write-line "Updating...")
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
                                           ;; TODO Yeet carpet bots
                                           (when player-name
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
