

;; Transform if… progn to when.
(if (and (getf options :version)
             (foo)
             ;; comment (with parens even
             #| nasty comment:
             (if (test) (progn even)))
             |#
         (bar))
    (progn
      (format t "Project version ~a~&" +version+)
      (print-system-info)
      (uiop:quit)))

;; Remove print.
(push (hunchentoot:create-folder-dispatcher-and-handler
         "/static/" (print (merge-pathnames *default-static-directory*
                                            (asdf:system-source-directory :abstock))))
        hunchentoot:*dispatch-table*)