



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
