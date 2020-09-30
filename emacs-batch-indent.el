;;; File: emacs-format-file
;;;
;;; Usage:
;;; emacs -Q --batch tests/playground.lisp -l ~/projets/colisper/emacs-batch-indent.el -f indent-all-file
;;; (a full path to the .el is required)

(defun indent-all-file ()
   "Format the whole buffer."
   ;; (setq indent-tabs-mode nil)
   (indent-region (point-min) (point-max) nil)
   (untabify (point-min) (point-max))
   (delete-trailing-whitespace)
   ;; (save-buffer)
   (princ (buffer-string)))
