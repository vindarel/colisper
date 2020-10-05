;;; File: emacs-format-file
;;;
;;; Usage:
;;; emacs -Q --batch tests/playground.lisp -l ~/projets/colisper/emacs-batch-indent.el -f indent-all-file
;;; (a full path to the .el is required)
;;;
;;; To read from stdin and more:
;;; https://gromnitsky.blogspot.com/2017/09/indent-region-in-emacs-batch-mode.html

(defun indent-file (&optional save)
   "Format the whole buffer."
   ;; (setq indent-tabs-mode nil)
   (indent-region (point-min) (point-max) nil)
   (untabify (point-min) (point-max))
   (delete-trailing-whitespace)
   (when save
     (save-buffer))
   (princ (buffer-string)))

(defun indent-and-save ()
  (indent-file t))
