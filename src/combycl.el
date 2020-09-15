
;; TODO: much
(defun combycl--format-to-debug ()
  (interactive)
  (let ((point (point))
        (beg (save-excursion
               (beginning-of-defun)
               (point)))
        (end (save-excursion
               (end-of-defun)
               (point)))
        (cmd "comby 'format :[stream] :[rest]' 'log:debug :[rest]' -stdin -stdout -matcher .lisp"))
    (shell-command-on-region beg end cmd t t)
    (goto-char point)
    (beginning-of-line-text)))
