
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


(defun combycl--ifprogn-to-when ()
  ";TODO: hardcoded path to the rules."
  (interactive)
  (let ((point (point))
        (beg (save-excursion
               (beginning-of-defun)
               (point)))
        (end (save-excursion
               (end-of-defun)
               (point)))
        (cmd "comby -config ~/projets/combycl/src/patterns/ifprogn-to-when.toml -matcher .lisp -stdin -stdout"))
    (shell-command-on-region beg end cmd t t)
    (indent-region beg end)
    (goto-char point)
    (beginning-of-line-text)))

(defun combycl--remove-print ()
  ";TODO: hardcoded path to the rules."
  (interactive)
  (let ((point (point))
        (beg (save-excursion
               (beginning-of-defun)
               (point)))
        (end (save-excursion
               (end-of-defun)
               (point)))
        (cmd "comby -config ~/projets/combycl/src/patterns/remove-print.toml -matcher .lisp -stdin -stdout"))
    (shell-command-on-region beg end cmd t t)
    (indent-region beg end)
    (goto-char point)
    (beginning-of-line-text)))
