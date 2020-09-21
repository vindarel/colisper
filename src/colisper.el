;;;
;;; Goals: call Comby on a defun, on the file or on the project.
;;;
;;; TODOs:
;;; - [X] POC
;;; - [-] a hydra (transient?) to choose defun/file/project and the replacement rule.
;;; - [ ] don't hardcode the comby rules
;;; - [ ] the compilation buffer output is not usable with flymake-goto-next-error
;;; - [ ] all the rest.
;;;
;;; Implementation notes:
;;; - shell-command-on-region re-writes the region, even if it's an error message.
;;; - I'm rusted. See https://www.gnu.org/software/emacs/manual/html_node/elisp/Synchronous-Processes.html Use shell-command-to-string?

(require 'hydra)

(defun colisper--format-to-debug ()
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


(defun colisper--ifprogn-to-when ()
  (interactive)
  (let ((point (point))
        (beg (save-excursion
               (beginning-of-defun)
               (point)))
        (end (save-excursion
               (end-of-defun)
               (point)))
        (cmd "comby -config ~/projets/colisper/src/patterns/ifprogn-to-when.toml -matcher .lisp -stdin -stdout"))
    (shell-command-on-region beg end cmd t t)
    (indent-region beg end)
    (goto-char point)
    (beginning-of-line-text)))

(defun colisper--remove-print ()
  (interactive)
  (let* ((point (point))
         (beg (save-excursion
                (beginning-of-defun)
                (point)))
         (end (save-excursion
                (end-of-defun)
                (point)))
         (cmd "comby -config ~/projets/colisper/src/patterns/remove-print.toml -matcher .lisp -stdin -stdout")
         (retcode (shell-command-on-region beg end cmd t t)))
    (cond
     ((= 0 retcode)
      (indent-region beg end)
      (goto-char point)
      (beginning-of-line-text))
     (t
      (message "Comby error.")))))

(defun colisper-check-file ()
  "Check the current file with all rules. See the comby diff in a compilation buffer."
  (interactive)
  (let* ((filename (buffer-file-name))
         (cmd (concatenate 'string
                           "comby -config ~/projets/colisper/src/patterns/* -matcher .lisp -f "
                           filename)))
    (message cmd)
    (compile cmd)))

(defun colisper-check-project ()
  "Check the current project."
  (interactive)
  (compile (concatenate 'string
                        "cd "
                        (projectile-project-root)
                        " && "
                        ;; comby finds the files with the required extension itself. Thanks!
                        "comby -config ~/projets/colisper/src/patterns/* -matcher .lisp -f .lisp")))

(defhydra colisper-defun-hydra (:color blue :columns 3)
  "
  Refactor this defun.
  "
  ("p" colisper--remove-print "remove Prints")
  ("d" colisper--format-to-debug "format to Debug")
  ("i" colisper--ifprogn-to-when "If…progn to when"))

(defhydra colisper-file-hydra (:color red :column 3)
  "
  Check or refactor this file.
  "
  ("c" colisper-check-file "Check file")
  ("a" colisper--replace-all "replace All in file"))

(defhydra colisper-project-hydra (:color red :column 3)
  "
  Check or refactor this project.
  "
  ("c" colisper-check-project "Check project"))
