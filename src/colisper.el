;;; colisper.el --- Syntactic checks and refactoring for Lisp code with Comby -*- lexical-binding: t -*-

;; Copyright (C) 2020  wtf public licence

;; Author: vindarel <vindarel@mailz.org>
;; URL:
;; Version: 0.1
;; Keywords: refactoring, static check, comby
;; Package-Requires: ((hydra "0") )

;;; Commentary:

;; Syntactic checks and refactoring. regex-replace on steroids, with Comby.  It can act on the current defun, the current file or a project.
;; You must install `comby' yourself: https://comby.dev/

;;; Code:

;;;
;;; Goals: call Comby on a defun, on the file or on the project.
;;;
;;; TODOs:
;;; - [X] POC
;;; - [X] a hydra to choose defun/file/project and the replacement rule.
;;; - [X] don't hardcode the patterns' path
;;; - [ ] the compilation buffer output is not usable with goto-next-error
;;; - [ ] all the rest.
;;;
;;; Implementation notes:
;;; - shell-command-on-region re-writes the region, even if it's an error message.
;;; - I'm rusted. See https://www.gnu.org/software/emacs/manual/html_node/elisp/Synchronous-Processes.html Use shell-command-to-string?

(require 'hydra)

(defgroup colisper nil
  "colisper uses Comby to check and refactor lisp code against existing rules. It can act on the current defun, the current file or a project."
  :group 'tools)

(defcustom colisper-comby-path "comby"
  "Path to the comby combinary."
  :type 'string
  :group 'colisper)

(defcustom colisper-patterns-path ""
  "Path to the comby rules, for example a directory of .toml files, each containing one or many rules."
  :type 'string
  :group 'colisper)

(defun colisper--create-comby-command (cmd/string &rest args)
  "Prepend the comby path from `colisper-comby-path' to this command (a string), and also concatenate optional parameters (strings)."
  (mapconcat 'identity
             (list colisper-comby-path
                   cmd/string
                   (mapconcat 'identity args " "))
             " "))

(defun colisper--create-rule-path (rule)
  "Concatenate the path to the rules with this rule name (a .toml file, as string), and warn of possible misconfiguration."
  (cond
   ((string-empty-p colisper-patterns-path)
    (warn "The path to the Comby rules appears null. You should set the variable `colisper-patterns-path'.")
    rule)
   (t
    (concat colisper-patterns-path
            (unless (string-suffix-p "/" colisper-patterns-path)
              "/")
            rule))))

(defun colisper--test ()
  (print "Testing colisper--create-comby-command...")
  (unless (assert (string-equal "comby -config /path/to other arg"
                                (colisper--create-comby-command "-config" "/path/to" "other" "arg")))
    (prin1 "ok")
    t))

(defun colisper--format-to-debug ()
  (interactive)
  (let ((point (point))
        (beg (save-excursion
               (beginning-of-defun)
               (point)))
        (end (save-excursion
               (end-of-defun)
               (point)))
        (cmd (colisper--create-comby-command "'format :[stream] :[rest]' 'log:debug :[rest]' -stdin -stdout -matcher .lisp")))
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
        (cmd (colisper--create-comby-command
              "-config" (colisper--create-rule-path "ifprogn-to-when.toml")
              "-matcher" ".lisp"
              "-stdin -stdout")))
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
         (cmd (colisper--create-comby-command
               "-config" (colisper--create-rule-path "remove-print.toml" )
               "-matcher" ".lisp"
               "-stdin -stdout"))
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
         (cmd  (colisper--create-comby-command
                "-config" (colisper--create-rule-path "*")
                "-matcher .lisp"
                "-f" filename)))
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
                        (colisper--create-comby-command
                         "-config" (colisper--create-rule-path "*")
                         "-matcher .lisp"
                         "-f" ".lisp"))))

;;;###autoload
(defhydra colisper-defun-hydra (:color blue :columns 3)
  "
  Refactor this defun.
  "
  ("p" colisper--remove-print "remove Prints")
  ("d" colisper--format-to-debug "format to Debug")
  ("i" colisper--ifprogn-to-when "If…progn to when"))

;;;###autoload
(defhydra colisper-file-hydra (:color red :column 3)
  "
  Check or refactor this file.
  "
  ("c" colisper-check-file "Check file")
  ("a" colisper--replace-all "replace All in file"))

;;;###autoload
(defhydra colisper-project-hydra (:color red :column 3)
  "
  Check or refactor this project.
  "
  ("c" colisper-check-project "Check project"))

(provide 'colisper)
;;;colisper.el ends here
