# colisper: static code checking and refactoring with [Comby](https://comby.dev/).

*defined for Common Lisp, could work for any Lisp*

Status: beta, usable.

Comby makes it easy to match code structures. It can output a diff or
change the code in-place.

We define rules for lisp.

We can call them from our favorite editor (Emacs) during development.

And we can run them as a pre-commit hook or in a CI.

![](colisper-cli.png)

<!-- drop the first elt:
 (custom-set-variables '(markdown-toc-user-toc-structure-manipulation-fn 'cdr))
 -->
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Demo](#demo)
    - [Transform `format t …` to `log:debug`](#transform-format-t--to-logdebug)
    - [Remove any `print`](#remove-any-print)
    - [Rewrite `if … progn` to `when`](#rewrite-if--progn-to-when)
    - [Other rules](#other-rules)
- [Installation](#installation)
- [Run all rules with a script](#run-all-rules-with-a-script)
    - [Run on a project](#run-on-a-project)
- [Emacs integration](#emacs-integration)
    - [Customization](#customization)
- [CHANGELOG](#changelog)
- [TODOs and ideas](#todos-and-ideas)
- [See also:](#see-also)
- [Final words](#final-words)

<!-- markdown-toc end -->


## Demo

Here are my practical use cases.

You can try by cloning the repo and using this comby command:

    colisper tests/playground.lisp

aka

    comby -config ~/path/to/combycl/src/catalog/lisp/base -f tests/playground.lisp

a one-liner with inline rewrite rules looks like:

    comby '(print :[rest])' ':[rest]' tests/playground.lisp

### Transform `format t …` to `log:debug`

We are writing Lisp when suddenly, we want to rewrite some `format` to `log:debug` (or the contrary).

```lisp
(defun product-create-route/post ("/create" :method :post)
  (title price)
  (format t "title is ~a~&" title)
  (format t "price is ~a~&" price)
  (handler-case
      (make-product :title title)
    (error (c)
      (format *error-output* "ooops: ~a" c)))
  (render-template* +product-created.html+ nil))
```

I call `M-x colisper--format-to-debug` (or I use a Hydra to find the rule among others) and I get:


```dif
@@ -226,12 +226,12 @@ Dev helpers:

 (defun product-create-route/post ("/create" :method :post)
   (title price)
-  (format t "title is ~a~&" title)
-  (format t "price is ~a~&" price)
+  (log:debug "title is ~a~&" title)
+  (log:debug "price is ~a~&" price)
   (handler-case
       (make-product :title title)
     (error (c)
-      (format *error-output* "ooops: ~a" c)))
+      (log:debug "ooops: ~a" c)))
   (render-template* +product-created.html+ nil))
```

With Comby:

    comby 'format :[stream] :[rest]' 'log:debug :[rest]' file.lisp

It seems that the search & replace is simple enough and that we don't
leverage Comby's power here. But Comby works easily with multilines,
comments, and it will shine even more when we match s-expressions delimiters.

### Remove any `print`

We are using `print` for debugging purposes when suddenly, our code is
ready for production use.

    M-x colisper--remove-print

```lisp
(push (hunchentoot:create-folder-dispatcher-and-handler
         "/static/" (print (merge-pathnames *default-static-directory*
                                            (asdf:system-source-directory :abstock))))
        hunchentoot:*dispatch-table*)
```

~~~lisp
(push (hunchentoot:create-folder-dispatcher-and-handler
         "/static/" (merge-pathnames *default-static-directory*
                                     (asdf:system-source-directory :abstock)))
        hunchentoot:*dispatch-table*)
~~~


### Rewrite `if … progn` to `when`

Rewrite:

```lisp
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
```

to:

```lisp
(when (and (getf options :version)
           (foo)
           ;; comment (with parens even
           #| nasty comment:
           (if (test) (progn even)))
           |#
           (bar))
  (format t "Project version ~a~&" +version+)
  (print-system-info)
  (uiop:quit))
```

### Other rules

There are two kinds of rules:

- the base ones (`catalog/base/`),
- as well as rules that only make sense for interactive use (`catalog/interactive/`).

Some other available rules:

- rewrite `(equal var nil)` to `(null var)`.
- rewrite `(cl-fad:file-exists-p` or `(fad:file-exists-p` to using `uiop`.
- rewrite `(funcall 'fn args)` to using a `#'` (respect lexical scope).
- remove all `(log:debug …)`
- check that `sort` is followed by `copy-seq` (WIP: we match the simplest expression of the form `(sort variable)`)

You can see `test/playground.lisp` for an overview of all available checks.


## Installation

Clone this repository. You can use an alias to `colisper.sh`:

    alias colisper=~/path/to/colisper/colisper.sh


## Run all rules with a script

    ./colisper.sh [--in-place] [--review] [file.lisp]

By default, only check the rules and print the diff on stdout.

If you don't give files as arguments, run the rules on all .lisp files of the current directory and its subdirectories.

With `--in-place`, write the changes to file (and indent them correctly with emacs).

With `--review` (`comby -review`), interactively accept or reject changes.

It returns 0 (success) if no rules were applied (code is good).

TODO: write a solid script.

### Run on a project

TLDR;

    cd src/ && colisper

This finds all `.lisp` files in subdirectories to run the Colisper rules on them.

Comby understands file extensions:

    comby -config comby.toml -f .lisp

but it doesn't handle wildcards very well, so it's better to `cd` into
your source directory before running Comby/Colisper.

Moreover:

> You can add additional flags, like -i, -exclude, -matcher and so on, as usual.


## Emacs integration

Load `colisper.el`.

Call `colisper-check-file`.

Call a hydra, that gives you the choice of the rule:

- `colisper-hydra/body`: asks wether to work on the current defun, the file, the project.
- `colisper-[defun/file/project]-hydra/body`: act on the current defun/file/project, where the actions can be:
  -`c`heck only: run all rules and display the diff in a compilation buffer,
  - `r`un and apply the replace rule(s).
    - this doesn't show much output, use your VCS (Magit) to see changes.

Or call a rule directly. For example, place the cursor inside a
function and call `M-x colisper--format-to-debug`. It replaces the
function body with the new result.

### Customization

You can customize the path to the catalog directory and use your own set of rules:

    (setq colisper-catalog-path "~/.config/colisper/catalog/")


## CHANGELOG

- <2025-01-18> added `colisper-project-replace-all` and `colisper-hydra/body` that dispatches on defun/file/project.
- initial POC

## TODOs and ideas

- [X] re-indent the file.

Comby doesn't respect indentation on rewriting, so we have to rely on another tool. We currently do with an `emacs --batch` command, and use the built-in `indent-region`.

> What is Comby not good at?

> When talking about matching or changing code, Comby is not well-suited to stylistic changes and formatting like "insert a line break after 80 characters". Pair Comby with a language-specific formatter to preserve formatting (like gofmt for the Go language) after performing a change.

https://comby.dev/docs/faq

- [X] interactively accept or reject changes (comby -review)
  - done with the shell script (use `comby -review`), not on Emacs, but we can use Magit.

- [X] differentiate rules that are made for live refactoring only, and rules for anti-pattern checks. => base/ and interactive/
- [ ] differentiate rules for CL, Elisp and friends.

<a href='https://ko-fi.com/K3K828W0V' target='_blank'><img height='36' style='border:0px;height:36px;' src='https://cdn.ko-fi.com/cdn/kofi2.png?v=2' border='0' alt='Buy Me a Coffee at ko-fi.com' title="Thanks!"/></a>

## See also:

- [trivial-formatter](https://github.com/hyotang666/trivial-formatter)
- [lisp-critic](https://github.com/g000001/lisp-critic/)
- [sblint](https://github.com/cxxxr/sblint)
- [cl-indentify](https://github.com/yitzchak/cl-indentify/)
- [comby.el](https://github.com/s-kostyaev/comby.el/), that asks rules interactively,

<!-- https://github.com/jonase/kibit,
     https://github.com/brunchboy/kibit-helper/blob/master/kibit-helper.el
     clojure tool, nice options. -->

## Final words

This method doesn't know about Lisp internals (the symbols' package and all). Work on SLIME to anable stuff like [this](https://github.com/slime/slime/issues/532) is still needed.

Let's build something useful!

Thanks to Svetlyak40wt for [finding it out](https://github.com/svetlyak40wt/comby-test).

LLGPLv3
