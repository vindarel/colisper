Structural refactoring recipes for Common Lisp with [Comby](https://comby.dev/).

Proof of Work. Not ready for use other than by myself.

Comby makes it easy to match code structures.

We call it from our favorite editor (Emacs), which we use to finish
cleaning the replacements.

## Demo

Here are my practical use cases.

### Transformat `format t …` to `log:debug`

We are writing Lisp when suddenly, we want to rewrite some `format` to `log:debug`.

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

Install comby and call

    comby 'format :[stream] :[rest]' 'log:debug :[rest]' file.lisp


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

Note that we might actually want to keep the `(format *error-output*…)` as is.

It seems we don't leverage Comby's power here. But this rule still
works with multilines:

```lisp
(defun product-create-route/post ("/create" :method :post)
  (title price)
  (format
   t
   "title is ~a~&"
   title)
  (format
   t
   "price is ~a~&"
   price)
  (handler-case
      (make-product :title title)
    (error (c)
      (format *error-output*
              "ooops: ~a"
              c)))
  (render-template* +product-created.html+ nil))
```

```diff
------ foo.lisp
++++++ foo.lisp
@|-6,18 +6,13 ============================================================
 |
 |(defun product-create-route/post ("/create" :method :post)
 |  (title price)
-|  (format
-|   t
-|   "title is ~a~&"
+|  (log:debug "title is ~a~&"
-|   title)
-|  (format
-|   t
-|   "price is ~a~&"
+|   title)
+|  (log:debug "price is ~a~&"
 |   price)
 |  (handler-case
 |      (make-product :title title)
 |    (error (c)
-|      (format *error-output*
-|              "ooops: ~a"
+|      (log:debug "ooops: ~a"
 |              c)))
 |  (render-template* +product-created.html+ nil))
```

And Comby will shine more when we have to span s-expressions.

## Remove any `print`

We are using `print` for debugging purposes when suddenly, our code is
ready for production use.

    M-x combycl--remove-print

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


## Rewrite `if … progn` to `when`

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
  (uiop:quit)
  )
```

(todo: indent correctly the last parenthesis)

## Emacs integration

Place the cursor inside the function and call `M-x combycl--format-to-debug`.

It replaces the function body with the new result.

# Difficulties

Rewriting doesn't respect indentation, so re-writing multiple lines is problematic.

https://comby.dev/docs/faq

> What is Comby not good at?

> When talking about matching or changing code, Comby is not well-suited to stylistic changes and formatting like "insert a line break after 80 characters". Pair Comby with a language-specific formatter to preserve formatting (like gofmt for the Go language) after performing a change.

So we'll use our editor. Emacs has `indent-region`.

Also, pair with https://github.com/hyotang666/trivial-formatter ?


## Final words

This method doesn't know about Lisp internals (the symbols' package and all). Work on SLIME to anable stuff like [this](https://github.com/slime/slime/issues/532) is still needed.

Let's build something useful!

Thanks to Svetlyak40wt for [finding it out](https://github.com/svetlyak40wt/comby-test).

See also https://github.com/s-kostyaev/comby.el/, that asks rules interactively.
