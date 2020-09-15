Structural refactoring recipes for Common Lisp with [Comby](https://comby.dev/).

Proof Of Work. Not much here.

## Demo

Comby makes it easy to match code structures.

Here's my practical use case.

### `format` to `log:debug`

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

Anyways, we can do this from our favorite editor.


## Emacs integration

Place the cursor inside the function and call `M-x combycl--format-to-debug`.

It replaces the function body with the new result.


## Final words

This method doesn't know about Lisp internals (the symbols' package and all). Work on SLIME to anable stuff like [this](https://github.com/slime/slime/issues/532) is still needed.

Let's build something useful!

Thanks to Svetlyak40wt for [finding it out](https://github.com/svetlyak40wt/comby-test).

See also https://github.com/s-kostyaev/comby.el/, that asks rules interactively.
