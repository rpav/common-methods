# common-methods

This is a framework for managing method namespaces by working around
generic function congruent lambda lists.  Along with this are a number
of Ruby-inspired methods, because they're illustrative, because
they're pragmatic, and because they establish an idiom.

*If you're a Common Lisp Purist, you already hate this; feel free to
continue on to other projects now.*

If you're sick of writing `(TYPE-ACTION x y z)` methods and wish you
could just write `(ACTION x y z)` without having `x y z` for every
method of `ACTION`, this may be for you.

This **is not** a replacement for CLOS or generic functions.  This
**is not** to "make Lisp more object-oriented."  This **does not** let
you write `(OBJECT METHOD ...`.

This **is** about *brevity* and *clarity*.

# Example

A simple example:

```
(def* x ((x number) (y number))
   (+ x y))

(def* x ((x string) (y string))
   (concatenate 'string x y))

(x 1 :y 1) ;; => 2
(x "foo" :y "bar") ;; => "foobar"

(def* x ((x string))
   (format nil "just X: ~A" x))

(x "foo") ;; => "just X: foo"
```

You should notice a few things going on here:

*  Methods are called with keyword parameters.  This is meant to be
   reminiscent of Smalltalk, but this is Common Lisp---we don't
   do `foo x: x y: y`, we can just do `(x x :y y)`.

*  They're named, but specialized!  Typically, you can't specialize
   keyword parameters to methods.

*  Yes, you use `def` and `def*` ... these are theoretically
   extensible, and they're also brief.

These are still methods of generic functions.  There is no mucking
with MOP or the internals of any implementation, it's just a few
simple macros and some semantics.

# How?

You may have deduced based on the use of keyword parameters that you
can "magically" specialize exactly how this works.

In the above example, `X` is not actually the name of the generic
function.  There are actually two, one called `*X*Y` and one called
`*X`.  In addition to these, there is the regular function `X`, and
the compiler-macro `X`.

`(X a :Y b)` expands to `(*X*Y a b)`, and because this is done in a
compiler macro, it's no slower than the explicit call.  In fact, if
you're in the habit of using keywords, this is probably *faster*,
because (based on my testing with SBCL), calling methods with keyword
parameters is considerably slower than calling those without.

This is the simplest case, but it's the basis for all others; a method
defined with `(def* x (p1 p2 p3))` expands to a method called
`*X*P2*P3`.  With `def*`, the first parameter can be called anything
you'd like, but is the value immediately following `X`.  Also,
*parameter names are in alphabetical order*.

You may be considering this too huge of a hack.  However, consider
that we write things like `(IMAGE-GET-PIXEL-X-Y image x y)` already.
This is essentially doing the same thing, manually.  Also consider
that we generate symbols for things all the time.  Essentially, assume
you've adopted a formal naming system, and Lisp can even write it for
you automatically.

Finally, this saves hard-coding knowledge about types and hierarchy.
For instance, consider having a GUI hierarchy where you have a
`WINDOW`, one might do the following:

```
(window-width window) ;; => ERROR, function not bound, WINDOW-WIDTH
```

Because in this hierarchy, `WINDOW` is also a `DRAWABLE`, and we
should have written `(drawable-width window)`.  Unfortunately we
couldn't just `(defmethod width ...)`, because many other things may
also have a `WIDTH` method---perhaps for font metrics, or
similar---with an entirely different lambda list.  Using individual
packages such as `(drawable:width ...)` or `(font:width ...)` has
essentially the same issue.

But with Common Methods, you can just write `(def width ...)` and be
sure you're not painted into a corner.

# Detailed Usage

## `DEF` and `DEF*`

You may have noticed I have referred to `def` and `def*`, but have
only shown `def*`.  The forms are as follows:

```
(def FUNCTION-NAME SPECIALIZER-NAME {method-qualifiers}*
     COMMON-METHOD-LAMBDA-LIST {additional-qualifiers}*
     [declaration* | documentation] form*)

(def* FUNCTION-NAME {method-qualifiers}*
      COMMON-METHOD-LAMBDA-LIST {additional-qualifiers}*
      [declaration* | documentation] form*)
```

The difference is (purists, hate away), `def` takes a bare specializer
after the function name, and provides an implicit parameter called
`SELF`.  This is pragmatic; we *very* often write methods where the
first parameter is specialized, and the primary object we're talking
about.

```
(def move window (x y)
   (setf (slot-value self 'x) x
         (slot-value self 'y) y))

(move *some-window* :x 50 :y 50)
```

But these are generic functions, and there's not actually a
*receiver*, and in many cases there is no "primary" object; for this
we have `def*`, which allows the first parameter to be specified
explicitly.  So, if you prefer:

```
(def* move ((w window) x y)
   (setf (slot-value w 'x) x) ...
```

Or:

```
(def* compare ((x thing) (:with y thing))
   (some-comparison x y))

(compare *one-thing* :with *another-thing*)
```

There is one other form, which is simply doing either of the
following:

```
(def foo)
(def* foo)
```

This generates the compiler macro (and fallback function) for `FOO`,
which will expand method calls.  This is primarily to *avoid* calling
the fallback, which is comparatively slow, performs string
manipulations, and allocates freely.  You want to avoid the fallback.

## common-method-lambda-list

The lambda-list for a common method is not exactly the same as other
methods:

```
COMMON-METHOD-LAMBDA-LIST ::=
  ({keyword-parameter}* [&rest rest-variable])

keyword-parmeter ::= name | (name specializer) |
                     (:name variable-name [specializer])
```

If `variable-name` differs from `name`, `variable-name` is used in
the body of the form, and `name` is specified as the keyword to the
method.

Due to underlying congruence requirements, a `rest-parameter` is
always implicitly specified as part of the generic function.

One caveat is that `&optional` may not be specified.  However,
you can take optional parameters via `&rest`, or specify alternate
forms:

```
(def* foo (x &rest optional) ...)

(def* foo (x y) ...)
(def* foo (x) ...)
```

Note that with `def*`, at least one `keyword-parameter` must be
specified.  In this case, if a `variable-name` is specified, it is
available in the body, but `name` is not otherwise used.

## additional-qualifiers

There are a few optional things you can specify after the lambda
list.  First, `:export BOOLEAN` can be used to automatically export or
unexport a method.  If you like to manage package exports in-line,
rather than via `defpackage`, this may be helpful:

```
(def* my-method (...) :export t
   ...)
```

This will ensure `my-method` is exported.  If the form is recompiled
with `:export nil`, the symbol will be *unexported*, not merely left.

Another additional qualifier is `:type`, which may be `:common` or
`:method`.  If specified as `:method`, the `def` form will be treated
as a *regular call to defmethod*.  This allows you to use `def` to
define methods that are not Common Methods, if you don't want to see
`defmethod` in your source:

```
(def initialize-instance ((instance some-type) &rest initargs &key &allow-other-keys)
     :type :method
   ...)
```

This may be expanded in the future to allow `def` to define other
forms.

## SETF

You can use `def` and `def*` for `setf` methods.  For example:

```
(def* (setf pixel) ((image image-type) value x y)
  (format t "setting (~A,~A) to ~A" x y value))

(def* (setf pixel) ((image image-type) value (:at point))
  (format t "setting ~A to ~A" point value))

(setf (pixel *some-image* :x 0 :y 1) 42)
;; => setting (0,1) to 42

(setf (pixel *some-image* :at '(4 5)) 42)
;; => setting (4 5) to 42
```

In this case, the `COMMON-METHOD-LAMBDA-LIST` must start with a
*first-specializing-parameter*, and the second must be the
*value-specializing-parameter*; other parameters may follow.  Note
that as per `def*`, if both `name` and `variable-name` are given,
`name` is effectively discarded.

## Protocols and cm-methods

In many cases in Common Lisp, generic functions are used for
*protocol-oriented* design.  Rather than being oriented around a
particular class or object that implements certain methods, a protocol
specifies the methods that need implemented to satisfy it.

Common Methods supports (and encourages!) this.

With Common Methods, there is less concern about namespace collision.
Therefore, a package called `CM-METHODS` is provided to contain
methods by default.  This package is *not* meant to be imported.
Instead, the nickname `M` is given for convenience:

```
(def m::move window (x y) ...)

(m:foo *some-window* :x x :y y)
```

Note that the reader requires you specify the double-colon if the
symbol is not interned and exported from `CM-METHODS`; you should
probably always specify it to be safe.  Methods are automatically
exported if specified into `CM-METHODS`.

`CM-METHODS` does not import symbols from any other package, including
`COMMON-LISP`.  Therefore, any symbol may be defined here, including
ones you otherwise would not be allowed, such as 'm:cons', 'm:+', or
similar.

However, putting methods into `CM-METHODS` should not always be done.
For instance, some projects may wish to specialize methods on Common
Lisp types such as `SEQUENCE` or `SYMBOL`.  Others may wish to define
protocols which can be widely implemented, such as "observable".
These should be defined into packages by their respective owners.

Common Methods considers any method in `CM-METHODS` specializing on a
type defined in `COMMON-LISP` to be reserved for its own definition.

## Calling Methods

There are a number of features for calling methods with Common
Methods.  In the simplest case, a method may be called with keyword
parameters (in any order):

```
(m:move window :x 2 :y 5)
;; - or -
(m:move window :y 5 :x 2)
```

Functions may optionally take `&rest` parameters:

```
(m:write window :x 5 :y 5 :font *some-font*
         "Some text")
```

However, the parsing may be confused if `&rest` parameters begin with
a *keyword*, so there are two "special" keywords which may be used:

```
(def m::set-property window (&rest rest)
   ...)

(m:set-property window :* :x :y :z)

(defvar *props* '(:x :y :z))
(m:set-property window :** *props*)
```

In both cases, `REST` is `'(:x :y :z)`.  Specifying `:*` tells the
parser that keyword arguments are done, and normal `&rest` begins.
However, specifying `:**` makes the call be expanded into an implicit
`APPLY`, where the final parameter is a list.

## define-common-generic

Both `def` and `def*` will appropriately specify `DEFGENERIC`
implicitly if necessary.  However, there are many reasons to write
this manually.  Thus, `DEFINE-COMMON-GENERIC` is provided to easily
write the appropriate form for a common method, while specifying
options to `DEFGENERIC`:

```
(define-common-generic foo (x y)
  (:documentation "A Foo Function")
  ...)
```

This will define the generic function `*FOO*X*Y` with the specified
options.

While it is certainly possible to specify methods here, no special
measures have been taken to support the `common-method-lambda-list`
form.

Also note that it's preferable to avoid special options for methods
defined in `CM-METHODS`.

## dot-methods

I have saved this til now in an effort to emphasize the
namespace-oriented nature and true functionality of Common Methods,
rather than as some ill-conceived notion to "make Common Lisp more
object-oriented".  Common Methods changes nothing about CLOS, merely
adds some automatic naming for brevity and clarity.

So keeping that in mind, dot-methods introduces an *importable*
version of `CM-METHODS`, where all common method functions are
prepended with a period.  If you are allergic to dots, or they're
forbade by your religion, or you just find them ugly, this is entirely
unnecessary.

For instance:

```
(use-package :cm-dot-methods)

(.join '(1 2 3) :with ", ") ;; => "1, 2, 3"
```

Additionally, `def` and `def*` treat symbols starting with a dot as
special, and automatically put them into `CM-METHODS`:

```
(def* .x ((x number) (y number))
   (+ x y))

(.x 1 :y 1) ;; => 2
```

This works equally with `SETF` methods:

```
(def* (setf .pixel) ((image image-type) value x y)
   ...)

(setf (.pixel *im* :x 1 :y 1) pixel-value)
```

That is pretty much all there is to dot methods.  They are an optional
but convenient way to handle common method names in `CM-METHODS`.
They do not apply to common methods defined in other packages.

## Etc.

Common Methods includes definitions for a few other optional features.

* Simplified forms for `(defmethod initialize-instance ...` called
  `definit` and `definit*`, which correspond roughly to `def` and
  `def*`.

* An alternate form for `defclass`, called `defclass*`, where sections
  may be specified as keyword rather than positional parameters.

* A macro form for `MAKE-INSTANCE` called `NEW`, which is both fewer
  letters to type, and quotes the class name (e.g., `(new foo)`).

* An alternate macro for `LAMBDA` called `->`, which looks like an
  arrow, is fewer characters to type, and is meant to be used for
  

* You can import `def`, `def*`, etc, or use them via `cm:def` etc.

* A `NAMED-READTABLES`-based readtable for [], which act like a call
  to `(slot-value self ...)`.  E.g.,

```
(def .move window (x y)
  (setf [x] x
        [y] y))
```

* The beginnings of predefined handling for some CL types, such as
  sequences.
