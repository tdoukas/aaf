# AAF (Abstract Argumentation Frameworks)

This repository contains a set of Lisp code designed for the generation of Abstract Argumentation Frameworks (AAFs), computation of their extensions, and searching for frameworks and extensions with specific properties.

## Features

- **Generate AAFs**: Tools to create Abstract Argumentation Frameworks from scratch.
- **Compute Extensions**: Implementations of various semantics for computing extensions of AAFs.
- **Search for Specific Properties**: Methods for searching AAFs and their extensions for desired properties.
  
## Requirements

- **Lisp**: The code is written in Common Lisp, and thus requires a Common Lisp implementation (e.g., SBCL, CLISP). Some familiarity with the lisp ecosystem is required.

## Getting Started

- **Loading**: An ASDF system definition is provided in `aaf.asd`. See e.g. <https://www.sbcl.org/asdf/Using-asdf-to-load-systems.html> how to load the code.
- **Framework Construction**: See `framework.lisp` for functions that create and operate on frameworks, and `semantics.lisp` for functions that compute admissible sets, extensions, etc. For example
```lisp
;; Create a framework
(make-aaf '(a b c) '((a b) (b c)))
  => #<AAF :arguments (A B C) :attacks ((A B) (B C))>

;; Compute the admissible sets
(admissible-extensions *)   
  => (NIL (A) (A C))
```
- **Search for Frameworks**: The following searches for a framework that has `((a b) (c))` as its preferred extensions, using 1000 iterations on a single thread. See the function definition in `explore.lisp` for more options (e.g., number of arguments / attacks).
```lisp
(find-framework-with-extensions :es '((a b) (c)) :semantics #'preferred-extensions :num-threads 1 :num-repeats 1000)
```
- **Rendering Frameworks**: The code contains a spring embedder that renders frameworks as tikz images, e.g.
```lisp
(output-embedding (make-aaf '(a b c) '((a b) (b c))))
```
The rendering will be output to `fig.tex`.

## License

This project is licensed under the MIT License (see the file LICENSE.txt).
