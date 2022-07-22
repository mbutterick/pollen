#lang scribble/manual

@(require scribble/eval pollen/render pollen/setup (for-label racket (except-in pollen #%module-begin) pollen/setup sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/file))


@title{License & source code}

This module is licensed under the MIT License.

Source repository at @link["https://git.matthewbutterick.com/mbutterick/pollen/"]{https://git.matthewbutterick.com/mbutterick/pollen/}. Suggestions & corrections welcome.
