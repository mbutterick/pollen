#lang scribble/manual

@(require scribble/eval (for-label racket (except-in pollen #%module-begin) pollen/world pollen/command))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen))

@section{Command}

@defmodule[pollen/command]

This module defines functions that are made accessible through @racket[raco], specifically by invoking @racket[raco pollen _command].