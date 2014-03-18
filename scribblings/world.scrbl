#lang scribble/manual

@(require scribble/eval pollen/cache pollen/world (for-label racket (except-in pollen #%module-begin) pollen/world))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/world))

@title{World}

@defmodule[pollen/world]

A set of global values and parameters that are used throughout the Pollen system. So if you don't like the defaults I've picked, you can change them.
