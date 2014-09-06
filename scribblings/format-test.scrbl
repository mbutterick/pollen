#lang scribble/manual

@(require "mb-tools.rkt")

@fileblock["hello" @bold{world}]


@codeblock|{
#lang pollen

◊define[greeting]{Plain @codeblock.}.
}|


@filebox["foo.html"]{
@codeblock|{
#lang pollen

◊define[greeting]{@codeblock wrapped in @filebox}.
}|}


@racketblock[
(define greeting
  "Plain @racketblock")
]

@filebox["foo.html"]{
@racketblock[
(define greeting
  "@racketblock wrapped in @filebox")
]}

@racketmod[racket/base
(define greeting
  "Plain @racketmod")
]

@racketmod[#:file "foo.html" racket/base
(define greeting
  "@racketmod with #:file argument")
]


@filebox["foo.html"]{
Plain #filebox
}