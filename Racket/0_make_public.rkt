#lang racket

; each file is a module and by default, everything it is private

; for ease when testing with another file, include the below line to
; make everything public

(provide (all-defined-out))