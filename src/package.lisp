(in-package :cl-user)

(defpackage #:cl-state-machine
  (:nicknames :statem)
  (:use :common-lisp)
  (:export

   ;; global
   :non-nil-symbol
   :before-hook-function
   :after-hook-function

   :state-machine-error

   ;; `state-definition'
   :state-definition

   :state
   :description
   :terminal
   :before-hooks
   :after-hooks

   :state-definition-list
   :state-definition-list?

   :state-definitions-of

   :call-before-hooks
   :call-after-hooks

   ;; `transition-definition'
   :transition-definition

   :from-state
   :to-state
   :event
   :description

   :transition-definition-list
   :transition-definition-list?

   :transition-definitions-of

   ;; `state-machine'
   :current-state
   :before-hooks
   :after-hooks
   :state-machine
   :state-machine--state-definitions
   :state-machine--transition-definitions

   :find-state-definition-by-state
   :find-transition-definition-by-state-and-event
   :can?
   :possible-events
   :terminated?
   :trigger!
   :jump!

   :state-machine-of

   ;; `state-transition'
   :state-transition
   :make-state-transition
   :state-transition-p

   :state-transition-args
   :state-transition-transition-definition
   :state-transition-state-machine
   ))
