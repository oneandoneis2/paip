;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File gps1.lisp: First version of GPS (General Problem Solver)

(load "auxfns.lisp")

(defvar *ops* nil "A list of available operators.")

(defstruct op "An operation"
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun GPS (state goals &optional (*ops* *ops*))
  "General Problem Solver: from state, achieve all goals using *ops*."
  (find-all-if #'action-p (achieve-all (cons '(start) state) goals nil)))

(defun action-p (x)
  (or (equal x '(start)) (executing-p x)))

(defun achieve-all (state goals goal-stack)
  "Try to achieve each goal, in multiple orders"
  (some #'(lambda (goals) (achieve-each state goals goal-stack))
        (orderings goals)))

(defun achieve-each (state goals goal-stack)
  "Achieve each goal, and make sure they all still hold at the end"
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
      current-state)))

(defun orderings (l)
  (if (> (length l) 1)
    (list l (reverse l))
    (list l)))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (find-all goal *ops* :test #'appropriate-p)))))

(defun member-equal (item list)
  (member item list :test #'equal))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member goal (op-add-list op)))

(defun apply-op (state goal op goal-stack)
  "Return a new state if op is applicable"
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op)
                             (cons goal goal-stack))))
    (unless (null state2)
      (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
      (append (remove-if #'(lambda (x)
                             (member-equal x (op-del-list op)))
                         state2)
              (op-add-list op)))))

(defun appropriate-p (goal op)
  "An op is approrpaite to a goal if the goal is in the add list"
  (member-equal goal (op-add-list op)))

(defun use (oplist)
  "Use specified oplist. Return length because reasons"
  (length (setf *ops* oplist)))

(defun executing-p (x)
  (starts-with x 'executing))

(defun convert-op (op)
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  (convert-op (make-op :action action :preconds preconds
                       :add-list add-list :del-list del-list)))

;;; ==============================

(defparameter *school-ops*
  (list
    (make-op :action 'drive-son-to-school
             :preconds '(son-at-home car-works)
             :add-list '(son-at-school)
             :del-list '(son-at-home))
    (make-op :action 'shop-installs-battery
             :preconds '(car-needs-battery shop-knows-problem shop-has-money)
             :add-list '(car-works))
    (make-op :action 'tell-shop-problem
             :preconds '(in-communication-with-shop)
             :add-list '(shop-knows-problem))
    (make-op :action 'telephone-shop
             :preconds '(know-phone-number)
             :add-list '(in-communication-with-shop))
    (make-op :action 'look-up-number
             :preconds '(have-phone-book)
             :add-list '(know-phone-number))
    (make-op :action 'give-shop-money
             :preconds '(have-money)
             :add-list '(shop-has-money)
             :del-list '(have-money))))

; (gps '(son-at-home car-needs-battery have-money have-phone-book) '(son-at-school) *school-ops* )
;
; (EXECUTING LOOK-UP-NUMBER)
; (EXECUTING TELEPHONE-SHOP)
; (EXECUTING TELL-SHOP-PROBLEM)
; (EXECUTING GIVE-SHOP-MONEY)
; (EXECUTING SHOP-INSTALLS-BATTERY)
; (EXECUTING DRIVE-SON-TO-SCHOOL)
; SOLVED
;
; (gps '(son-at-home car-needs-battery have-money ) '(son-at-school) *school-ops* )
; NIL
;
; (gps '(son-at-home car-works ) '(son-at-school) *school-ops* )
;
; (EXECUTING DRIVE-SON-TO-SCHOOL)
; SOLVED
;
