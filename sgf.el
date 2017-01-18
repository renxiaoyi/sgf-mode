;;; sgf.el --- SGF GO back end

;; Copyright (C) 2012-2017 Free Software Foundation, Inc.

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Created: 2012-05-15
;; Version: 0.1
;; Keywords: game go sgf
;; Package-Requires: ((emacs "24"))
;; URL: http://eschulte.github.io/el-go/

;; Last modified by Xiaoyi Ren in Jan. 2017

;; This software is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; Code:
(require 'eieio)

(defun rcons (x lst)
  (append lst (list x)))

(defmacro rpush (x place)
  "Insert X at the back of the list stored in PLACE."
  (if (symbolp place) (list 'setq place (list 'rcons x place))
    (list 'callf2 'rcons x place)))

(defun sgf-ref (sgf index)
  "Returns the move in sgf pointed by index.
Example: (sgf-ref '(0 1 2 (a3 a4) (b3 b4 b5)) '(3 1)) => a4."
  (let ((part sgf))
    (while (car index)
      (setq part (nth (car index) part))
      (setq index (cdr index)))
    part))

(defun set-sgf-ref (sgf index new)
  (eval `(setf ,(reduce (lambda (acc el) (list 'nth el acc))
                        index :initial-value 'sgf)
               ',new)))

(defsetf sgf-ref set-sgf-ref)

(defun sgf-from-file (file)
  (interactive "f")
  (make-instance 'sgf :self (sgf2el-file-to-el file)))

(defun sgf-to-file (sgf file)
  (interactive "F")
  (when (and (file-exists-p file)
             (not (y-or-n-p (format "overwrite %s? " file))))
    (error "aborted"))
  (with-temp-file file
    (delete-region (point-min) (point-max))
    (insert (pp (self sgf)))))  ; todo: implement el2sgf

(defun variation-p (node)
  "Returns t if node is a variation line, otherwise nil.
Example:
  (((:B :pos 13 . 3))((:W :pos 17 . 3))((:B :pos 16 . 2))) => t
  ((:B :pos 15 . 3)) => nil
  ((:B :pos 16 . 3) (:LB ((:label . \"1\") (:pos 16 . 3))) (:C . \"Lee Sedol played Black.\")) => nil"
  (cond ((not (listp node)) nil)
        ((not (listp (car node))) nil)
        ((not (listp (caar node))) nil)
        (t t)))  ; is move-type necessary?


;;; Class
(defclass sgf nil
  ((self  :initarg :self  :accessor self  :initform nil)
   (index :initarg :index :accessor index :initform (list 0)))
  "Class for the SGF back end.")

(defmethod current ((sgf sgf))
  "Finds the current move."
  (sgf-ref (self sgf) (index sgf)))

(defun set-current (sgf new)
  (setf (sgf-ref (self sgf) (index sgf)) new))

(defsetf current set-current)

(defmethod root ((sgf sgf))
  (sgf-ref (self sgf) '(0)))

(defun set-root (sgf new)
  (if (self sgf)
      (setf (car (self sgf)) new)
    (setf (self sgf) (list new))))

(defsetf root set-root)

(defmethod next-moves ((sgf sgf))
  "Finds the next move(s) without changing the index."
  (let ((index (copy-list (index sgf))))
    (incf (car (last index)))
    (let ((node (sgf-ref (self sgf) index)) (ret))
      (if (not (variation-p node))
          (list (car node))  ; removes non-position properties like comments
        (rpush (caar node) ret)
        (incf (car (last index)))
        (while (setq node (sgf-ref (self sgf) index))
          (rpush (caar node) ret)
          (incf (car (last index))))
        ret))))

(defmethod next ((sgf sgf) branch)
  "Updates sgf.index to point to the next move."
  (incf (car (last (index sgf))))  ; increments the last element
  (if (variation-p (current sgf))
      (progn
        (nconc (index sgf) (list 0))
        (incf (car (last (index sgf) 2)) branch))))  ; jumps to the given branch

(defmethod prev ((sgf sgf))
  (if (equal (index sgf) '(0))
      (message "sgf: no prev moves")
    (if (= 0 (car (last (index sgf))))
        (setf (index sgf) (butlast (index sgf))))
    (decf (car (last (index sgf))))
    (while (and (> (car (index sgf)) 0)
                (variation-p (current sgf)))  ; in variation, should go back to root
      (decf (car (last (index sgf)))))))


;;; interface
(defmethod go-size ((sgf sgf))
  (or (aget (root sgf) :S)
      (aget (root sgf) :SZ)
      19))

(defmethod set-go-size ((sgf sgf) size)
  (cond
   ((aget (root sgf)  :S) (setf (cdr (assoc  :S (root sgf))) size))
   ((aget (root sgf) :SZ) (setf (cdr (assoc :SZ (root sgf))) size))
   (t                     (push (cons :S size) (root sgf)))))

(defmethod go-name ((sgf sgf))
  (or (aget (root sgf) :GN)
      (aget (root sgf) :EV)))

(defmethod set-go-name ((sgf sgf) name)
  (cond
   ((aget (root sgf) :GN) (setf (cdr (assoc :GN (root sgf))) name))
   ((aget (root sgf) :EV) (setf (cdr (assoc :EV (root sgf))) name))
   (t                     (push (cons :GN name) (root sgf)))))

(defmethod go-move ((sgf sgf) branch)
  (next sgf branch)
  (let ((turn (current sgf)))
    (if turn
        (or (assoc :B turn) (assoc :W turn))
      (message "sgf: no more moves")
      (prev sgf))))

;; TODO: currently this only works with linear sgf files w/o alternatives
(defmethod set-go-move ((sgf sgf) move)
  (next sgf)
  (if (current sgf)
      (setf (current sgf) (list move))
    (setf (self sgf) (rcons (list move) (self sgf)))))

(defmethod go-labels ((sgf sgf))
  (let ((turn (current sgf)))
    (if turn
        (remove-if-not (lambda (pair) (member (car pair) '(:LB :LW))) turn)
      (message "sgf: no more moves")
      (prev sgf))))

(defmethod set-go-lables ((sgf sgf) labels)
  (if (current sgf)
      (setf (current sgf) (cons (or (assoc :B (current sgf))
                                    (assoc :W (current sgf)))
                                labels))
    (rpush labels (sgf-ref (self sgf) (butlast (index sgf))))))

(defmethod go-comment ((sgf sgf))
  (aget (current sgf) :C))

(defmethod set-go-comment ((sgf sgf) comment)
  (if (aget (current sgf) :C)
      (setf (cdr (assoc :C (current sgf))) comment)
    (push (cons :C comment) (current sgf))))

;; non setf'able generic functions
(defmethod go-undo ((sgf sgf))
  (prev sgf))

(provide 'sgf)
;;; sgf.el ends here
