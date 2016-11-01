;;; go-board.el --- Smart Game Format GO board visualization

;; Copyright (C) 2012-2013 Free Software Foundation, Inc.

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Created: 2012-05-15
;; Version: 0.1
;; Keywords: game go sgf

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

;;; Code:
(require 'go-util)
(require 'go-board-faces)

(defvar *history*  nil "Holds the board history for a GO buffer.")
(defvar *size*     nil "Holds the board size.")
(defvar *turn*     nil "Holds the color of the current turn.")
(defvar *black*    nil "Plist of info on black player.")
(defvar *white*    nil "Plist of info on white player.")
(defvar *sgf*      nil "The sgf object representing the board.")
(defvar *sgf-file* nil "Path to the current sgf file.")

(defvar black-piece "X")
(defvar white-piece "O")

(defvar go-board-use-images t)
(defvar *go-board-overlays* nil
  "List of overlays carrying GO board painting information.")

;;; Board manipulation functions
(defun make-board (size) (make-vector (* size size) nil))

(defun board-size (board) (round (sqrt (length board))))

(defun go-player-get (color property)
  (plist-get (case color (:W *white*) (:B *black*)) property))

(defun go-player-set (color property value)
  (let ((player (case color (:W *white*) (:B *black*))))
    (plist-put player property value)))

(defsetf go-player-get go-player-set)

(defun move-type (move)
  (cond
   ((member (car move) '(:B  :W))  :move)
   ((member (car move) '(:LB :LW)) :label)))

(defun other-color (color)
  (if (equal color :B) :W :B))

(defun point-of-pos (pos)
  (catch 'found-pos
    (dotimes (p (1- (point-max)) (error "point-of-pos: pos %S not found" pos))
      (let ((pos-at-point (get-text-property (1+ p) :pos)))
        (when (and pos-at-point (tree-equal pos pos-at-point))
          (throw 'found-pos (1+ p)))))))

(defun apply-turn-to-board (moves)
  (let ((board (pieces-to-board (car *history*) *size*)))
    (clear-labels board)
    (dolist (move moves) (apply-move board move))
    (push (board-to-pieces board) *history*)
    (update-display (current-buffer))))

(defun apply-move (board move)
  (cl-flet ((bset (val data)
               (let ((data (if (listp (car data)) data (list data))))
                 (setf (aref board (pos-to-index (aget data :pos)
                                                 (board-size board)))
                       (case val
                         (:B  :B)
                         (:W  :W)
                         (:LB (aget data :label))
                         (:LW (aget data :label))
                         (t nil))))))
    (case (move-type move)
      (:move
       (bset (car move) (cdr move))
       (let ((color (if (equal :B (car move)) :B :W)))
         (remove-dead board (other-color color))
         (remove-dead board color)))
      (:label
       (dolist (data (cdr move)) (bset (car move) data))))))

(defun clear-labels (board)
  (dotimes (point (length board) board)
    (when (aref board point)
      (unless (member (aref board point) '(:B :W))
        (setf (aref board point) nil)))))

(defun neighbors (board piece)
  (let ((size (board-size board))
        neighbors)
    (when (not (= (mod piece size) (1- size))) (push (1+ piece) neighbors))
    (when (not (= (mod piece size) 0))         (push (1- piece) neighbors))
    (when (< (+ piece size) (length board))    (push (+ piece size) neighbors))
    (when (> (- piece size) 0)                 (push (- piece size) neighbors))
    neighbors))

(defun alive-p (board piece &optional already)
  (let* ((val (aref board piece))
         (enemy (other-color val))
         (neighbors (remove-if (lambda (n) (member n already))
                               (neighbors board piece)))
         (neighbor-vals (mapcar (lambda (n) (aref board n)) neighbors))
         (friendly (delete nil (mapcar
                                (lambda (n) (when (equal (aref board n) val) n))
                                neighbors)))
         (already (cons piece already)))
    (or (some (lambda (v) (not (or (equal v enemy) ; touching open space
                                   (equal v val))))
              neighbor-vals)
        (some (lambda (n) (alive-p board n already)) ; touching alive dragon
              friendly))))

(defun remove-dead (board color)
  ;; must remove one color at a time for ko situations
  (let (cull)
    (dotimes (n (length board) board)
      (when (and (equal (aref board n) color) (not (alive-p board n)))
        (push n cull)))
    (incf (go-player-get (other-color color) :prisoners) (length cull))
    (dolist (n cull cull) (setf (aref board n) nil))))

(defun board-to-pieces (board)
  (let (pieces)
    (dotimes (n (length board) pieces)
      (let ((val (aref board n)))
        (when val (push (cons val n) pieces))))))

(defun pieces-to-board (pieces size)
  (let ((board (make-vector (* size size) nil)))
    (dolist (piece pieces board)
      (setf (aref board (cdr piece)) (car piece)))))


;;; Visualization
(defun board-header (board)
  (cl-flet ((hd (str hd)
             (put-text-property 0 1 :type `(,hd . :offboard) str)
             str))
    (let ((size (board-size board)))
      (concat "   "
              (hd " " :filler)
              (mapconcat (lambda (n)
                           (let ((char (+ ?A n)))
                             (when (>= char ?I) (setq char (+ 1 char)))
                             (hd (string char) :header)))
                         (range size) (hd " " :filler))))))

(defun board-pos-to-string (board pos)
  (let ((size (board-size board)))
    (cl-flet ((emph (n)
                 (cond
                  ((= size 19)
                   (or (= 3 n)
                       (= 4 (- size n))
                       (= n (/ (- size 1) 2))))
                  ((= size 13)
                   (or (= 3 n)
                       (= 9 n)))
                  ((= size 9)
                   (or (= 2 n)
                       (= 6 n)))))
           (put (str prop val) (put-text-property 0 (length str) prop val str)))
      (let* ((val (aref board (pos-to-index pos size)))
             (str (cond
                   ((equal val :W) white-piece)
                   ((equal val :B) black-piece)
                   ((and (stringp val) (= 1 (length val)) val))
                   (t  (if (and (emph (car pos)) (emph (cdr pos))) "+" ".")))))
        (put str :type
             (cons (cond ;; foreground
                    ((string= str white-piece) :white)
                    ((string= str black-piece) :black)
                    ((string= str "+")         :hoshi)
                    ((string= str ".")         :background-1)
                    (t                         :background))
                   (cond ;; background
                    ((and (= 0 (car pos)) (= 0 (cdr pos)))                 :bl)
                    ((and (= 0 (car pos)) (= (1- size) (cdr pos)))         :br)
                    ((and (= (1- size) (car pos)) (= 0 (cdr pos)))         :tl)
                    ((and (= (1- size) (car pos)) (= (1- size) (cdr pos))) :tr)
                    ((= 0 (car pos))                                       :b)
                    ((= (1- size) (car pos))                               :t)
                    ((= 0 (cdr pos))                                       :l)
                    ((= (1- size) (cdr pos))                               :r)
                    (t nil))))
        (put str :pos (cons (cdr pos) (car pos)))
        str))))

(defun board-row-to-string (board row)
  (let* ((size (board-size board))
         (label (format "%3d" (1+ row)))
         (row-body "")
         (filler " "))
    (put-text-property 0 1 :type (cons :background nil) filler)
    (dotimes (n size)
      (setq row-body
            (concat row-body
                    (board-pos-to-string board (cons row n))
                    filler)))
    (concat label " " (substring row-body 0 (1- (length row-body))) label)))

(defun board-body-to-string (board)
  (let ((board (transpose-array board)))
    (mapconcat (lambda (m) (board-row-to-string board m))
               (reverse (range (board-size board))) "\n")))

(defun board-to-string (board)
  (let ((header (board-header board))
        (body   (board-body-to-string board)))
    (mapconcat #'identity (list header body header) "\n")))

(defun go-board-paint (&optional start end)
  (interactive "r")
  (cl-flet ((ov (point face &optional back)
             (let ((ovly (make-overlay point (1+ point))))
               (overlay-put ovly 'go-pt point)
               (overlay-put ovly 'face (sym-cat 'go-board face))
               (when go-board-use-images
                 (overlay-put ovly 'display
                              (if (equal face 'filler)
                                  '(space :width (18))
                                (eval (sym-cat 'go-board 'image face back)))))
               (push ovly *go-board-overlays*)))
         (hide (point)
               (let ((ovly (make-overlay point (1+ point))))
                 (overlay-put ovly 'invisible t)
                 (push ovly *go-board-overlays*))))
    (let ((start (or start (point-min)))
          (end   (or end   (point-max))))
      (dolist (point (range start end))
        (if (get-text-property point :turn)
            (font-lock-prepend-text-property point (1+ point) 'face 'underline)
          (let ((back (case (cdr (get-text-property point :type))
                        (:tl 'top-left)
                        (:tr 'top-right)
                        (:bl 'bottom-left)
                        (:br 'bottom-right)
                        (:t  'top)
                        (:b  'bottom)
                        (:l  'left)
                        (:r  'right)
                        (:offboard 'offboard))))
            (case (car (get-text-property point :type))
              (:header       nil)
              (:filler       (ov point 'filler back))
              (:hoshi        (ov point 'hoshi))
              (:white        (ov point 'white back))
              (:black        (ov point 'black back))
              (:background  (if go-board-use-images
                                (hide point)
                              (ov point 'background)))
              (:background-1 (ov point 'background back)))))))))

(defun player-to-string (color)
  (format "%10s: %3d"
          (let ((name (go-player-get color :name)))
            (put-text-property 0 (length name) :turn (equal *turn* color) name)
            name)
          (go-player-get color :prisoners)))

(defun update-display (buffer)
  (with-current-buffer buffer
    (let ((point (point)))
      (delete-region (point-min) (point-max))
      (insert "\n"
              (board-to-string
               (pieces-to-board (car *history*) *size*)) "\n\n"
              (player-to-string :W) "\n"
              (player-to-string :B) "\n")
      (let ((comment (go-comment *sgf*)))
        (when comment
          (insert (make-string (+ 6 (* 2 *size*)) ?=)
                  "\n\n"
                  comment)))
      (go-board-paint)
      (goto-char point)))
  buffer)

(defun go-board (sgf)
  (let ((buffer (generate-new-buffer "*GO*")))
    (go-board-in-buffer sgf buffer)))

(defun go-board-in-buffer (sgf buffer)
  (with-current-buffer buffer
    (go-board-mode)
    (let ((name (go-name sgf)))
      (when name
        (rename-buffer (ear-muffs name) 'unique)))
    (set (make-local-variable '*sgf*) sgf)
    (set (make-local-variable '*turn*) :B)
    (set (make-local-variable '*black*) '(:name "black" :prisoners 0))
    (set (make-local-variable '*white*) '(:name "white" :prisoners 0))
    (set (make-local-variable '*size*) (go-size sgf))
    (set (make-local-variable '*go-board-overlays*) nil)
    (set (make-local-variable '*history*)
         (list (board-to-pieces (make-board *size*)))))
  (pop-to-buffer buffer)
  (plist-put *black* :prisoners 0)
  (plist-put *white* :prisoners 0)
  (setq truncate-lines t)
  (update-display buffer))


;;; User input
(defvar go-board-actions '(move resign undo comment)
  "List of actions which may be taken on an GO board.")

(defun go-board-act ()
  "Send a command to the current GO board."
  (interactive)
  (let ((command (go-completing-read
                  "Action: " (mapcar #'symbol-name go-board-actions))))
    (case (intern command)
      (move    (message "make a move"))
      (resign  (message "game over"))
      (undo    (message "loser"))
      (comment (message "what?")))))

(defun go-board-move (&optional pos)
  (interactive)
  (let* ((color (case *turn* (:B "black") (:W "white")))
         (pos (or pos (cons (char-to-num
                             (aref (downcase
                                    (go-completing-read
                                     (format "[%s] X pos: " color)
                                     (mapcar #'string
                                             (mapcar #'gtp-num-to-char
                                                     (range 1 *size*)))))
                                   0))
                            (1- (string-to-number
                                 (go-completing-read
                                  (format "[%s] Y pos: " color)
                                  (mapcar #'number-to-string
                                          (range 1 *size*))))))))
         (move (cons *turn* (cons :pos pos))))
    (setf (go-move *sgf*) move)
    (setf *turn* (other-color *turn*))
    (apply-turn-to-board (list move))))

(defun go-board-refresh ()
  (interactive)
  (update-display (current-buffer)))

(defun go-board-resign ()
  (interactive)
  (go-resign *sgf*))

(defun go-board-mark-point (point mark)
  (mapc (lambda (ov) (go-board-mark ov mark)) (overlays-at point)))

(defun go-board-pass ()
  (interactive)
  (go-pass *sgf*)
  (save-window-excursion
    (setf *turn* (other-color *turn*))
    (when (equalp :pass (go-board-next))
      ;; mark open points
      (mapc (lambda (move)
              (go-board-mark-point (point-of-pos (cddr move))
                                   (go-board-cross (ecase (car move)
                                                     (:B 'black)
                                                     (:W 'white)))))
            (go-territory *sgf*))
      ;; mark dead stones
      (mapc (lambda (move)
              (let* ((point (point-of-pos (cddr move)))
                     (color (car (get-text-property point :type))))
                (go-board-mark-point point
                                     (go-board-cross (ecase color
                                                       (:black 'white)
                                                       (:white 'black))))))
            (go-dead *sgf*))
      (message "final score: %s" (go-score *sgf*)))))

(defun go-board-undo (&optional num)
  (interactive "p")
  (go-undo *sgf*)
  (pop *history*)
  (update-display (current-buffer))
  (setf *turn* (other-color *turn*))
  (go-board-show-next))

(defun go-board-comment (&optional comment)
  (interactive "MComment: ")
  (setf (go-comment *sgf*) comment))

(defun go-board-level (&optional level)
  (interactive "nLevel: ")
  (setf (go-level *sgf*) level))

(defun go-board-next (&optional count)
  (interactive "p")
  (let (move)
    (dotimes (n (or count 1) move)
      (setf move (go-move *sgf*))
      (if (equal move :pass)
          (message "pass")
        (setf *turn* (other-color *turn*))
        (apply-turn-to-board
         (cons move (go-labels *sgf*))))
      (if (equal move :pass)
          (goto-char (point-min))
        (goto-char (point-of-pos (cddr move))))))
  (go-board-show-next))

(defun go-board-show-next ()
  (let ((char 97))  ; "a"
    (dolist (move (next-moves *sgf*))
      (go-board-mark-point
       (point-of-pos (cddr move))
       (go-board-label
        (ecase (car move) (:B 'black) (:W 'white))
        (char-to-string char)))
      (incf char))))

(defun go-board-mouse-move (ev)
  (interactive "e")
  (go-board-move (get-text-property (posn-point (event-start ev)) :pos)))

(defun go-board-quit ()
  (interactive)
  (when (y-or-n-p "quit: ")
    (kill-buffer (current-buffer))))

(defun go-board-safe-quit ()
  (ignore-errors (go-quit *sgf*))
  t)


;;; Display mode
(defvar go-board-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") 'go-board-mouse-move)
    (define-key map (kbd "m") 'go-board-move)
    (define-key map (kbd "r") 'go-board-refresh)
    (define-key map (kbd "R") 'go-board-resign)
    (define-key map (kbd "u") 'go-board-undo)
    (define-key map (kbd "c") 'go-board-comment)
    (define-key map (kbd "l") 'go-board-level)
    (define-key map (kbd "p") 'go-board-pass)
    (define-key map (kbd "<right>") 'go-board-next)
    (define-key map (kbd "<left>")  'go-board-undo)
    (define-key map (kbd "q") 'go-board-quit)
    map)
  "Keymap for `go-board-mode'.")

(define-derived-mode go-board-mode nil "GO"
  "Major mode for viewing a GO board."
  (set (make-local-variable 'kill-buffer-query-functions)
       (add-to-list 'kill-buffer-query-functions 'go-board-safe-quit)))

(provide 'go-board)
;;; go-board.el ends here
