;;; go-board-mode.el --- Smart Game Format (SGF) GO board visualization

;; Usage:
;;
;; Add following lines to .emacs:
;;   (add-to-list 'load-path "/path/to/go-board-mode")
;;   (require 'go-board-mode)
;;
;; Open foo.sgf (e.g. go-board-mode/alphago.sgf) in go-board-mode:
;;   M-x go-open-sgf RET [then complete the path to foo.sgf when prompt]
;;
;; Or first open (C-x C-f) /path/to/foo.sgf, then
;;   M-x go-load-sgf RET
;;
;; Toggle guess-move mode (inspired by Android APP "Go Dojo"):
;;   M-x go-toggle-guess-move-mode
;;
;; Enter: one move forward.
;; Space: one move back.
;; Letter keys "a"-"n": navigate through variation branches.
;; "q" to bury the buffer, "Q" to kill the buffer, "R" to reload.

;; Copyright (C) 2012-2017 Free Software Foundation, Inc.

;; Creator: Eric Schulte <schulte.eric@gmail.com>
;; Created: 2012-05-15
;; Version: 0.1
;; Keywords: game go sgf
;; Package-Requires: ((emacs "24"))
;; URL: http://eschulte.github.io/el-go/
;;
;; Last modified by Xiaoyi Ren in Feb. 2017

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
(require 'assoc)
(require 'go-board-faces)
(require 'sgf)
(require 'sgf2el)

(defvar *history* nil "Holds the move history.")
(defvar *label-history*  nil "Holds the label history.")
(defvar *black-prisoner-history* nil "Holds the prioner history for black.")
(defvar *white-prisoner-history* nil "Holds the prioner history for white.")
(defvar *size* nil "Holds the board size.")
(defvar *turn* nil "Holds the color of the current turn.")
(defvar *sgf* nil "The sgf object representing the board.")
(defvar *sgf-file* nil "Path to the current sgf file.")
(defvar *guess-move-mode* nil "Non-nil means to enable guess-move mode.")

(defvar black-piece "X")
(defvar white-piece "O")

(defvar go-board-use-images t)
(defvar *go-board-overlays* nil
  "List of overlays carrying GO board painting information.")

(defun range (a &optional b)
  (block nil
    (let (tmp)
      (unless b
        (cond ((> a 0) (decf a))
              ((= a 0) (return nil))
              ((> 0 a) (incf a)))
        (setq b a a 0))
      (if (> a b) (setq tmp a a b b tmp))
      (let ((res (number-sequence a b)))
        (if tmp (nreverse res) res)))))

(defun pos-to-index (pos size)
  (+ (car pos) (* (cdr pos) size)))

(defun transpose-array (board)
  (let ((size (round (sqrt (length board))))
        (trans (make-vector (length board) nil)))
    (dotimes (row size trans)
      (dotimes (col size)
        (setf (aref trans (pos-to-index (cons (- size 1 row) col) size))
              (aref board (pos-to-index (cons col row) size)))))))

(defun ear-muffs (str) (concat "*" str "*"))

(defun un-ear-muffs (str)
  (let ((pen-ult (1- (length str))))
    (if (and (= ?\* (aref str 0))
             (= ?\* (aref str pen-ult)))
        (substring str 1 pen-ult)
      str)))

;;; Board manipulation functions
(defun make-board (size) (make-vector (* size size) nil))

(defun board-size (board) (round (sqrt (length board))))

(defun move-type (move)
  (cond
   ((equal (cdr move) :pass) :pass)
   ((member (car move) '(:B  :W))  :move)
   ((member (car move) '(:LB :LW)) :label)))

(defun other-color (color)
  (if (equal color :B) :W :B))

(defun point-of-pos (pos)
  ;; Need a transpose because emacs :pos property is different from sgf :pos (link below)
  ;; http://www.red-bean.com/sgf/go.html
  (let ((trans-pos (cons (car pos) (- *size* 1 (cdr pos)))))
    (catch 'found-pos
      (dotimes (p (1- (point-max)) (error "point-of-pos: pos %S not found" pos))
        (let ((pos-at-point (get-text-property (1+ p) :pos)))
          (when (and pos-at-point (tree-equal trans-pos pos-at-point))
            (throw 'found-pos (1+ p))))))))

(defun apply-turn-to-board (turn)
  "Updates the board by adding one 'turn', then displays.
The input 'turn' is actually one sgf node, which may contain multiple elements ('move'), e.g.:
  - ((:B :pos 15 . 3))  ; one move
  - ((:W :pos 16 . 5) (:LB ((:label . '1') (:pos 16 . 3))))  ; two moves"
  (cl-flet ((bset (val data)  ; sets board point at :pos
                  (let ((data (list data)))
                    (setf (aref board (pos-to-index (aget data :pos) *size*))
                          val)))
            (lcol (data)  ; collects label at :pos
                  (push (cons (aget data :label)
                              (point-of-pos (aget data :pos)))
                        labels)))
    (let ((board (pieces-to-board (car *history*) *size*)) (labels))
      (dolist (move turn)
        (case (move-type move)
          (:move
           (bset (car move) (cdr move))
           (let ((color (if (equal :B (car move)) :B :W)))
             (remove-dead board (other-color color))
             (remove-dead board color)))
          (:label
           (dolist (data (cdr move)) (lcol data)))
          (:pass (message "pass"))))
        (push (board-to-pieces board) *history*)
        (push labels *label-history*))))

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
    (case color
      (:B (push (length cull) *black-prisoner-history*))
      (:W (push (length cull) *white-prisoner-history*)))
    (dolist (n cull cull) (setf (aref board n) nil))))

(defun board-to-pieces (board)
  "'pieces' is a list of pairs (board-value . board-index) to represent the board.
Example: pieces ((:W . 111) (:B . 72)) shows there're two stones on the board.

'board' is a size*size vector storing :B, :W or empty.
"
  (let (pieces)
    (dotimes (n (length board) pieces)
      (let ((val (aref board n)))
        (when val (push (cons val n) pieces))))))

(defun pieces-to-board (pieces size)
  "See board-to-pieces."
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
  ;; Needs a transpose: sgf :pos -> 'board' array index -> emacs :pos
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
               (if go-board-use-images
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
          (let ((name (case color (:W "white") (:B "black"))))
            (put-text-property 0 (length name) :turn (equal *turn* color) name)
            name)
          (let ((history (case color (:W *white-prisoner-history*) (:B *black-prisoner-history*))))
            (apply '+ history))))

(defun update-display (buffer)
  (with-current-buffer buffer
    (visual-line-mode)
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
                  (decode-coding-string comment 'utf-8))))
      (go-board-paint)
      (goto-char point))
    (go-board-add-label (car *label-history*)))
  buffer)

(defun go-board-in-buffer (sgf buffer)
  (with-current-buffer buffer
    (go-board-mode)
    (let ((name (go-name sgf)))
      (when name
        (rename-buffer (ear-muffs (decode-coding-string name 'utf-8)) 'unique)))
    (set (make-local-variable '*sgf*) sgf)
    (set (make-local-variable '*turn*) :B)
    (set (make-local-variable '*size*) (go-size sgf))
    (set (make-local-variable '*go-board-overlays*) nil)
    (set (make-local-variable '*history*)
         (list (board-to-pieces (make-board *size*)))))
    (set (make-local-variable '*label-history*) nil)
    (set (make-local-variable '*black-prisoner-history*) nil)
    (set (make-local-variable '*white-prisoner-history*) nil)
    (set (make-local-variable '*guess-move-mode*) nil)
  (pop-to-buffer buffer)
  (setq truncate-lines t)
  (update-display buffer)
  (go-board-show-next))


;;; User inputs and commands
(defun go-open-sgf (&optional file)
  "Opens an SGF file in go-board-mode in a new buffer."
  (interactive "fSGF file: ")
  (let ((buffer (generate-new-buffer "*GO*")))
    (go-load-sgf file buffer)))

(defun go-load-sgf (&optional file buffer)
  (interactive)
  (setf *sgf-file* (or file buffer-file-name))
  (let ((sgf (make-instance
              'sgf
              :self (sgf2el-file-to-el *sgf-file*)
              :index (copy-list '(0))))) ; uses copy-list to avoid modifying '(0)
    (go-board-in-buffer sgf (or buffer (current-buffer)))))

(defun go-toggle-guess-move-mode ()
  "Toggles guess-move mode when viewing sgf."
  (interactive)
  (with-current-buffer (current-buffer)
    (if *guess-move-mode*
        (setf *guess-move-mode* nil)
      (setf *guess-move-mode* t))))

(defun go-board-refresh ()
  (interactive)
  (update-display (current-buffer))
  (go-board-show-next))

(defun go-board-reload ()
  (interactive)
  (go-load-sgf *sgf-file* (current-buffer)))

(defun go-board-mark-point (point mark)
  (mapc (lambda (ov) (go-board-mark ov mark)) (overlays-at point)))

(defun go-board-undo (&optional num)
  (interactive "p")
  (go-undo *sgf*)
  (pop *history*)
  (pop *label-history*)
  (pop *black-prisoner-history*)
  (pop *white-prisoner-history*)
  (update-display (current-buffer))
  (go-board-show-next)
  (setf *turn* (other-color *turn*)))

(defun go-board-next (&optional branch count)
  (interactive "p")
  (if *guess-move-mode* (setf branch 0))
  (let (move)
    (dotimes (n (or count 1) move)
      (setf move (go-move *sgf* (or branch 0)))
      (if move  ; move is nil if already at the last move
          (progn
            (setf *turn* (other-color *turn*))
            (apply-turn-to-board
             (cons move (go-labels *sgf*)))
            (if (equal (cdr move) :pass)
                (goto-char (point-min))
              (goto-char (point-of-pos (cddr move))))
            (update-display (current-buffer))))))
  (go-board-show-next))

(defun go-board-next-0 ()
  "Binded to (kbd \"a\") to move one step further in the first branch. Note:
- (define-key map (kbd \"a\") (kbd \"C-u 0 M-x go-board-next\")) suppresses (message ...) in go-board-next.
- (define-key map (kbd \"a\") 'go-board-next) translates nil to 1 in (interactive \"p\") mode. See
https://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Command-Arguments.html#Prefix-Command-Arguments
"
  (interactive)
  (go-board-next 0))

(defun neighbor-move (move dx dy)
  (let ((color (car move))
        (x (+ dx (caddr move)))
        (y (+ dy (cdddr move)))
        (board (pieces-to-board (car *history*) *size*)))
    (cond
     ((or (< x 0) (< y 0) (> x 18) (> y 18)) nil)
     ((not (equal nil (aref board (pos-to-index (cons x y) *size*)))) nil)
     (t (cons color (cons :pos (cons x y)))))))

(defun random-moves (move)
  "Generates <= 9 moves in a 3x3 square, including the correct move (i.e. the input move).
This is used in guess-move mode.
A typical move may look like (:W :pos 17 . 3).
"
  (let ((ret) (rx (- (random 3) 1)) (ry (- (random 3) 1)))
    (if (eq :move (move-type move))
        (dolist (dy '(1 0 -1))
          (dolist (dx '(1 0 -1))
            (let ((mv (neighbor-move move (+ dx rx) (+ dy ry))))
              (if mv (push mv ret))))))
    ret))

(defun go-board-show-next ()
  (interactive)
  (let ((char 97)  ; "a"
        (candidates))  ; next move candidates
    (if *guess-move-mode*
        (setf candidates (random-moves (car (next-moves *sgf*))))
      (setf candidates (next-moves *sgf*)))
    (dolist (move candidates)
      (case (move-type move)
        (:pass
         (message "%s to pass" (char-to-string char))
         (incf char))
        (:move
         (go-board-mark-point
          (point-of-pos (cddr move))
          (go-board-label
           (ecase (car move) (:B 'darkslategray) (:W 'whitesmoke))
           (char-to-string char)))
         (incf char))))))

(defun go-board-add-label (labels)
  (dolist (label labels)  ; label is a list (text . point)
    (if label
        (let* ((text (car label)) (len (length text)))
          (if (> len 2)
              (setf text (substring text -2 len)))
          (go-board-mark-point
           (cdr label)
           (go-board-label 'red text))))))

(defun go-board-quit ()
  (interactive)
  (when (y-or-n-p "Quit: ")
    (kill-buffer (current-buffer))))


;;; Display mode
(defvar go-board-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'go-board-refresh)
    (define-key map (kbd "R") 'go-board-reload)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "Q") 'go-board-quit)

    (define-key map (kbd "SPC") 'go-board-undo)
    (define-key map (kbd "RET") 'go-board-next-0)
    (define-key map (kbd "a") 'go-board-next-0)
    (define-key map (kbd "b") (kbd "C-u 1 M-x go-board-next"))
    (define-key map (kbd "c") (kbd "C-u 2 M-x go-board-next"))
    (define-key map (kbd "d") (kbd "C-u 3 M-x go-board-next"))
    (define-key map (kbd "e") (kbd "C-u 4 M-x go-board-next"))
    (define-key map (kbd "f") (kbd "C-u 5 M-x go-board-next"))
    (define-key map (kbd "g") (kbd "C-u 6 M-x go-board-next"))
    (define-key map (kbd "h") (kbd "C-u 7 M-x go-board-next"))
    (define-key map (kbd "i") (kbd "C-u 8 M-x go-board-next"))
    (define-key map (kbd "j") (kbd "C-u 9 M-x go-board-next"))
    (define-key map (kbd "k") (kbd "C-u 10 M-x go-board-next"))
    (define-key map (kbd "l") (kbd "C-u 11 M-x go-board-next"))
    (define-key map (kbd "m") (kbd "C-u 12 M-x go-board-next"))
    (define-key map (kbd "n") (kbd "C-u 13 M-x go-board-next"))
    map)
  "Keymap for `go-board-mode'.")

(define-derived-mode go-board-mode nil "go-board"
  "Major mode for viewing SGF files.")

(provide 'go-board-mode)
;;; go-board-mode.el ends here
