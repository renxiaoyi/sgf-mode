;;; go.el --- Play GO, translate and transfer between GO back ends

;; Copyright (C) 2012  Free Software Foundation, Inc.

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Maintainer: Eric Schulte <schulte.eric@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))
;; Created: 2012-05-15
;; Keywords: game go sgf
;; URL: http://eschulte.github.io/el-go/

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
(let ((load-path
       (cons (file-name-directory (or load-file-name (buffer-file-name)))
             load-path)))
  (require 'go-board        "go-board.el")
  (require 'go-board-faces  "go-board-faces.el")
  (require 'sgf             "sgf.el")
  (require 'sgf2el          "sgf2el.el"))

;;;###autoload
(defun go-view-sgf (&optional file)
  "View an SGF file."
  (interactive "fSGF file: ")
  (setq *sgf-file* file)
  (let ((sgf (make-instance 'sgf
                            :self (sgf2el-file-to-el file)
                            :index (copy-list '(0))))) ; uses copy-list to avoid modifying '(0)
    (go-board sgf)))

;;;###autoload
(defun go-reload-sgf ()
  "Reloads the current SGF file."
  (interactive)
  (let ((sgf (make-instance 'sgf
                            :self (sgf2el-file-to-el *sgf-file*)
                            :index (copy-list '(0)))))
    (go-board-in-buffer sgf (current-buffer))))

(provide 'go)
;;; go.el ends here
