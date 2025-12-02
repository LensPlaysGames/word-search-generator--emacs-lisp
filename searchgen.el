;;; searchgen.el --- Generate a word search -*- lexical-binding: t -*-

;; Author: Lens_r
;; Maintainer: Lens_r
;; Version: 0.1.0
;; Package-Requires: ((emacs))
;; Homepage: homepage
;; Keywords: game, play, word search


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; (searchgen-advanced '("bat" "bear" "bee" "cat" "cow" "deer" "dog" "dove" "duck" "frog" "goose" "horse" "kiwi" "lion" "pig" "rat" "robin" "whale"))
;; (searchgen-advanced '("code" "compiler" "expression" "type" "integer" "address" "operator" "declaration" "return" "if" "for") :size 12)

;;; Code:

(eval-when-compile (require 'cl-lib))

(defcustom searchgen--retry-limit
  10
  "The amount of times to retry putting words on the board once the
 maximum size has been reached."
  :type 'integer
  :group 'searchgen)

(defvar searchgen--basic-fill-chars
  "abcdefghijklmnopqrstuvwxyz")

;; https://en.wikipedia.org/wiki/Letter_frequency
;; TODO:
;; - Most common doubled letters: LL EE SS OO TT FF RR NN PP CC
;; - Most common letter pairs:
;;   TH HE AN RE ER IN ON AT ND ST ES EN OF TE ED OR TI HI AS TO
;; It'd be cool to favor those more in the fill character positions. We
;; may end up accidentally making more actual words this way, though.
(defvar searchgen--probability-fill-chars
  (concat
   (make-list 82 ?a)
   (make-list 15 ?b)
   (make-list 28 ?c)
   (make-list 43 ?d)
   (make-list 127 ?e)
   (make-list 22 ?f)
   (make-list 20 ?g)
   (make-list 61 ?h)
   (make-list 70 ?i)
   (make-list 4 ?j)
   (make-list 7 ?k)
   (make-list 40 ?l)
   (make-list 24 ?m)
   (make-list 67 ?n)
   (make-list 75 ?o)
   (make-list 19 ?p)
   (make-list 2 ?q)
   (make-list 60 ?r)
   (make-list 63 ?s)
   (make-list 91 ?t)
   (make-list 28 ?u)
   (make-list 10 ?v)
   (make-list 24 ?w)
   (make-list 2 ?x)
   (make-list 20 ?y)
   (make-list 1 ?z)))

;; Inspired by https://gist.github.com/purcell/34824f1b676e6188540cdf71c7cc9fc4
(defun searchgen--seq-shuffle (seq)
  (seq-sort-by (lambda (_) (random)) #'<= seq))

(defun searchgen--longest (words)
  "Given a list of strings, WORDS, return the longest length."
  (let ((n 0))
    (mapc
     (lambda (word)
       (when (> (length word) n) (setf n (length word))))
     words)
    n))
(cl-assert (= 0 (searchgen--longest '())))
(cl-assert (= 4 (searchgen--longest '("f" "foo" "b" "baar" "as"))))

(defun searchgen--sort-by-length (words)
  "Given a list of strings, WORDS, return a sorted list longest to shortest."
  (sort words :key 'length :reverse t))
(cl-assert (equal '("goo" "fo" "bo" "f" "") (searchgen--sort-by-length '("" "f" "fo" "bo" "goo"))))

(defun searchgen--right-coordinates (word x y)
  (let ((the-list nil))
    (dotimes (x1 (length word))
      (push (list (+ x x1) y) the-list))
    (reverse the-list)))

(defun searchgen--left-coordinates (word x y)
  (let ((the-list nil))
    (dotimes (x1 (length word))
      (push (list (- x x1) y) the-list))
    (reverse the-list)))

(defun searchgen--down-coordinates (word x y)
  (let ((the-list nil))
    (dotimes (y1 (length word))
      (push (list x (+ y y1)) the-list))
    (reverse the-list)))

(defun searchgen--up-coordinates (word x y)
  (let ((the-list nil))
    (dotimes (y1 (length word))
      (push (list x (- y y1)) the-list))
    (reverse the-list)))

(defun searchgen--upleft-coordinates (word x y)
  (let ((the-list nil))
    (dotimes (y1 (length word))
      (push (list (- x y1) (- y y1)) the-list))
    (reverse the-list)))

(defun searchgen--upright-coordinates (word x y)
  (let ((the-list nil))
    (dotimes (y1 (length word))
      (push (list (+ x y1) (- y y1)) the-list))
    (reverse the-list)))

(defun searchgen--downleft-coordinates (word x y)
  (let ((the-list nil))
    (dotimes (y1 (length word))
      (push (list (- x y1) (+ y y1)) the-list))
    (reverse the-list)))

(defun searchgen--downright-coordinates (word x y)
  (let ((the-list nil))
    (dotimes (y1 (length word))
      (push (list (+ x y1) (+ y y1)) the-list))
    (reverse the-list)))

(defun searchgen--board-positions (board-size)
  (let ((the-list nil))
    (cl-loop
     for y in (number-sequence 0 (1- board-size))
     do (cl-loop
  	   for x in (number-sequence 0 (1- board-size))
  	   do (push (list x y) the-list)))
    the-list))

(defvar searchgen--basic-direction-functions
  (list 'searchgen--right-coordinates 'searchgen--down-coordinates))

(defvar searchgen--intermediate-direction-functions
  (list 'searchgen--right-coordinates
        'searchgen--down-coordinates
        'searchgen--downright-coordinates))

(defvar searchgen--all-direction-functions
  (list 'searchgen--left-coordinates 'searchgen--right-coordinates
        'searchgen--down-coordinates 'searchgen--up-coordinates
        'searchgen--upleft-coordinates 'searchgen--upright-coordinates
        'searchgen--downleft-coordinates 'searchgen--downright-coordinates))

(defun searchgen--valid-at (char x y board board-size)
  "Return true iff character may be placed within board at given position."
  (and
   ;; position allows
   ;;   0 >= x < board-size
   (>= x 0)
   (< x board-size)
   ;;   0 >= y < board-size
   (>= y 0)
   (< y board-size)
   ;; value allows
   (or
    ;; empty cell
    (eq 'empty (nth (+ x (* y board-size)) board))
    ;; matches character in word
    (string= (char-to-string char) (nth (+ x (* y board-size)) board)))))


(defun searchgen--print-board (board board-size)
  (cl-assert (listp board))
  (cl-assert (= (* board-size board-size) (length board)))
  (cl-assert (integerp board-size))
  (cl-assert (>= board-size 0))

  (let ((index 0))
    (mapconcat
     (lambda (char)
       (format "%s%s"
               char
               (if (= 0 (% (cl-incf index) board-size)) "\n" " ")))
     board))
  )

(defun searchgen--as-latex (board board-size words)
  "Return a string containing LaTeX source (hopefully) of a single-page
PDF containing the word search puzzle"
  (with-temp-buffer

    (insert "\\documentclass{article}\n")
    (insert "\\usepackage[letterpaper]{geometry}\n")
    (insert "\\usepackage{graphicx}\n")
    (insert "\\usepackage{adjustbox}\n")
    (insert "\\begin{document}\n")

    (insert "\\begin{center}\n")
    (insert "\\begin{adjustbox}{width=\\textwidth}\n")
    (insert "\\begin{tabular}{|")
    (dotimes (_ board-size)
      (insert " c"))
    (insert " |}\n")

    (insert "\\hline\n")
    (dotimes (n (* board-size board-size))
      (insert (nth (+ (% n board-size) (* board-size (/ n board-size))) board))
      (if (= (1- board-size) (% n board-size))
          (insert " \\\\\n")
        (insert " & ")))
    (insert "\\hline\n")

    (insert "\\end{tabular}\n")
    (insert "\\end{adjustbox}\n")
    (insert "\\end{center}\n")

    (insert "\\begin{center}\n")
    (insert "\\begin{adjustbox}{width=\\textwidth}\n")
    (insert "\\begin{tabular}{ccccc}\n")
    ;; padding := (align - (value % align)) % align
    ;; aligned-value := value + padding
    (dotimes (_ (% (- 5 (% (length words) 5)) 5))
      (setf words (append words (list ""))))
    (seq-map-indexed
     (lambda (word index)
       (insert word)
       (if (= (1- 5) (% index 5))
           (insert " \\\\\n")
         (insert " & ")))
     words)

    (insert "\\end{tabular}\n")
    (insert "\\end{adjustbox}\n")
    (insert "\\end{center}\n")

    (insert "\\end{document}\n")
    (buffer-string)))

(defun searchgen--as-plaintext (board board-size words)
  "Return a string of plain text source containing the given word search puzzle."
  (with-temp-buffer

    (dotimes (n (* board-size board-size))
      (insert (nth (+ (% n board-size) (* board-size (/ n board-size))) board))
      (if (= (1- board-size) (% n board-size))
          (insert "\n")
        (insert " ")))
    (insert "\n")

    (seq-map-indexed
     (lambda (word index)
       (insert word)
       (if (or (= index (1- (length words))) (= (1- 5) (% index 5)))
           (insert "\n")
         (insert "  ")))
     words)
    (buffer-string)))

(cl-defun searchgen-make--impl (words board-size fill-character-set direction-functions depth &optional &key seed max-board-size)
  (cl-assert (integerp board-size) t)
  (cl-assert (>= board-size 0) t)

  (cl-assert (seqp fill-character-set) t)

  (when max-board-size
    (cl-assert (<= board-size max-board-size)))

  (let*
      ((board (make-list (* board-size board-size) 'empty))
       ;; Attempt to place each word in the board.
       (words-placed
        ;; For every word (sorted longest to shortest)...
        (mapcar
         (lambda (word)
           ;; For every position (shuffled)
           (let ((word-placed nil))
             (cl-loop until word-placed
                      for position in (searchgen--seq-shuffle (searchgen--board-positions board-size))
                      do (let
                             ((x (nth 0 position))
                              (y (nth 1 position)))
                           ;; For every direction (shuffled)
                           (mapc
                            (lambda (direction-function)
                              (unless
                                  word-placed
                                (let
                                    ((validity-of-positions
                                      (seq-mapn
                                       (lambda (position char)
                                         (let
                                             ((x (nth 0 position))
                                              (y (nth 1 position)))
                                           (searchgen--valid-at char x y board board-size)))
                                       (funcall direction-function word x y)
                                       word)))
                                  ;; (message "Word valid to be placed in RIGHT direction at (%s,%s): %s" x y (unless (memq nil validity-of-positions) t))
                                  ;; When word /is/ actually valid, perform placement (alter 'board')
                                  (unless (memq nil validity-of-positions)
                                    (seq-mapn
                                     (lambda (position char)
                                       (let ((x1 (nth 0 position))
                                             (y1 (nth 1 position)))
                                         (setf (nth (+ x1 (* y1 board-size)) board) (char-to-string char))
                                         ;; (message "Placed character %c at coordinates (%s,%s)" char x1 y1)
                                         ;; (print (searchgen--print-board board board-size))
                                         ))
                                     (funcall direction-function word x y)
                                     word)
                                    ;; (message "Placed WORD %s at coordinates (%s,%s)" word x y)
                                    (setf word-placed t)
                                    t)))
                              )
                            (searchgen--seq-shuffle direction-functions))
                           ))
             word-placed))
         words)))

    (if (memq nil words-placed)
        ;; Board invalid, could not place words successfully.
        (if (and
             (and
              max-board-size
              (>= board-size max-board-size))
             (>= depth searchgen--retry-limit))
            (error "Recurse limit %d reached at maximum board size"
                   searchgen--retry-limit)
          ;; Recurse!
          (searchgen-make--impl
           words
           (if max-board-size
               (min max-board-size (1+ board-size))
             (1+ board-size))
           fill-character-set
           direction-functions
           (1+ depth)
           :seed seed
           :max-board-size max-board-size))
      (progn
        ;; With every word placed, fill in the empty positions with characters
        ;; from the fill character set (at random, for now).
        ;; TODO: I think it may be interesting to pull portions of the input words
        ;; and try to place those in the empty positions, to have red herrings.
        (setf board
              (mapcar
               (lambda (cell)
                 (if (eq 'empty cell)
                     (char-to-string
                      (seq-random-elt fill-character-set))
                   cell))
               board))

        ;; Return board
        board
        ))))

(cl-defun searchgen-make (words fill-character-set direction-functions &optional &key seed min-size max-size)
  "'WORDS'   A list of strings which will be placed in the word search, to
be found.
'FILL-CHARACTER-SET'   A string, each character of which may be used to
fill an empty position after all words are placed.
'DIRECTION-FUNCTIONS'    A list of callables that take three arguments.
A 'word' that is to be placed at given position ('X','Y').
Expected to return a list of coordinate pairs (lists of length 2)
corresponding to where each character of the given word would go, if it
were to be placed in that direction.

OPTIONAL:
':seed'   A string that is passed to 'random', to make it so the same
board may be generated across multiple invocations.
':min-size'   An integer that sets the minimum size of the given puzzle board.
':max-size'   An integer that sets the maximum size of the given puzzle board;
 once at this size, will retry 'searchgen--retry-limit' times before emitting an error."

  (cl-assert (seqp fill-character-set) t)

  (when (stringp seed)
    (random seed))

  (searchgen-make--impl
   (searchgen--sort-by-length (mapcar 'downcase words))
   (cond
    (min-size)
    ((searchgen--longest words)))
   fill-character-set
   direction-functions
   1
   :seed seed
   :max-board-size max-size))

(defun searchgen--to-string (board board-size)
  "Returns simple string representation of given board."
  (searchgen--print-board board board-size))

(defun searchgen--to-latex-file (path board board-size words)
  "Writes the LaTeX representation of the given puzzle board to a
file at 'PATH'.
'PATH'   Passed to 'find-file-noselect' to get a buffer.
See 'searchgen--as-plaintext'."
  (with-current-buffer (find-file-noselect path)
    (delete-region (point-min) (point-max))
    (insert (searchgen--as-latex board board-size words))
    (save-buffer)))

(defun searchgen--to-plaintext-file (path board board-size words)
  "Writes the plaintext representation of the given puzzle board to a
file at 'PATH'.
'PATH'   Passed to 'find-file-noselect' to get a buffer.
See 'searchgen--as-plaintext'."
  (with-current-buffer (find-file-noselect path)
    (delete-region (point-min) (point-max))
    (insert (searchgen--as-plaintext board board-size words))
    (save-buffer)))

(defun searchgen--to-all (board board-size words)
  (searchgen--to-plaintext-file "board.txt" board board-size words)
  (searchgen--to-latex-file "board.tex" board board-size words)
  (searchgen--to-string board board-size))

(cl-defun searchgen--driver (words fill-chars direction-functions &key seed min-size max-size)
  (when max-size
    (cl-assert (>= max-size (searchgen--longest words))
               nil
               "Maximum size, %d, is less than the length of the longest word, %d."
               max-size (searchgen--longest words)))

  (when (and min-size max-size)
    (cl-assert (>= max-size min-size)
               nil
               "Given maximum size, %d, is less than the given minimum size, %d."
               max-size min-size))

  (let
      ((board (searchgen-make
               words
               fill-chars
               direction-functions
               :seed seed
               :min-size min-size
               :max-size max-size)))
    (searchgen--to-all board (cl-isqrt (length board)) words)))

(cl-defun searchgen-basic (words &optional &key seed min-size max-size)
  "Limits word direction to RIGHT, and DOWN."
  (searchgen--driver
   words
   searchgen--basic-fill-chars
   searchgen--basic-direction-functions
   :seed seed
   :min-size min-size
   :max-size max-size))

(cl-defun searchgen-intermediate (words &optional &key seed min-size max-size)
  "Limits word direction to RIGHT, DOWN, and DOWN-RIGHT.
Fill characters roughly weighted based on frequency of appearance in general text."
  (searchgen--driver
   words
   searchgen--probability-fill-chars
   searchgen--intermediate-direction-functions
   :seed seed
   :min-size min-size
   :max-size max-size))

(cl-defun searchgen-advanced (words &optional &key seed min-size max-size)
  "- All directions possible:
LEFT, RIGHT, UP, DOWN, UP-LEFT, UP-RIGHT, DOWN-LEFT, DOWN-RIGHT.
- Fill characters roughly weighted based on frequency of appearance in
general text. That is, `e` shows up more than `z`, for example.
- Alters fill characters to only contain characters that are in the words
you are trying to find. If there is no `z` in any input word, it won't
be in the board.
"
  (let*
      ((words-text (mapconcat 'identity words))
       (fill-chars (seq-filter
                    (lambda (char)
                      (string-search (char-to-string char) words-text))
                    searchgen--probability-fill-chars)))
    (searchgen--driver
     words
     fill-chars
     searchgen--all-direction-functions
     :seed seed
     :min-size min-size
     :max-size max-size)))

(provide 'searchgen)

;;; searchgen.el ends here
