(in-package #:autolith)

;;;; -- Cell-Aware Column Layout --

(-> layout--column-count (list) (integer 0))
(defun layout--column-count (rows)
  "Return the greatest number of cells in any of ROWS."
  (loop for row in rows
        maximize (length row)))

(-> layout--minimum-widths (integer (option list)) list)
(defun layout--minimum-widths (column-count requested-widths)
  "Return COLUMN-COUNT non-negative minimum widths from REQUESTED-WIDTHS."
  (loop for index below column-count
        for requested = (and requested-widths
                             (nth index requested-widths))
        collect (if (and (integerp requested) (not (minusp requested)))
                    requested
                    1)))

(-> layout--natural-widths (list integer list) list)
(defun layout--natural-widths (rows column-count minimum-widths)
  "Return the widest terminal-cell demand for each column in ROWS."
  (loop for index below column-count
        for minimum in minimum-widths
        collect
        (max minimum
             (loop for row in rows
                   for cell = (nth index row)
                   when (stringp cell)
                     maximize (text-cell-width cell) into width
                   finally (return (or width 0))))))

(-> layout--fit-minimums (list integer) list)
(defun layout--fit-minimums (minimum-widths budget)
  "Fit MINIMUM-WIDTHS into BUDGET, preserving one cell per column when possible."
  (let ((widths (make-list (length minimum-widths) :initial-element 0))
        (remaining budget))
    (loop while (plusp remaining)
          for changed-p = nil
          do (loop for minimum in minimum-widths
                   for tail on widths
                   when (and (plusp remaining)
                             (< (first tail) minimum))
                     do (incf (first tail))
                        (decf remaining)
                        (setf changed-p t))
          unless changed-p
            do (loop-finish))
    widths))

(-> layout--grow-widths (list list integer &key (:fill-p boolean)) list)
(defun layout--grow-widths (widths natural-widths budget &key fill-p)
  "Grow WIDTHS toward NATURAL-WIDTHS within BUDGET and optionally fill it."
  (let ((remaining (- budget (reduce #'+ widths :initial-value 0))))
    (loop while (plusp remaining)
          for candidates = (loop for natural in natural-widths
                                 for width in widths
                                 for index from 0
                                 when (< width natural)
                                   collect index)
          while candidates
          for narrowest = (reduce
                           (lambda (left right)
                             (if (<= (nth left widths)
                                     (nth right widths))
                                 left
                                 right))
                           candidates)
          do (incf (nth narrowest widths))
             (decf remaining))
    (when (and fill-p (plusp remaining) widths)
      (loop for index from 0 below remaining
            do (incf (nth (mod index (length widths)) widths))))
    widths))

(-> layout-column-widths
    (list integer &key (:gap-width integer)
                       (:minimum-widths (option list))
                       (:fill-p boolean))
    list)
(defun layout-column-widths
    (rows total-width &key (gap-width 1) minimum-widths fill-p)
  "Return terminal-cell widths for the columns in ROWS within TOTAL-WIDTH.

TOTAL-WIDTH includes GAP-WIDTH cells between neighboring columns. Natural
widths are preserved when possible. Oversized columns share the remaining
space after smaller columns reach their natural width. FILL-P distributes any
surplus space instead of leaving it unused."
  (let ((column-count (layout--column-count rows)))
    (if (zerop column-count)
        nil
        (let* ((gap-budget (* (max 0 gap-width) (1- column-count)))
               (cell-budget (max 0 (- (max 0 total-width) gap-budget)))
               (minimums (layout--minimum-widths column-count minimum-widths))
               (naturals (layout--natural-widths rows column-count minimums))
               (widths (layout--fit-minimums minimums cell-budget)))
          (layout--grow-widths widths naturals cell-budget
                               :fill-p fill-p)))))

(-> layout-fit-text
    (string integer &key (:alignment (member :left :right)))
    string)
(defun layout-fit-text (text width &key (alignment ':left))
  "Clip TEXT to WIDTH terminal cells and pad it using ALIGNMENT.

ALIGNMENT is either :LEFT or :RIGHT."
  (let* ((width (max 0 width))
         (visible (text-cell-prefix text width))
         (padding (make-string (max 0 (- width (text-cell-width visible)))
                               :initial-element #\Space)))
    (ecase alignment
      (:left (concatenate 'string visible padding))
      (:right (concatenate 'string padding visible)))))
