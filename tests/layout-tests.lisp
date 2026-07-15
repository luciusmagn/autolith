(in-package #:autolith)

;;;; -- Cell-Aware Layout Tests --

(-> test-layout-column-widths () null)
(defun test-layout-column-widths ()
  "Test bounded columns preserve small cells and share constrained space."
  (test-assert (equal (layout-column-widths
                       '(("id" "description")
                         ("longer-id" "a considerably longer description"))
                       30
                       :gap-width 2)
                      '(9 19))
               "column layout preserves the short label column before wrapping values")
  (test-assert (= (reduce #'+
                          (layout-column-widths
                           '(("first" "second"))
                           24
                           :gap-width 3
                           :fill-p t))
                  21)
               "filled cell widths account for their inter-column gap")
  (test-assert (equal (layout-column-widths
                       '(("a" "b" "c"))
                       2
                       :gap-width 1)
                      '(0 0 0))
               "impossibly narrow layouts never exceed their width")
  nil)

(-> test-layout-fit-text () null)
(defun test-layout-fit-text ()
  "Test cell-aware clipping, padding, and alignment."
  (test-assert (string= (layout-fit-text "ab" 4) "ab  ")
               "left-aligned cells pad on the right")
  (test-assert (string= (layout-fit-text "ab" 4 :alignment ':right) "  ab")
               "right-aligned cells pad on the left")
  (test-assert (= (text-cell-width (layout-fit-text "λ界x" 3)) 3)
               "fitted Unicode text consumes its exact terminal-cell budget")
  nil)

(-> run-layout-tests () boolean)
(defun run-layout-tests ()
  "Run cell-aware layout tests and return true on success."
  (test-layout-column-widths)
  (test-layout-fit-text)
  t)
