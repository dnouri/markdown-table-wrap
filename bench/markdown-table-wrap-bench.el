;;; markdown-table-wrap-bench.el --- Benchmarks for markdown-table-wrap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Performance benchmarks for markdown-table-wrap.  Run with:
;;
;;   make bench          # GUI via xvfb (matches real Emacs)
;;   make bench-batch    # --batch (no font engine, ~2.3× faster)
;;
;; or directly:
;;
;;   xvfb-run -a emacs -Q -L . -l bench/markdown-table-wrap-bench.el \
;;         -f markdown-table-wrap-bench-run-batch
;;
;; The default `make bench` uses xvfb-run to start a headless GUI
;; Emacs.  This matters because `string-width' — the hot path in
;; wrapping — consults font metrics in GUI mode but uses a simple
;; character table in --batch mode, making batch ~2.3× faster.
;; GUI numbers reflect what users actually experience.
;;
;; Each benchmark prints:  NAME  ELAPSED  GCS  GCTIME
;;
;; The primary metric is the "resize-all" benchmark, which simulates
;; what happens when a user resizes a window containing multiple
;; tables: every table is re-wrapped at 20 different widths.
;;
;; Fixture: bench/fixtures/tables.md
;; Four tables (compact, single-line rows) with rich markdown:
;; bold, italic, code, strikethrough, links, bold-italic, and CJK
;; characters.  Column counts range from 4 to 7 with diverse
;; separator styles (short colons, plain dashes, long varied
;; dashes, compact no-space).  Cell content includes dense prose,
;; accented characters (á, é), currency symbols ($, £), em dashes,
;; and mixed Chinese/Japanese/Korean text.

;;; Code:

(require 'benchmark)
(require 'markdown-table-wrap)

;;;; Fixture Loading

(defvar markdown-table-wrap-bench--tables nil
  "List of table strings extracted from the fixture file.")

(defvar markdown-table-wrap-bench--fixture-dir
  (expand-file-name "fixtures/"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Directory containing benchmark fixture files.")

(defun markdown-table-wrap-bench--extract-tables (file)
  "Extract pipe tables from FILE, returning a list of table strings."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((lines (split-string (buffer-string) "\n"))
          tables current)
      (dolist (line lines)
        (if (string-match-p "^|" (string-trim line))
            (push line current)
          (when current
            (push (mapconcat #'identity (nreverse current) "\n") tables)
            (setq current nil))))
      (when current
        (push (mapconcat #'identity (nreverse current) "\n") tables))
      (nreverse tables))))

(defun markdown-table-wrap-bench--load-fixtures ()
  "Load fixture tables if not already loaded."
  (unless markdown-table-wrap-bench--tables
    (let ((file (expand-file-name "tables.md"
                                  markdown-table-wrap-bench--fixture-dir)))
      (unless (file-exists-p file)
        ;; Fall back to parenti-tables.md for backward compatibility
        (setq file (expand-file-name "parenti-tables.md"
                                     markdown-table-wrap-bench--fixture-dir)))
      (unless (file-exists-p file)
        (error "No fixture found in %s" markdown-table-wrap-bench--fixture-dir))
      (setq markdown-table-wrap-bench--tables
            (markdown-table-wrap-bench--extract-tables file)))))

;;;; Synthetic Table Generation

(defun markdown-table-wrap-bench--make-table (num-rows num-cols cell-width)
  "Generate a synthetic table with NUM-ROWS, NUM-COLS, CELL-WIDTH chars/cell.
Return a pipe-table string with headers, separator, and data rows.
Cell content is random words to exercise the wrapping logic."
  (let* ((words '("the" "quick" "brown" "fox" "jumps" "over" "lazy" "dog"
                  "and" "runs" "through" "forest" "with" "great" "speed"
                  "while" "other" "animals" "watch" "from" "distance"
                  "implementation" "infrastructure" "documentation"
                  "systematically" "internationalization"))
         (make-cell (lambda ()
                      (let ((s "") (target (max 1 (+ cell-width (- (random 7) 3)))))
                        (while (< (length s) target)
                          (setq s (concat s (when (> (length s) 0) " ")
                                         (nth (random (length words)) words))))
                        (substring s 0 (min (length s) (+ cell-width 5))))))
         (make-row (lambda ()
                     (concat "| "
                             (mapconcat (lambda (_) (funcall make-cell))
                                        (number-sequence 1 num-cols) " | ")
                             " |")))
         (headers (concat "| "
                          (mapconcat (lambda (i) (format "Col_%d" i))
                                     (number-sequence 1 num-cols) " | ")
                          " |"))
         (sep (concat "| "
                      (mapconcat (lambda (_) "---")
                                 (number-sequence 1 num-cols) " | ")
                      " |")))
    (mapconcat #'identity
               (append (list headers sep)
                       (cl-loop for _ from 1 to num-rows
                                collect (funcall make-row)))
               "\n")))

;;;; Benchmark Definitions

(defun markdown-table-wrap-bench--wrap-single ()
  "Wrap the largest fixture table once at width 80."
  (markdown-table-wrap-bench--load-fixtures)
  (let ((tbl (car markdown-table-wrap-bench--tables)))
    (benchmark-run 1
      (markdown-table-wrap tbl 80))))

(defun markdown-table-wrap-bench--parse-single ()
  "Parse the largest fixture table."
  (markdown-table-wrap-bench--load-fixtures)
  (let ((tbl (car markdown-table-wrap-bench--tables)))
    (benchmark-run 1
      (markdown-table-wrap-parse tbl))))

(defun markdown-table-wrap-bench--resize-single ()
  "Wrap the largest table at 20 different widths (resize simulation)."
  (markdown-table-wrap-bench--load-fixtures)
  (let ((tbl (car markdown-table-wrap-bench--tables))
        (widths (number-sequence 40 196 8)))
    (benchmark-run 1
      (dolist (w widths)
        (markdown-table-wrap tbl w)))))

(defun markdown-table-wrap-bench--resize-all ()
  "Wrap all fixture tables at 20 different widths.
This is the primary benchmark: it simulates what happens when a
user resizes a window containing a buffer with multiple tables."
  (markdown-table-wrap-bench--load-fixtures)
  (let ((widths (number-sequence 40 196 8)))
    (benchmark-run 1
      (dolist (tbl markdown-table-wrap-bench--tables)
        (dolist (w widths)
          (markdown-table-wrap tbl w))))))

(defun markdown-table-wrap-bench--narrow-all ()
  "Wrap all fixture tables at width 40 (worst case — maximum wrapping)."
  (markdown-table-wrap-bench--load-fixtures)
  (benchmark-run 1
    (dolist (tbl markdown-table-wrap-bench--tables)
      (markdown-table-wrap tbl 40))))

(defun markdown-table-wrap-bench--wide-all ()
  "Wrap all fixture tables at width 200 (best case — minimal wrapping)."
  (markdown-table-wrap-bench--load-fixtures)
  (benchmark-run 1
    (dolist (tbl markdown-table-wrap-bench--tables)
      (markdown-table-wrap tbl 200))))

(defun markdown-table-wrap-bench--scale-rows ()
  "Scaling test: synthetic tables with 10, 50, 200, 500 rows, 5 cols."
  (let ((results nil))
    (dolist (n '(10 50 200 500))
      (let ((tbl (markdown-table-wrap-bench--make-table n 5 20)))
        (garbage-collect)
        (let ((r (benchmark-run 1 (markdown-table-wrap tbl 80))))
          (push (cons n r) results))))
    (nreverse results)))

(defun markdown-table-wrap-bench--scale-cols ()
  "Scaling test: synthetic tables with 50 rows, 2/5/10/20 cols."
  (let ((results nil))
    (dolist (c '(2 5 10 20))
      (let ((tbl (markdown-table-wrap-bench--make-table 50 c 15)))
        (garbage-collect)
        (let ((r (benchmark-run 1 (markdown-table-wrap tbl 120))))
          (push (cons c r) results))))
    (nreverse results)))

;;;; Runner

(defconst markdown-table-wrap-bench--suite
  '(("parse-single"  . markdown-table-wrap-bench--parse-single)
    ("wrap-single"   . markdown-table-wrap-bench--wrap-single)
    ("narrow-all"    . markdown-table-wrap-bench--narrow-all)
    ("wide-all"      . markdown-table-wrap-bench--wide-all)
    ("resize-single" . markdown-table-wrap-bench--resize-single)
    ("resize-all"    . markdown-table-wrap-bench--resize-all))
  "Alist of (NAME . FUNCTION) for the benchmark suite.")

(defvar markdown-table-wrap-bench--repetitions 5
  "Number of repetitions for each benchmark.")

(defun markdown-table-wrap-bench-run (&optional repetitions)
  "Run all benchmarks REPETITIONS times, print results.
Default is 5 repetitions."
  (let ((k (or repetitions markdown-table-wrap-bench--repetitions)))
    (markdown-table-wrap-bench--load-fixtures)

    ;; Header
    (princ (format "\nmarkdown-table-wrap benchmarks (%d repetitions)\n" k))
    (princ (format "Fixture: %d tables, %s input lines\n"
                   (length markdown-table-wrap-bench--tables)
                   (mapconcat (lambda (tbl)
                                (number-to-string
                                 (length (split-string tbl "\n" t))))
                              markdown-table-wrap-bench--tables
                              "/")))
    (princ (format "Emacs %s, %s\n\n"
                   emacs-version
                   (if noninteractive "batch (no font engine)"
                     (format "GUI (font: %s)"
                             (face-attribute 'default :family)))))

    ;; Column headers
    (princ (format "%-16s %8s %8s %8s  %4s %8s\n"
                   "benchmark" "min" "median" "max" "GCs" "GC-time"))
    (princ (format "%-16s %8s %8s %8s  %4s %8s\n"
                   "---------" "---" "------" "---" "---" "-------"))

    ;; Run suite
    (dolist (entry markdown-table-wrap-bench--suite)
      (let ((name (car entry))
            (fn (cdr entry))
            (times nil)
            (total-gcs 0)
            (total-gc-time 0.0))
        (dotimes (_ k)
          (garbage-collect)
          (let ((result (funcall fn)))
            (push (car result) times)
            (setq total-gcs (+ total-gcs (nth 1 result)))
            (setq total-gc-time (+ total-gc-time (nth 2 result)))))
        (let* ((sorted (sort (copy-sequence times) #'<))
               (mn (car sorted))
               (mx (car (last sorted)))
               (med (nth (/ k 2) sorted)))
          (princ (format "%-16s %7.1fms %7.1fms %7.1fms  %4d %7.1fms\n"
                         name
                         (* 1000 mn) (* 1000 med) (* 1000 mx)
                         total-gcs (* 1000 total-gc-time))))))

    ;; Scaling tests (single run each)
    (princ (format "\n%-16s %8s %8s\n" "scaling-rows" "rows" "ms"))
    (princ (format "%-16s %8s %8s\n" "------------" "----" "----"))
    (dolist (entry (markdown-table-wrap-bench--scale-rows))
      (princ (format "%-16s %8d %7.1fms\n"
                     (format "rows/%d/5cols" (car entry))
                     (car entry)
                     (* 1000 (cadr entry)))))

    (princ (format "\n%-16s %8s %8s\n" "scaling-cols" "cols" "ms"))
    (princ (format "%-16s %8s %8s\n" "------------" "----" "----"))
    (dolist (entry (markdown-table-wrap-bench--scale-cols))
      (princ (format "%-16s %8d %7.1fms\n"
                     (format "cols/50rows/%d" (car entry))
                     (car entry)
                     (* 1000 (cadr entry)))))

    (princ "\n")))

(defun markdown-table-wrap-bench-run-batch ()
  "Entry point for batch execution.  Run benchmarks and exit."
  (let ((standard-output #'external-debugging-output)
        (args command-line-args-left)
        (k markdown-table-wrap-bench--repetitions))
    ;; Parse -c flag for repetition count
    (while args
      (cond ((string= (car args) "-c")
             (setq args (cdr args))
             (when args
               (setq k (string-to-number (pop args)))))
            (t (pop args))))
    (markdown-table-wrap-bench-run k)
    (kill-emacs 0)))

(provide 'markdown-table-wrap-bench)
;;; markdown-table-wrap-bench.el ends here
