;;; markdown-table-wrap-test.el --- Tests for markdown-table-wrap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the markdown-table-wrap package.
;; Covers parsing, cell wrapping, markup-aware wrapping, column width
;; computation, full table rendering, cell height caps, and code fence
;; detection.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'markdown-table-wrap)

;;;; Helpers

(defvar markdown-table-wrap-test--project-root
  (file-name-directory
   (directory-file-name
    (file-name-directory
     (or load-file-name (buffer-file-name)))))
  "Project root directory (parent of test/).")

(defun markdown-table-wrap-test--fixture-tables ()
  "Extract pipe tables from bench/fixtures/tables.md.
Return a list of table strings.  Each table is a contiguous block
of lines starting with |."
  (let ((file (expand-file-name "bench/fixtures/tables.md"
                                markdown-table-wrap-test--project-root)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((tables nil) (current nil))
        (dolist (line (split-string (buffer-string) "\n"))
          (if (string-match-p "^|" (string-trim line))
              (push line current)
            (when current
              (push (mapconcat #'identity (nreverse current) "\n") tables)
              (setq current nil))))
        (when current
          (push (mapconcat #'identity (nreverse current) "\n") tables))
        (nreverse tables)))))

(defun markdown-table-wrap-test--extract-cells (line)
  "Extract trimmed cell strings from a pipe-table LINE.
Split on | characters.  Does not handle escaped pipes in cell
content (none present in fixture tables).
Return a list of strings, or nil if LINE is not a table row."
  (when (and (> (length line) 0) (= (aref line 0) ?|))
    (let ((parts (split-string line "|")))
      (when (> (length parts) 2)
        (mapcar #'string-trim (butlast (cdr parts)))))))

(defun markdown-table-wrap-test--find-orphan-delimiter (cell)
  "Return first token in CELL with unrecognized markup delimiters, or nil.
A token is an orphan if it contains * or ~ characters but is not
recognized as a matched markup span by `markup-span-parts'.
Such tokens could cause bold/italic/strikethrough to leak across
column boundaries when rendered by a markdown fontifier."
  (let ((trimmed (string-trim cell)))
    (unless (string-empty-p trimmed)
      (cl-find-if
       (lambda (tok)
         (and (string-match-p "[*~]" tok)
              (not (markdown-table-wrap--markup-span-parts tok))))
       (markdown-table-wrap--tokenize-cell-text trimmed)))))

;;;; Parsing

(ert-deftest markdown-table-wrap-test-parse-simple ()
  "Parse a simple 2-column table into headers, aligns, and rows."
  (let ((parsed (markdown-table-wrap-parse
                 "| A | B |\n|---|---|\n| 1 | 2 |")))
    (should (equal (nth 0 parsed) '("A" "B")))
    (should (equal (nth 1 parsed) '(nil nil)))
    (should (equal (nth 2 parsed) '(("1" "2"))))))

(ert-deftest markdown-table-wrap-test-parse-alignment ()
  "Parse alignment markers: left, right, center, none."
  (let ((parsed (markdown-table-wrap-parse
                 "| L | R | C | D |\n|:---|---:|:---:|---|\n| a | b | c | d |")))
    (should (equal (nth 1 parsed) '(left right center nil)))))

(ert-deftest markdown-table-wrap-test-parse-multiple-rows ()
  "Parse table with multiple data rows."
  (let ((parsed (markdown-table-wrap-parse
                 "| H |\n|---|\n| r1 |\n| r2 |\n| r3 |")))
    (should (= (length (nth 2 parsed)) 3))))

(ert-deftest markdown-table-wrap-test-parse-escaped-pipe ()
  "Escaped pipe in cell content is not treated as a column separator."
  (let ((parsed (markdown-table-wrap-parse
                 "| A | B |\n|---|---|\n| a \\| b | c |")))
    (should (equal (nth 0 parsed) '("A" "B")))
    (should (equal (nth 2 parsed) '(("a \\| b" "c"))))))

(ert-deftest markdown-table-wrap-test-parse-escaped-pipe-header ()
  "Escaped pipe in header cell is preserved."
  (let ((parsed (markdown-table-wrap-parse
                 "| X \\| Y | Z |\n|---|---|\n| 1 | 2 |")))
    (should (equal (nth 0 parsed) '("X \\| Y" "Z")))
    (should (equal (nth 2 parsed) '(("1" "2"))))))

(ert-deftest markdown-table-wrap-test-parse-multiple-escaped-pipes ()
  "Multiple escaped pipes in one row are all preserved."
  (let ((parsed (markdown-table-wrap-parse
                 "| A | B |\n|---|---|\n| a \\| b \\| c | d |")))
    (should (equal (nth 2 parsed) '(("a \\| b \\| c" "d"))))))

;;;; Word Wrapping

(ert-deftest markdown-table-wrap-test-cell-short-text ()
  "Text shorter than width returns a single line."
  (should (equal (markdown-table-wrap-cell "hello" 20) '("hello"))))

(ert-deftest markdown-table-wrap-test-cell-at-word-boundary ()
  "Text wraps at word boundaries when exceeding width."
  (let ((lines (markdown-table-wrap-cell "hello world foo" 11)))
    (should (equal lines '("hello world" "foo")))))

(ert-deftest markdown-table-wrap-test-cell-force-break ()
  "Long unbroken words are force-broken at column width."
  (let ((lines (markdown-table-wrap-cell "abcdefghij" 5)))
    (should (= (length lines) 2))
    (should (equal (car lines) "abcde"))))

(ert-deftest markdown-table-wrap-test-cell-empty-text ()
  "Empty text returns a single empty-string line."
  (should (equal (markdown-table-wrap-cell "" 10) '(""))))

(ert-deftest markdown-table-wrap-test-cell-single-long-word ()
  "A single long word is broken into multiple chunks."
  (let ((lines (markdown-table-wrap-cell "abcdefghijklmnop" 5)))
    (should (>= (length lines) 3))
    (dolist (line lines)
      (should (<= (string-width line) 5)))))

;;;; Force-Break Internals

(ert-deftest markdown-table-wrap-test-force-break-plain-ascii-url ()
  "ASCII URL is force-broken at exact width boundaries.
A 30-char ASCII string at width 15 produces exactly two chunks."
  (let ((chunks (markdown-table-wrap--force-break-plain
                 "https://example.com/long/path/" 15)))
    (should (= (length chunks) 2))
    (should (equal (car chunks) "https://example"))
    (should (equal (cadr chunks) ".com/long/path/"))
    (dolist (chunk chunks)
      (should (<= (string-width chunk) 15)))))

(ert-deftest markdown-table-wrap-test-force-break-plain-ascii-exact ()
  "ASCII word of exact width produces a single chunk."
  (let ((chunks (markdown-table-wrap--force-break-plain "abcde" 5)))
    (should (equal chunks '("abcde")))))

(ert-deftest markdown-table-wrap-test-force-break-plain-ascii-long ()
  "Long ASCII word broken into correct number of chunks."
  (let ((chunks (markdown-table-wrap--force-break-plain
                 "abcdefghijklmnopqrstuvwxyz" 10)))
    (should (equal chunks '("abcdefghij" "klmnopqrst" "uvwxyz")))
    (dolist (chunk (butlast chunks))
      (should (= (length chunk) 10)))))

(ert-deftest markdown-table-wrap-test-force-break-plain-cjk ()
  "CJK double-width characters are force-broken respecting display width.
Each CJK char is display-width 2, so at width 6 we fit 3 chars."
  (let ((chunks (markdown-table-wrap--force-break-plain "漢字東京大学" 6)))
    ;; 6 chars × 2 display-width = 12 total, at width 6: two chunks of 3
    (should (= (length chunks) 2))
    (should (equal (car chunks) "漢字東"))
    (should (equal (cadr chunks) "京大学"))
    (dolist (chunk chunks)
      (should (<= (string-width chunk) 6)))))

(ert-deftest markdown-table-wrap-test-force-break-plain-mixed-width ()
  "Mixed ASCII and accented characters break at correct boundaries.
Accented chars like á are single display-width, so position = width."
  (let ((chunks (markdown-table-wrap--force-break-plain "Chávez-López" 6)))
    (should (equal chunks '("Chávez" "-López")))))

(ert-deftest markdown-table-wrap-test-force-break-plain-strip-markup ()
  "Force-break-plain produces identical results with strip-markup.
Plain words (non-markup tokens) are unaffected by strip-markup,
so results must be the same regardless of the setting."
  (let ((word "https://example.com/path")
        (width 10))
    (let ((without-strip (markdown-table-wrap--force-break-plain word width))
          (with-strip (let ((markdown-table-wrap--strip-markup t))
                        (markdown-table-wrap--force-break-plain word width))))
      (should (equal without-strip with-strip)))))

(ert-deftest markdown-table-wrap-test-force-break-plain-vs16-emoji ()
  "Words containing VS16 emoji are force-broken respecting display width.
VS16 (U+FE0F) promotes base characters to 2-wide emoji presentation.
Each ☺️ sequence is display-width 2 via our VS16 correction."
  ;; Build a word: "x" + ☺ + VS16 + "y" + ☺ + VS16 + "z"
  ;; display-width: x(1) + ☺️(2) + y(1) + ☺️(2) + z(1) = 7
  (let* ((smiley (concat (string #x263A) (string #xFE0F))) ; ☺️
         (word (concat "x" smiley "y" smiley "z"))
         (chunks (markdown-table-wrap--force-break-plain word 4)))
    ;; Each chunk should fit within display-width 4
    (dolist (chunk chunks)
      (should (<= (markdown-table-wrap--display-width chunk) 4)))
    ;; Verify the total content is preserved
    (should (equal (apply #'concat chunks) word))))

;;;; Table Metrics

(ert-deftest markdown-table-wrap-test-compute-metrics-returns-vectors ()
  "Table metrics returns natural-widths as a vector."
  (let* ((text "| Foo | Bar |\n|---|---|\n| Hello world | Baz |")
         (parsed (markdown-table-wrap-parse text))
         (headers (nth 0 parsed))
         (rows (nth 2 parsed))
         (metrics (markdown-table-wrap-compute-table-metrics
                   headers rows (length headers))))
    (should (vectorp (plist-get metrics :natural-widths)))
    (should (= (length (plist-get metrics :natural-widths)) 2))))

(ert-deftest markdown-table-wrap-test-metrics-produce-same-widths ()
  "Using pre-computed metrics produces identical column widths."
  (let* ((text "| Foo | Bar |\n|---|---|\n| Hello world | Baz |")
         (parsed (markdown-table-wrap-parse text))
         (headers (nth 0 parsed))
         (rows (nth 2 parsed))
         (num-cols (length headers))
         (widths-direct (markdown-table-wrap-compute-widths
                         headers rows 40 num-cols))
         (metrics (markdown-table-wrap-compute-table-metrics
                   headers rows num-cols))
         (widths-from-metrics (markdown-table-wrap-distribute-widths
                               metrics 40 num-cols)))
    (should (equal widths-direct widths-from-metrics))))

;;;; Batch Rendering

(ert-deftest markdown-table-wrap-test-batch-matches-individual ()
  "Batch rendering produces identical output to individual calls."
  (let ((text "| Foo | Bar |\n|---|---|\n| Hello world | Baz |")
        (widths '(30 40 50 60 80)))
    (let ((batch-results (markdown-table-wrap-batch text widths))
          (individual-results (mapcar (lambda (w) (markdown-table-wrap text w))
                                     widths)))
      (should (equal batch-results individual-results)))))

(ert-deftest markdown-table-wrap-test-batch-single-width ()
  "Batch with a single width produces a one-element list."
  (let* ((text "| A | B |\n|---|---|\n| x | y |")
         (result (markdown-table-wrap-batch text '(40))))
    (should (= (length result) 1))
    (should (equal (car result) (markdown-table-wrap text 40)))))

;;;; Markup-Aware Wrapping

(ert-deftest markdown-table-wrap-test-cell-link-stays-intact ()
  "Link uses inner text only when marker overhead exceeds column.
Raw width 36 > column 20, marker overhead 23 > 20.  The URL is
discarded and only the link text (API Reference) is kept.  At 13
chars, it fits on one line within column 20."
  (let ((lines (markdown-table-wrap-cell
                "[API Reference](https://example.com)" 20)))
    (should (= (length lines) 1))
    (should (equal (car lines) "API Reference"))
    (should-not (string-match-p "](" (car lines)))))

(ert-deftest markdown-table-wrap-test-cell-bold-wraps-by-raw-width ()
  "Bold span wraps when raw width exceeds column (no-strip default).
Raw width of **very important text** is 23, exceeding column 20."
  (let ((lines (markdown-table-wrap-cell "**very important text**" 20)))
    ;; Raw 23 > 20, so force-break: marker overhead 4, inner-width 16
    ;; Inner \"very important text\" (19 chars) wraps at 16
    (should (> (length lines) 1))
    (dolist (line lines)
      (should (string-prefix-p "**" line))
      (should (string-suffix-p "**" line)))))

(ert-deftest markdown-table-wrap-test-cell-bold-stays-intact-with-strip ()
  "Bold span stays on one line when stripped width fits (strip-markup t)."
  (let ((markdown-table-wrap--strip-markup t))
    (let ((lines (markdown-table-wrap-cell "**very important text**" 20)))
      ;; Stripped width is 19 (\"very important text\"), fits in 20
      (should (= (length lines) 1))
      (should (equal (car lines) "**very important text**")))))

(ert-deftest markdown-table-wrap-test-cell-code-stays-intact ()
  "Code span stays on one line when raw width fits column."
  (let ((lines (markdown-table-wrap-cell "`npm install typescript`" 25)))
    ;; Raw width 24 <= column 25, fits as-is
    (should (= (length lines) 1))
    (should (equal (car lines) "`npm install typescript`"))))

(ert-deftest markdown-table-wrap-test-cell-strikethrough-wraps-by-raw-width ()
  "Strikethrough wraps when raw width exceeds column (no-strip default).
Raw width of ~~old deprecated API~~ is 22, exceeding column 20."
  (let ((lines (markdown-table-wrap-cell "~~old deprecated API~~" 20)))
    ;; Raw 22 > 20, force-break: marker overhead 4, inner-width 16
    (should (> (length lines) 1))
    (dolist (line lines)
      (should (string-prefix-p "~~" line))
      (should (string-suffix-p "~~" line)))))

(ert-deftest markdown-table-wrap-test-cell-strikethrough-stays-intact-with-strip ()
  "Strikethrough stays on one line when stripped width fits."
  (let ((markdown-table-wrap--strip-markup t))
    (let ((lines (markdown-table-wrap-cell "~~old deprecated API~~" 20)))
      (should (= (length lines) 1))
      (should (equal (car lines) "~~old deprecated API~~")))))

(ert-deftest markdown-table-wrap-test-cell-mixed-markup-and-plain ()
  "Mixed plain text and markup spans wrap correctly at boundaries."
  (let ((lines (markdown-table-wrap-cell
                "Use **bold text** and `code here` for emphasis" 20)))
    ;; Each markup span should appear intact in some line
    (let ((joined (mapconcat #'identity lines " ")))
      (should (string-match-p "\\*\\*bold text\\*\\*" joined))
      (should (string-match-p "`code here`" joined)))))

(ert-deftest markdown-table-wrap-test-cell-bold-force-break ()
  "Bold span wider than column gets markers duplicated on each line."
  (let ((lines (markdown-table-wrap-cell "**very important text**" 10)))
    ;; Raw width 23 > column 10, must force-break.
    ;; Each line should be valid bold (opens and closes with **)
    (should (> (length lines) 1))
    (dolist (line lines)
      (should (string-prefix-p "**" line))
      (should (string-suffix-p "**" line)))))

(ert-deftest markdown-table-wrap-test-cell-link-force-break-keeps-intact ()
  "Link whose URL overhead exceeds column uses inner text only.
The markdown link syntax is sacrificed to fit within column width;
the URL is discarded and only the link text is force-broken.
This prevents ](url) fragments from appearing in the output, which
would trigger catastrophic regex backtracking in markdown-mode's
font-lock."
  (let ((lines (markdown-table-wrap-cell
                "[API Reference Guide](https://example.com)" 10)))
    ;; Marker overhead (27 chars) > column (10), so link markup dropped
    (should (> (length lines) 1))
    (dolist (line lines)
      (should (<= (markdown-table-wrap-visible-width line) 10))
      ;; No ](url) fragments in the output
      (should-not (string-match-p "](" line)))))

(ert-deftest markdown-table-wrap-test-cell-link-force-break-wide-column ()
  "Link in wide-enough column duplicates [text](url) per line."
  (let ((lines (markdown-table-wrap-cell
                "[API Reference Guide](https://x.co)" 15)))
    ;; URL overhead = 1 + 14 = 15.  Column = 15, inner = 0 -> keep intact.
    ;; But with shorter URL where overhead < column, it splits.
    ;; Use very short URL so overhead is small.
    (setq lines (markdown-table-wrap-cell
                 "[API Reference Guide](u)" 12))
    ;; overhead = 1 + 4 = 5.  inner-width = 7.
    ;; "API" (3) fits, "Reference" (9) > 7, force-break, "Guide" (5) fits
    (should (> (length lines) 1))
    (dolist (line lines)
      (should (string-match-p "^\\[.*\\](u)$" line)))))

(ert-deftest markdown-table-wrap-test-cell-code-force-break ()
  "Code span wider than column gets backticks duplicated on each line."
  (let ((lines (markdown-table-wrap-cell
                "`npm install --save-dev typescript`" 12)))
    ;; Raw width 34 > column 12, must force-break.
    (should (> (length lines) 1))
    (dolist (line lines)
      (should (string-prefix-p "`" line))
      (should (string-suffix-p "`" line)))))

;;;; Image Markup Handling

(ert-deftest markdown-table-wrap-test-cell-image-stays-intact ()
  "Image uses alt text only when marker overhead exceeds column.
The URL is discarded; only the alt text (Dashboard) is kept.  At
9 chars, it fits on one line within column 15."
  (let ((lines (markdown-table-wrap-cell
                "![Dashboard](https://cdn.example.com/img.png)" 15)))
    (should (= (length lines) 1))
    (should (equal (car lines) "Dashboard"))
    (should-not (string-match-p "](" (car lines)))))

(ert-deftest markdown-table-wrap-test-cell-image-force-break ()
  "Image force-break duplicates ![...](url) markers per line."
  (let ((lines (markdown-table-wrap-cell
                "![A long alt text here](u)" 15)))
    ;; Marker overhead: ![ (2) + ](u) (4) = 6.  Inner-width = 15-6 = 9.
    ;; "A long alt text here" (20 chars) wraps at 9
    (should (> (length lines) 1))
    (dolist (line lines)
      (should (string-prefix-p "![" line))
      (should (string-suffix-p "](u)" line)))))

(ert-deftest markdown-table-wrap-test-strip-markup-image ()
  "strip-markup removes image syntax, leaving alt text."
  (should (equal (markdown-table-wrap-strip-markup
                  "![Dashboard](https://cdn.example.com/img.png)")
                 "Dashboard")))

;;;; Force-Break Markup: Trailing Punctuation and Strip-Markup Overhead

(ert-deftest markdown-table-wrap-test-force-break-trailing-only-on-last-line ()
  "Trailing punctuation after markup should appear only on the last line.
When **constitutional structure**, is force-broken, the comma belongs
on the last continuation line, not duplicated on every line."
  (let ((lines (markdown-table-wrap-cell "**constitutional structure**," 12)))
    ;; Must force-break: raw width 29 > column 12
    (should (> (length lines) 1))
    ;; Only the last line should end with **,  — others end with ** only
    (let ((last-line (car (last lines)))
          (non-last (butlast lines)))
      (should (string-suffix-p "**," last-line))
      (dolist (line non-last)
        (should (string-suffix-p "**" line))
        (should-not (string-suffix-p "**," line))))))

(ert-deftest markdown-table-wrap-test-force-break-strip-markup-bold-overhead ()
  "In strip-markup mode, bold markers are invisible: overhead is 0.
**very long bold text** at column 10 should wrap so that every line's
visible (stripped) width fits within 10."
  (let ((markdown-table-wrap--strip-markup t))
    (let ((lines (markdown-table-wrap-cell "**very long bold text**" 10)))
      (should (> (length lines) 1))
      (dolist (line lines)
        (should (<= (markdown-table-wrap-visible-width line) 10))))))

(ert-deftest markdown-table-wrap-test-force-break-strip-markup-bold-italic-overhead ()
  "In strip-markup mode, bold-italic ***text*** keeps italic * visible.
markdown-mode hides ** (bold) but keeps * (italic), so visible overhead
is 2 (one * on each side).  Each line's stripped width must fit column."
  (let ((markdown-table-wrap--strip-markup t))
    (let ((lines (markdown-table-wrap-cell "***Democracy for the Few***" 11)))
      ;; Stripped content: *Democracy for the Few* = 24 chars > 11
      ;; Visible overhead: 2 (* on each side), so inner fits in 9
      (should (> (length lines) 1))
      (dolist (line lines)
        (should (<= (markdown-table-wrap-visible-width line) 11))))))

(ert-deftest markdown-table-wrap-test-force-break-strip-markup-trailing-punct ()
  "In strip-markup mode, trailing punctuation after bold is visible.
**structure**, stripped → structure, — the comma is 1 visible char.
Each line's stripped width must account for this."
  (let ((markdown-table-wrap--strip-markup t))
    (let ((lines (markdown-table-wrap-cell "**constitutional structure**," 12)))
      (should (> (length lines) 1))
      (dolist (line lines)
        (should (<= (markdown-table-wrap-visible-width line) 12))))))

(ert-deftest markdown-table-wrap-test-force-break-strip-markup-italic-wrapping-bold ()
  "In strip-markup mode, italic wrapping bold creates visible * markers.
When *...text...**persistent omission**...text...* is force-broken,
continuation lines like ***persistent omission*** have visible italic
markers (2 extra chars) because * + ** = *** keeps the italic * visible.
Each line's stripped width must account for this overhead."
  (let ((markdown-table-wrap--strip-markup t))
    ;; Simulate italic span containing bold inner content
    ;; prefix=* inner=includes **bold**, suffix=*
    (let ((lines (markdown-table-wrap-cell
                  "*some text **persistent omission** of evidence*" 19)))
      (should (> (length lines) 1))
      (dolist (line lines)
        (should (<= (markdown-table-wrap-visible-width line) 19))))))

;;;; Tokenizer: Trailing Punctuation After Markup Spans

(ert-deftest markdown-table-wrap-test-tokenize-trailing-paren ()
  "Closing paren attached to a markup span stays with the span.
`[mypy](url))` should tokenize as one unit, not `[mypy](url)` + `)`."
  (let ((tokens (markdown-table-wrap--tokenize-cell-text
                 "hints via [mypy](https://mypy.readthedocs.io))")))
    ;; The `)` after the link should not be a separate token
    (should-not (member ")" tokens))
    ;; The link-with-paren should be one token
    (should (cl-some (lambda (tk) (string-suffix-p ")" tk)) tokens))))

(ert-deftest markdown-table-wrap-test-markup-span-parts-trailing-punct ()
  "Markup span with trailing punctuation decomposes correctly.
Trailing characters are included in the suffix, preserving
the span structure for force-breaking."
  ;; Link with trailing paren
  (let ((parts (markdown-table-wrap--markup-span-parts
                "[mypy](https://example.com))")))
    (should parts)
    (should (equal (nth 0 parts) "["))
    (should (equal (nth 1 parts) "mypy"))
    (should (equal (nth 2 parts) "](https://example.com))")))
  ;; Bold with trailing comma
  (let ((parts (markdown-table-wrap--markup-span-parts "**bold**,")))
    (should parts)
    (should (equal (nth 0 parts) "**"))
    (should (equal (nth 1 parts) "bold"))
    (should (equal (nth 2 parts) "**,")))
  ;; Code with trailing semicolon
  (let ((parts (markdown-table-wrap--markup-span-parts "`code`;")))
    (should parts)
    (should (equal (nth 0 parts) "`"))
    (should (equal (nth 1 parts) "code"))
    (should (equal (nth 2 parts) "`;")))
  ;; Clean markup span still works (no trailing punct)
  (let ((parts (markdown-table-wrap--markup-span-parts "[text](url)")))
    (should parts)
    (should (equal (nth 2 parts) "](url)"))))

;;;; Tokenizer: Double-Backtick Code Spans

(ert-deftest markdown-table-wrap-test-tokenize-double-backtick ()
  "Double-backtick code span tokenizes as a single token."
  (let ((tokens (markdown-table-wrap--tokenize-cell-text "`` `code` ``")))
    (should (= (length tokens) 1))
    (should (equal (car tokens) "`` `code` ``"))))

(ert-deftest markdown-table-wrap-test-tokenize-double-backtick-in-context ()
  "Double-backtick code span amid plain text stays as one token."
  (let ((tokens (markdown-table-wrap--tokenize-cell-text "Use `` `inner` `` here")))
    (should (= (length tokens) 3))
    (should (equal (nth 0 tokens) "Use"))
    (should (equal (nth 1 tokens) "`` `inner` ``"))
    (should (equal (nth 2 tokens) "here"))))

(ert-deftest markdown-table-wrap-test-markup-span-parts-double-backtick ()
  "Double-backtick code span decomposes into (\"``\" INNER \"``)\"."
  (let ((parts (markdown-table-wrap--markup-span-parts "`` `code` ``")))
    (should parts)
    (should (equal (nth 0 parts) "`` "))
    (should (equal (nth 1 parts) "`code`"))
    (should (equal (nth 2 parts) " ``"))))

;;;; Multi-Word Italic and Bold-Italic Spans

(ert-deftest markdown-table-wrap-test-tokenize-multi-word-italic ()
  "Multi-word italic spans like *St. Martin's Press* stay as one token."
  (let ((tokens (markdown-table-wrap--tokenize-cell-text
                 "*St. Martin's Press* (9 editions)")))
    (should (equal (car tokens) "*St. Martin's Press*"))
    (should (equal (nth 1 tokens) "(9"))
    (should (equal (nth 2 tokens) "editions)"))))

(ert-deftest markdown-table-wrap-test-tokenize-bold-italic ()
  "Bold-italic spans ***text*** stay as one token."
  (should (equal (markdown-table-wrap--tokenize-cell-text
                  "***Democracy for the Few***")
                 '("***Democracy for the Few***"))))

(ert-deftest markdown-table-wrap-test-markup-span-parts-bold-italic ()
  "Bold-italic spans decompose into (\"***\" INNER \"***\")."
  (let ((parts (markdown-table-wrap--markup-span-parts
                "***Democracy for the Few***")))
    (should parts)
    (should (equal (nth 0 parts) "***"))
    (should (equal (nth 1 parts) "Democracy for the Few"))
    (should (equal (nth 2 parts) "***"))))

(ert-deftest markdown-table-wrap-test-markup-span-parts-multi-word-italic ()
  "Multi-word italic decomposes into (\"*\" INNER \"*\")."
  (let ((parts (markdown-table-wrap--markup-span-parts
                "*St. Martin's Press*")))
    (should parts)
    (should (equal (nth 0 parts) "*"))
    (should (equal (nth 1 parts) "St. Martin's Press"))
    (should (equal (nth 2 parts) "*"))))

(ert-deftest markdown-table-wrap-test-tokenize-two-italic-spans ()
  "Two separate italic spans in one cell are tokenized independently."
  (let ((tokens (markdown-table-wrap--tokenize-cell-text
                 "*quick* brown *fox*")))
    (should (equal tokens '("*quick*" "brown" "*fox*")))))

(ert-deftest markdown-table-wrap-test-tokenize-italic-with-nested-bold ()
  "Italic span containing nested **bold** is tokenized as a single span."
  (let ((tokens (markdown-table-wrap--tokenize-cell-text
                 "*italic with **bold** inside*")))
    (should (equal tokens '("*italic with **bold** inside*")))))

(ert-deftest markdown-table-wrap-test-span-parts-italic-with-nested-bold ()
  "Italic span with nested bold returns correct prefix/inner/suffix."
  (let ((parts (markdown-table-wrap--markup-span-parts
                "*italic with **bold** inside*")))
    (should (equal parts '("*" "italic with **bold** inside" "*")))))

(ert-deftest markdown-table-wrap-test-tokenize-cjk-with-markup ()
  "CJK text with embedded markup splits at markup boundaries.
CJK has no whitespace between words, so the tokenizer must split
at markup delimiters to produce multiple manageable tokens."
  (let ((tokens (markdown-table-wrap--tokenize-cell-text
                 "年由*出版社*翻译**大学**中")))
    ;; Should produce: "年由" "*出版社*" "翻译" "**大学**" "中"
    (should (equal tokens '("年由" "*出版社*" "翻译" "**大学**" "中")))))

(ert-deftest markdown-table-wrap-test-tokenize-cjk-with-backticks ()
  "CJK text with inline code splits at backtick boundaries.
Backtick spans become separate tokens, preventing force-break
from cutting through backtick boundaries (which triggers the
backtick parity guard)."
  (let ((tokens (markdown-table-wrap--tokenize-cell-text
                 "被**大学**和`复旦大学`政治学")))
    ;; "被" "**大学**" "和" "`复旦大学`" "政治学"
    (should (member "`复旦大学`" tokens))
    (should (member "**大学**" tokens))))

(ert-deftest markdown-table-wrap-test-bold-italic-force-break ()
  "Bold-italic spans re-apply ***...*** markers on each wrapped fragment."
  (let ((lines (markdown-table-wrap-cell "***Democracy for the Few***" 15)))
    ;; Each line should have balanced *** markers
    (dolist (line lines)
      (when (string-match-p "\\*" line)
        (should (string-prefix-p "***" line))
        (should (string-suffix-p "***" line))))))

;;;; Graceful Markup Degradation at Narrow Widths

(ert-deftest markdown-table-wrap-test-cell-bold-degrades-at-narrow-width ()
  "Bold markers are dropped when they would consume more than half each line.
**hello world** at width 5: marker-overhead 4, inner-width 1 < 4.
Markers are dropped; inner text wraps as plain text."
  (let ((lines (markdown-table-wrap-cell "**hello world**" 5)))
    (should (equal lines '("hello" "world")))
    (dolist (line lines)
      (should-not (string-match-p "\\*\\*" line))
      (should (<= (string-width line) 5)))))

(ert-deftest markdown-table-wrap-test-cell-code-degrades-at-narrow-width ()
  "Code backticks are dropped when they would consume more than half each line.
`hello world` at width 3: marker-overhead 2, inner-width 1 < 2.
Backticks are dropped; inner text wraps as plain text."
  (let ((lines (markdown-table-wrap-cell "`hello world`" 3)))
    (dolist (line lines)
      (should-not (string-match-p "`" line))
      (should (<= (string-width line) 3)))
    (should (equal (string-join lines "") "helloworld"))))

(ert-deftest markdown-table-wrap-test-cell-strikethrough-degrades-at-narrow-width ()
  "Strikethrough markers are dropped when they dominate each line.
~~old deprecated~~ at width 6: marker-overhead 4, inner-width 2 < 4.
Tildes are dropped; inner text wraps as plain text."
  (let ((lines (markdown-table-wrap-cell "~~old deprecated~~" 6)))
    (dolist (line lines)
      (should-not (string-match-p "~~" line))
      (should (<= (string-width line) 6)))
    ;; All inner content is present
    (should (equal (string-join lines " ") "old deprec ated"))))

(ert-deftest markdown-table-wrap-test-cell-bold-italic-degrades-at-narrow-width ()
  "Bold-italic markers are dropped when they dominate each line.
***Democracy for the Few*** at width 10: marker-overhead 6,
inner-width 4 < 6.  Stars are dropped; inner text wraps as plain."
  (let ((lines (markdown-table-wrap-cell "***Democracy for the Few***" 10)))
    (dolist (line lines)
      (should-not (string-match-p "\\*" line))
      (should (<= (string-width line) 10)))
    ;; Verify content is preserved
    (let ((joined (string-join lines " ")))
      (should (string-match-p "Democracy" joined))
      (should (string-match-p "Few" joined)))))

(ert-deftest markdown-table-wrap-test-cell-bold-with-trailing-punct-degrades ()
  "Trailing punctuation survives markup degradation.
**bold**, at width 5: markers dropped, comma on last line."
  (let ((lines (markdown-table-wrap-cell "**bold**," 5)))
    (should-not (string-match-p "\\*\\*" (string-join lines "")))
    ;; Comma must appear on the last line
    (should (string-suffix-p "," (car (last lines))))
    (dolist (line lines)
      (should (<= (string-width line) 5)))))

(ert-deftest markdown-table-wrap-test-cell-bold-with-multi-char-trailing-punct ()
  "Multi-character trailing punctuation survives degradation without overflow.
**bold**)); at width 6: markers dropped, trailing )); included in content."
  (let ((lines (markdown-table-wrap-cell "**bold**));" 6)))
    (should-not (string-match-p "\\*\\*" (string-join lines "")))
    ;; All trailing chars must appear somewhere
    (let ((joined (string-join lines "")))
      (should (string-match-p "));" joined)))
    (dolist (line lines)
      (should (<= (string-width line) 6)))))

(ert-deftest markdown-table-wrap-test-cell-bold-does-not-degrade-above-threshold ()
  "Bold markers are preserved when inner-width >= marker-overhead.
**bold text** at width 12: marker-overhead 4, inner-width 8 >= 4.
Markers should be kept."
  (let ((lines (markdown-table-wrap-cell "**bold text**" 12)))
    (dolist (line lines)
      (should (string-prefix-p "**" line))
      (should (string-suffix-p "**" line)))))

(ert-deftest markdown-table-wrap-test-table-render-narrow-degrades-gracefully ()
  "Full table at narrow width degrades markup rather than producing 1-char lines.
No output cell should have marker characters consuming more than half its width."
  (let* ((md "| A | B |\n| --- | --- |\n| **important data** | **critical info** |")
         (result (markdown-table-wrap md 18))
         (lines (split-string result "\n" t)))
    ;; The table should render without error
    (should (stringp result))
    ;; No single-character bold lines like **i**
    (dolist (line lines)
      (let ((cells (markdown-table-wrap-test--extract-cells line)))
        (dolist (cell cells)
          (let ((trimmed (string-trim cell)))
            (when (and (not (string-empty-p trimmed))
                       (string-match-p "\\*\\*" trimmed))
              ;; If bold markers are present, content should be > marker overhead
              (let* ((marker-len 4) ; ** on each side
                     (content-len (- (string-width trimmed) marker-len)))
                (should (>= content-len marker-len))))))))))

;;;; Strip-Markup Marker Overhead in Force-Break

(ert-deftest markdown-table-wrap-test-cell-bold-strip-uses-full-width ()
  "With strip-markup, bold markers are invisible — inner text gets full width.
When `markdown-hide-markup' is active, **text** displays as just \"text\".
Force-breaking should allocate the full column width to inner text, not
subtract marker overhead (which is invisible)."
  (let ((markdown-table-wrap--strip-markup t))
    (let ((lines (markdown-table-wrap-cell "**Typing discipline**" 7)))
      ;; Visible width of inner text: \"Typing discipline\" = 17 > 7, must wrap.
      ;; With strip: markers invisible, inner gets full 7 chars.
      ;; Expected: \"**Typing**\" (vis 6) / \"**discipl**\" (vis 7) / \"**ine**\" (vis 3)
      ;; Bug: marker-overhead=4 subtracted, inner gets 3 chars → 3-char fragments.
      (should (equal (markdown-table-wrap-strip-markup (nth 0 lines)) "Typing"))
      (should (equal (markdown-table-wrap-strip-markup (nth 1 lines)) "discipl"))
      (should (equal (markdown-table-wrap-strip-markup (nth 2 lines)) "ine")))))

(ert-deftest markdown-table-wrap-test-cell-link-strip-uses-full-width ()
  "With strip-markup, link syntax is invisible — inner text gets full width."
  (let ((markdown-table-wrap--strip-markup t))
    (let ((lines (markdown-table-wrap-cell "[click here](http://example.com)" 6)))
      ;; Visible: \"click here\" = 10 > 6, must wrap.
      ;; With strip: [](url) invisible, inner gets full 6 chars.
      ;; Expected: \"[click](url)\" (vis 5) / \"[here](url)\" (vis 4)
      ;; Bug: overhead=20 subtracted, inner-width < 1, kept intact as one line.
      (should (= (length lines) 2))
      (should (equal (markdown-table-wrap-strip-markup (nth 0 lines)) "click"))
      (should (equal (markdown-table-wrap-strip-markup (nth 1 lines)) "here")))))

(ert-deftest markdown-table-wrap-test-render-bold-data-strip-fills-column ()
  "Bold data cells use full column width with strip-markup, not col - 4."
  (let* ((md "| Name |\n|---|\n| **Typing discipline** |")
         (result (markdown-table-wrap md 12 nil t)))
    ;; Column width for 1 col at width 12: 12 - 4(border) = 8.
    ;; \"Typing discipline\" vis = 17 > 8, wraps.
    ;; With strip: wrap at 8, NOT at 8-4=4.
    ;; Expected: \"**Typing**\" / \"**discipli**\" / \"**ne**\"
    ;; Bug: wraps at 4: \"**Typi**\" / \"**ng**\" / \"**disc**\" etc.
    (should (string-match-p "\\*\\*Typing\\*\\*" result))
    (should-not (string-match-p "\\*\\*Typi\\*\\*" result))))

;;;; Column Width Computation

(ert-deftest markdown-table-wrap-test-widths-fit-naturally ()
  "Columns that fit naturally get at least their natural widths."
  (let ((widths (markdown-table-wrap-compute-widths
                 '("A" "B") '(("x" "y")) 20 2)))
    ;; Border overhead for 2 cols: 3*2+1 = 7.  Available: 13.
    (should (= (length widths) 2))
    (should (>= (nth 0 widths) 1))
    (should (>= (nth 1 widths) 1))))

(ert-deftest markdown-table-wrap-test-widths-shrinking ()
  "Columns shrink proportionally when table exceeds available width."
  (let ((widths (markdown-table-wrap-compute-widths
                 '("Command" "Description")
                 '(("npm install" "Install all project dependencies from package.json"))
                 40 2)))
    ;; Must fit in 40 chars. Border overhead: 7.  Available for cells: 33.
    (should (<= (+ (nth 0 widths) (nth 1 widths)) 33))
    (should (> (nth 0 widths) 3))
    (should (> (nth 1 widths) 3))))

;; Shared test fixture: 4-column table with diverse natural widths.
;; Natural widths: [46 22 29 85].
(defconst markdown-table-wrap-test--wide-headers
  '("App" "Model" "Candidate" "Rec"))
(defconst markdown-table-wrap-test--wide-rows
  '(("Basic validation agents (basic_epd, basic_lca)"
     "gemini-3-flash-preview"
     "gemini-3.1-flash-lite-preview"
     "Test via eval - could reduce cost for simple QA, but verify accuracy does not regress")))

(ert-deftest markdown-table-wrap-test-widths-content-rich-column-gets-more ()
  "A column with more content gets proportionally more width.
Sqrt-proportional allocation ensures that a column with 85 chars
of wrappable prose gets more space than one with only 22 chars,
even when the narrower column contains a long unbreakable token."
  (let ((widths (markdown-table-wrap-compute-widths
                 markdown-table-wrap-test--wide-headers
                 markdown-table-wrap-test--wide-rows
                 100 4)))
    ;; Rec (natural=85) must get more width than Model (natural=22).
    (should (> (nth 3 widths) (nth 1 widths)))))

(ert-deftest markdown-table-wrap-test-widths-small-columns-favored ()
  "Small columns receive proportionally more of their natural width.
Sqrt dampening naturally favors narrow columns: a column with
natural width 1 keeps its full width, and a date column (nat=10)
retains most of it even when wider columns are heavily compressed."
  (let ((widths (markdown-table-wrap-compute-widths
                 '("#" "Task" "Due" "Notes")
                 '(("1" "Implement authentication flow with SSO" "2026-03-20"
                    "Blocked on identity provider configuration"))
                 60 4)))
    ;; "#" (nat=1) should keep its full width — it's already minimal.
    (should (= (nth 0 widths) 1))
    ;; "Due" (nat=10) should keep most of its width (>= 80%).
    (should (>= (nth 2 widths) 8))
    ;; "Task" and "Notes" are wider and should be compressed more.
    (should (< (/ (float (nth 1 widths)) 39.0)
               (/ (float (nth 2 widths)) 10.0)))))

(ert-deftest markdown-table-wrap-test-widths-sum-uses-full-budget ()
  "When shrinking, column widths sum to exactly the cell budget.
Border overhead is 3×num-cols+1.  No space should be wasted when
columns are compressed to fit."
  (dolist (available '(60 80 100 120))
    (let* ((num-cols 4)
           (widths (markdown-table-wrap-compute-widths
                    markdown-table-wrap-test--wide-headers
                    markdown-table-wrap-test--wide-rows
                    available num-cols))
           (border-overhead (+ (* 3 num-cols) 1))
           (cell-budget (- available border-overhead)))
      (should (= (apply #'+ widths) cell-budget)))))

(ert-deftest markdown-table-wrap-test-widths-monotonic ()
  "Every column width is monotonically non-decreasing as width grows.
Widening the terminal must never shrink any column."
  (dolist (nats '((2 12 11 93) (46 22 29 85)
                  (4 22 7 6 5 10 10 35) (18 30 16 29)))
    (let* ((headers (cl-loop for i below (length nats)
                             collect (format "C%d" i)))
           (row (make-list (length nats) "x"))
           (num-cols (length nats))
           (prev nil))
      ;; Fake natural widths by using cells of exactly those widths.
      (setq row (cl-loop for n in nats
                         collect (make-string n ?x)))
      (cl-loop for w from 20 to 150 do
               (let ((widths (markdown-table-wrap-compute-widths
                              headers (list row) w num-cols)))
                 (when prev
                   (dotimes (i num-cols)
                     (should-not
                      (< (nth i widths) (nth i prev)))))
                 (setq prev widths))))))

(ert-deftest markdown-table-wrap-test-widths-extremely-narrow ()
  "Extreme narrowing gives at least 1 char per column."
  (let ((widths (markdown-table-wrap-compute-widths
                 '("A" "B" "C") '(("x" "y" "z")) 15 3)))
    ;; Border overhead: 10.  Available: 5.
    (should (= (length widths) 3))
    (should (>= (apply #'+ widths) 3))))

;;;; Full Table Rendering

(ert-deftest markdown-table-wrap-test-render-fits ()
  "Table that fits within width renders with pipe-table syntax."
  (let* ((md "| A | B |\n|---|---|\n| x | y |")
         (result (markdown-table-wrap md 30)))
    (should (stringp result))
    (should (string-match-p "x" result))
    (should (string-match-p "y" result))
    ;; Every line starts and ends with |
    (dolist (line (split-string result "\n" t))
      (should (string-prefix-p "|" line))
      (should (string-suffix-p "|" line)))))

(ert-deftest markdown-table-wrap-test-render-wrapping ()
  "Wide table wraps cells to fit, producing more output lines."
  (let* ((md "| Name | Description |\n|---|---|\n| foo | This is a very long description that will wrap |")
         (result (markdown-table-wrap md 35)))
    ;; Should have more lines than input (wrapping happened)
    (let ((input-lines (length (split-string md "\n" t)))
          (output-lines (length (split-string result "\n" t))))
      (should (> output-lines input-lines)))
    ;; No line should exceed available width
    (dolist (line (split-string result "\n" t))
      (should (<= (string-width line) 35)))))

(ert-deftest markdown-table-wrap-test-render-inline-formatting ()
  "Inline markdown formatting is preserved in wrapped output.
With no-strip default, links may be force-broken into valid sub-links
when raw width exceeds the column."
  (let* ((md "| Name | Link |\n|---|---|\n| **bold** | [click](http://example.com) |")
         (result (markdown-table-wrap md 40)))
    ;; Bold markup preserved intact
    (should (string-match-p "\\*\\*bold\\*\\*" result))
    ;; Link syntax preserved — each link fragment is valid [text](url)
    (should (string-match-p "\\[.*\\](http://example.com)" result))))

(ert-deftest markdown-table-wrap-test-render-inline-formatting-with-strip ()
  "With strip-markup, link stays intact since visible width fits."
  (let* ((md "| Name | Link |\n|---|---|\n| **bold** | [click](http://example.com) |")
         (result (markdown-table-wrap md 40 nil t)))
    (should (string-match-p "\\*\\*bold\\*\\*" result))
    (should (string-match-p "\\[click\\]" result))))

(ert-deftest markdown-table-wrap-test-render-alignment-preserved ()
  "Alignment markers survive in wrapped output separator."
  (let* ((md "| L | R | C |\n|:---|---:|:---:|\n| a | b | c |")
         (result (markdown-table-wrap md 30)))
    (should (string-match-p ":" result))))

(ert-deftest markdown-table-wrap-test-render-alignment-applied-to-cells ()
  "Cell content is padded according to column alignment.
Right-aligned cells have leading spaces; center-aligned have both."
  (let* ((md "| Left | Right | Center |\n|:---|---:|:---:|\n| a | b | c |")
         (result (markdown-table-wrap md 40))
         (lines (split-string result "\n" t))
         ;; Data row is after separator (3rd line, index 2)
         (data-line (nth 2 lines)))
    ;; Right-aligned "b" should have leading spaces: "     b" not "b     "
    (should (string-match-p "|  +b " result))
    ;; Center-aligned "c" should have spaces on both sides
    (should (string-match-p "| +c +|" result))))

(ert-deftest markdown-table-wrap-test-render-alignment-with-wrapping ()
  "Alignment is applied even when cells wrap to multiple lines."
  (let* ((md "| Left | Right |\n|:---|---:|\n| hello world | 42 |")
         (result (markdown-table-wrap md 25))
         (lines (split-string result "\n" t)))
    ;; Find a data line containing "42" — it should be right-aligned
    (let ((line-42 (cl-find-if (lambda (l) (string-match-p "42" l)) lines)))
      (should line-42)
      ;; "42" should have leading spaces (right-aligned)
      (should (string-match-p " +42 |" line-42)))))

(ert-deftest markdown-table-wrap-test-render-empty-cells ()
  "Tables with empty cells render without error."
  (let* ((md "| A | B |\n|---|---|\n|  | val |\n| x |  |")
         (result (markdown-table-wrap md 30)))
    (should (stringp result))
    (should (string-match-p "val" result))))

(ert-deftest markdown-table-wrap-test-render-single-column ()
  "Single-column table renders correctly."
  (let* ((md "| Only |\n|---|\n| data |")
         (result (markdown-table-wrap md 20)))
    (should (stringp result))))

(ert-deftest markdown-table-wrap-test-render-many-columns-narrow ()
  "Many columns in narrow width still fit, no line exceeds width."
  (let* ((md "| A | B | C | D | E |\n|---|---|---|---|---|\n| 1 | 2 | 3 | 4 | 5 |")
         (result (markdown-table-wrap md 30)))
    (should (stringp result))
    (dolist (line (split-string result "\n" t))
      (should (<= (string-width line) 30)))))

(ert-deftest markdown-table-wrap-test-render-links-default ()
  "Links kept intact; non-link lines respect raw width constraint.
Without strip-markup, links that exceed column width overflow (they
cannot be broken without corrupting markdown syntax)."
  (let* ((md "| Issue |\n|---|\n| See [long link text here](http://example.com/very/long/path) for details |")
         (result (markdown-table-wrap md 30)))
    (should (stringp result))
    ;; Link text should be present and intact
    (should (string-match-p "long link text here" result))
    ;; Non-link, non-separator lines respect raw width
    (dolist (line (split-string result "\n" t))
      (unless (or (string-match-p "\\[.*\\](.*)" line)
                  (string-match-p "^|[-: |]+|$" line))
        (should (<= (string-width line) 30))))))

(ert-deftest markdown-table-wrap-test-render-links-with-strip ()
  "With strip-markup, visible width of all lines respects constraint.
Links overflow in raw width but fit in visible (stripped) width."
  (let* ((md "| Issue |\n|---|\n| See [long link text here](http://example.com/very/long/path) for details |")
         (result (markdown-table-wrap md 30 nil t)))
    (should (stringp result))
    (should (string-match-p "long link text here" result))
    ;; All lines fit in visible (stripped) width
    (let ((markdown-table-wrap--strip-markup t))
      (dolist (line (split-string result "\n" t))
        (should (<= (markdown-table-wrap-visible-width line) 30))))))

(ert-deftest markdown-table-wrap-test-render-fewer-cells ()
  "Rows with fewer cells than headers don't error."
  (let* ((md "| A | B | C |\n|---|---|---|\n| only-one |")
         (result (markdown-table-wrap md 30)))
    (should (stringp result))))

(ert-deftest markdown-table-wrap-test-render-consistent-line-widths ()
  "All data lines in wrapped output have the same width."
  (let* ((md "| Short | Much longer header |\n|---|---|\n| a | b |\n| longer value | c |")
         (result (markdown-table-wrap md 40)))
    (let* ((data-lines (cl-remove-if
                        (lambda (l) (string-match-p "^|[-:|[:space:]]+|$" l))
                        (split-string result "\n" t)))
           (widths (mapcar #'string-width data-lines)))
      (when (> (length widths) 1)
        (let ((first-w (car widths)))
          (dolist (w (cdr widths))
            (should (= w first-w))))))))

(ert-deftest markdown-table-wrap-test-render-returns-original-when-too-narrow ()
  "When width is impossibly narrow, return original markdown unchanged."
  (let* ((md "| A | B | C |\n|---|---|---|\n| x | y | z |")
         ;; 3 cols need border overhead of 10, plus 1 per col = 13 minimum
         ;; Width 5 is way too narrow
         (result (markdown-table-wrap md 5)))
    (should (equal result md))))

(ert-deftest markdown-table-wrap-test-render-zero-columns ()
  "Empty table (no columns) returns original text."
  (should (equal (markdown-table-wrap "" 40) "")))

;;;; Pad Cell

(ert-deftest markdown-table-wrap-test-pad-cell-left ()
  "Left-aligned (nil fmt) pads on the right."
  (should (equal (markdown-table-wrap--pad-cell "hi" 5 nil) "hi   ")))

(ert-deftest markdown-table-wrap-test-pad-cell-right ()
  "Right-aligned pads on the left."
  (should (equal (markdown-table-wrap--pad-cell "hi" 5 'right) "   hi")))

(ert-deftest markdown-table-wrap-test-pad-cell-center ()
  "Center-aligned pads both sides."
  (should (equal (markdown-table-wrap--pad-cell "hi" 6 'center) "  hi  ")))

(ert-deftest markdown-table-wrap-test-pad-cell-raw-width-default ()
  "Padding uses raw width by default — no extra padding when cell fits."
  ;; **hi** has raw width 6 > target 5, so no padding added
  (should (equal (markdown-table-wrap--pad-cell "**hi**" 5 nil) "**hi**"))
  ;; raw width 6 = target 6, no padding
  (should (equal (markdown-table-wrap--pad-cell "**hi**" 6 nil) "**hi**"))
  ;; raw width 6 < target 8, pad with 2 spaces
  (should (equal (markdown-table-wrap--pad-cell "**hi**" 8 nil) "**hi**  ")))

(ert-deftest markdown-table-wrap-test-pad-cell-stripped-width ()
  "With strip-markup, padding accounts for hidden markup."
  ;; **hi** has stripped width 2, pad to 5 = 3 spaces
  (let ((markdown-table-wrap--strip-markup t))
    (should (equal (markdown-table-wrap--pad-cell "**hi**" 5 nil)
                   "**hi**   "))))

;;;; Strip Markup

(ert-deftest markdown-table-wrap-test-strip-markup ()
  "strip-markup removes all inline formatting."
  (should (equal (markdown-table-wrap-strip-markup "**bold**") "bold"))
  (should (equal (markdown-table-wrap-strip-markup "[text](url)") "text"))
  (should (equal (markdown-table-wrap-strip-markup "`code`") "code"))
  (should (equal (markdown-table-wrap-strip-markup "~~strike~~") "strike"))
  (should (equal (markdown-table-wrap-strip-markup "plain") "plain")))

(ert-deftest markdown-table-wrap-test-strip-markup-italic-with-spaces ()
  "Italic with spaces is stripped: *italic text here* -> italic text here."
  (should (equal (markdown-table-wrap-strip-markup "*italic text here*")
                 "italic text here")))

(ert-deftest markdown-table-wrap-test-strip-markup-bold-italic ()
  "Bold-italic: ***text*** -> *text* (italic markers stay visible).
markdown-mode's `markdown-hide-markup' hides only the ** (bold)
delimiters for bold-italic spans, keeping the * (italic) visible."
  (should (equal (markdown-table-wrap-strip-markup "***bold and italic***")
                 "*bold and italic*")))

(ert-deftest markdown-table-wrap-test-strip-markup-bold-italic-split ()
  "Split bold-italic: ***text** other* keeps * markers visible.
When bold closes before italic (***text** other*), markdown-mode
hides the ** (bold) but NOT the * markers — they become visible
text.  The strip result should match: *text other*."
  (should (equal (markdown-table-wrap-strip-markup "***bold** of text*")
                 "*bold of text*"))
  (should (equal (markdown-table-wrap-strip-markup "***omission** of*")
                 "*omission of*")))

(ert-deftest markdown-table-wrap-test-strip-markup-code-protects-inner ()
  "Code spans protect content from further stripping: `**text**` -> **text**."
  (should (equal (markdown-table-wrap-strip-markup "`**text**`")
                 "**text**")))

(ert-deftest markdown-table-wrap-test-strip-markup-double-backtick ()
  "Double-backtick code spans: `` `code` `` -> `code`."
  (should (equal (markdown-table-wrap-strip-markup "`` `code` ``")
                 "`code`")))

(ert-deftest markdown-table-wrap-test-strip-markup-italic-wrapping-code ()
  "Italic wrapping a code span: *`italic code`* -> italic code."
  (should (equal (markdown-table-wrap-strip-markup "*`italic code`*")
                 "italic code")))

;;;; Visible Width

(ert-deftest markdown-table-wrap-test-visible-width-default-no-strip ()
  "visible-width returns raw string width by default (no stripping)."
  (should (= (markdown-table-wrap-visible-width "**bold**") 8))
  (should (= (markdown-table-wrap-visible-width "[text](url)") 11))
  (should (= (markdown-table-wrap-visible-width "`code`") 6))
  (should (= (markdown-table-wrap-visible-width "plain") 5)))

(ert-deftest markdown-table-wrap-test-visible-width-with-strip ()
  "visible-width strips markup when strip-markup is t."
  (let ((markdown-table-wrap--strip-markup t))
    (should (= (markdown-table-wrap-visible-width "**bold**") 4))
    (should (= (markdown-table-wrap-visible-width "[text](url)") 4))
    (should (= (markdown-table-wrap-visible-width "`code`") 4))
    (should (= (markdown-table-wrap-visible-width "plain") 5))))

;;;; Display Width (VS16 Emoji Correction)

(ert-deftest markdown-table-wrap-test-display-width-vs16-promotes ()
  "VS16 (U+FE0F) after narrow base char produces display width 2."
  (should (= 2 (markdown-table-wrap--display-width "⛰️")))   ; U+26F0 + U+FE0F
  (should (= 2 (markdown-table-wrap--display-width "❤️")))   ; U+2764 + U+FE0F
  (should (= 2 (markdown-table-wrap--display-width "☁️"))))  ; U+2601 + U+FE0F

(ert-deftest markdown-table-wrap-test-display-width-no-vs16 ()
  "Without VS16, narrow base chars keep their natural width."
  (should (= 1 (markdown-table-wrap--display-width "⛰")))    ; U+26F0 alone
  (should (= 1 (markdown-table-wrap--display-width "❤"))))   ; U+2764 alone

(ert-deftest markdown-table-wrap-test-display-width-already-wide ()
  "Already-wide emoji are unaffected by VS16 correction."
  (should (= 2 (markdown-table-wrap--display-width "🕌")))   ; U+1F54C
  (should (= 2 (markdown-table-wrap--display-width "✅"))))  ; U+2705

(ert-deftest markdown-table-wrap-test-display-width-mixed-string ()
  "Mixed string with VS16 emoji computes correct total width."
  ;; a(1) + b(1) + ⛰(1→2 with VS16) + FE0F(0) + c(1) + d(1) = 6
  (should (= 6 (markdown-table-wrap--display-width "ab⛰️cd"))))

(ert-deftest markdown-table-wrap-test-display-width-plain-ascii ()
  "Plain ASCII strings pass through unmodified."
  (should (= 5 (markdown-table-wrap--display-width "hello")))
  (should (= 0 (markdown-table-wrap--display-width ""))))

(ert-deftest markdown-table-wrap-test-display-width-cjk ()
  "CJK wide characters are measured correctly."
  (should (= 2 (markdown-table-wrap--display-width "漢")))
  (should (= 6 (markdown-table-wrap--display-width "ab漢cd"))))

(ert-deftest markdown-table-wrap-test-visible-width-uses-display-width ()
  "visible-width incorporates VS16 correction for emoji."
  (should (= 2 (markdown-table-wrap-visible-width "⛰️")))
  (should (= 6 (markdown-table-wrap-visible-width "ab⛰️cd"))))

(ert-deftest markdown-table-wrap-test-table-with-vs16-emoji ()
  "Table containing VS16 emoji aligns correctly.
All non-separator lines should have equal display width."
  (let* ((md "| Icon | Name |\n|---|---|\n| ⛰️ | Mountain |\n| 🕌 | Mosque |")
         (result (markdown-table-wrap md 30)))
    ;; Both emoji are width 2 — rows should have consistent display width
    (let* ((data-lines (cl-remove-if
                        (lambda (l) (string-match-p "^|[-:|[:space:]]+|$" l))
                        (split-string result "\n" t)))
           (widths (mapcar #'markdown-table-wrap--display-width data-lines)))
      ;; Verify all lines have the same absolute width (not just equality)
      (should (>= (length widths) 3))  ; header + 2 data rows
      (let ((expected-w (car widths)))
        (should (> expected-w 0))
        (dolist (w (cdr widths))
          (should (= w expected-w)))))))

;;;; Truncate Cell Lines

(ert-deftest markdown-table-wrap-test-truncate-cell-lines-no-mutation ()
  "Truncating cell lines does not mutate the original list."
  (let* ((original (list "line1" "line2" "line3" "line4"))
         (copy (copy-sequence original))
         (_truncated (markdown-table-wrap--truncate-cell-lines original 2)))
    ;; Original should be unchanged
    (should (equal original copy))))

;;;; Cell Height Cap

(ert-deftest markdown-table-wrap-test-height-cap-truncates ()
  "Cells exceeding max-cell-height are truncated with ellipsis."
  ;; Cell that wraps to 4+ lines at width 10
  (let* ((md "| Header |\n|---|\n| one two three four five six seven eight |")
         (result (markdown-table-wrap md 20 2)))
    ;; Find data rows (after the separator line)
    (let* ((lines (split-string result "\n"))
           (sep-idx (cl-position-if
                     (lambda (l) (string-match-p "^|[-: |]+|$" l))
                     lines))
           (data-lines (seq-drop lines (1+ sep-idx))))
      ;; Should have exactly 2 data lines (capped)
      (should (= (length data-lines) 2))
      ;; Last data line should contain the ellipsis marker
      (should (string-match-p "…" (car (last data-lines)))))))

(ert-deftest markdown-table-wrap-test-height-cap-no-truncation-when-fits ()
  "Cells within max-cell-height are not truncated."
  ;; Cell wraps to only 2 lines — well within cap
  (let* ((md "| Header |\n|---|\n| hello world |")
         (result (markdown-table-wrap md 15 5)))
    (should-not (string-match-p "…" result))))

(ert-deftest markdown-table-wrap-test-height-cap-nil-unlimited ()
  "When max-cell-height is nil, no truncation occurs."
  (let* ((md "| Header |\n|---|\n| one two three four five six seven eight |")
         (result (markdown-table-wrap md 16 nil)))
    (should-not (string-match-p "…" result))))

(ert-deftest markdown-table-wrap-test-height-cap-applies-to-headers ()
  "Height cap also applies to header cells."
  (let* ((md "| very long header that wraps a lot |\n|---|\n| short |")
         (result (markdown-table-wrap md 15 2)))
    ;; Count lines before separator
    (let* ((lines (split-string result "\n"))
           (sep-idx (cl-position-if
                     (lambda (l) (string-match-p "^|[-: |]+|$" l))
                     lines))
           (header-lines (seq-take lines sep-idx)))
      (should (<= (length header-lines) 2))
      (should (string-match-p "…" (car (last header-lines)))))))

;;;; Strip-Markup Mode Integration

(ert-deftest markdown-table-wrap-test-strip-mode-no-strip-wider-columns ()
  "Without strip-markup, columns are wider (raw width includes markup)."
  (let* ((md "| Header |\n|---|\n| **bold text** and [link](http://x.co) |")
         (result-no-strip (markdown-table-wrap md 40))
         (result-strip (markdown-table-wrap md 40 nil t))
         (no-strip-lines (split-string result-no-strip "\n" t))
         (strip-lines (split-string result-strip "\n" t)))
    ;; No-strip version may produce more lines (wraps more aggressively)
    (should (>= (length no-strip-lines) (length strip-lines)))))

(ert-deftest markdown-table-wrap-test-strip-mode-no-strip-raw-width-respected ()
  "Without strip-markup, all output lines respect raw width constraint.
Lines with unbreakable markup spans may overflow — that is expected
since the span cannot be broken without corrupting syntax."
  (let* ((md "| Cmd | Desc |\n|---|---|\n| run | Install deps |")
         (result (markdown-table-wrap md 30)))
    ;; No markup in test data — all lines must fit in raw width
    (dolist (line (split-string result "\n" t))
      (should (<= (string-width line) 30)))))

(ert-deftest markdown-table-wrap-test-strip-mode-strip-visible-width-respected ()
  "With strip-markup, visible (stripped) width respects constraint."
  (let* ((md "| Cmd | Desc |\n|---|---|\n| **run** | Install all deps |")
         (result (markdown-table-wrap md 35 nil t)))
    (let ((markdown-table-wrap--strip-markup t))
      (dolist (line (split-string result "\n" t))
        (should (<= (markdown-table-wrap-visible-width line) 35))))))

(ert-deftest markdown-table-wrap-test-strip-mode-default-never-exceeds-raw ()
  "Default (no-strip) never produces lines exceeding target width for plain text."
  (let* ((md "| A | B |\n|---|---|\n| hello world | foo bar baz qux |")
         (result (markdown-table-wrap md 30)))
    (dolist (line (split-string result "\n" t))
      (should (<= (string-width line) 30)))))

(ert-deftest markdown-table-wrap-test-strip-mode-bold-links-both-modes ()
  "A table with bold and links wraps correctly in both modes."
  (let* ((md "| Feature | Status |\n|---|---|\n| **Auth** | [Docs](http://x.co/auth) |"))
    ;; No-strip: each mode produces valid pipe-table output
    (let ((result (markdown-table-wrap md 40)))
      (should (string-match-p "\\*\\*Auth\\*\\*" result))
      (dolist (line (split-string result "\n" t))
        (should (string-prefix-p "|" line))
        (should (string-suffix-p "|" line))))
    ;; Strip: same structure
    (let ((result (markdown-table-wrap md 40 nil t)))
      (should (string-match-p "\\*\\*Auth\\*\\*" result))
      (dolist (line (split-string result "\n" t))
        (should (string-prefix-p "|" line))
        (should (string-suffix-p "|" line))))))

;;;; Code Fence Detection

(ert-deftest markdown-table-wrap-test-inside-code-fence-basic ()
  "Detect position inside a fenced code block."
  (with-temp-buffer
    (insert "text\n```\n| pipe |\n```\noutside\n")
    ;; Inside the fence (on the "| pipe |" line)
    (goto-char (point-min))
    (search-forward "| pipe")
    (should (markdown-table-wrap-inside-code-fence-p
             (line-beginning-position)))
    ;; Outside the fence
    (goto-char (point-min))
    (search-forward "outside")
    (should-not (markdown-table-wrap-inside-code-fence-p
                 (line-beginning-position)))))

(ert-deftest markdown-table-wrap-test-inside-code-fence-tilde ()
  "Detect tilde-fenced code blocks."
  (with-temp-buffer
    (insert "~~~\n| pipe |\n~~~\n")
    (goto-char (point-min))
    (search-forward "| pipe")
    (should (markdown-table-wrap-inside-code-fence-p
             (line-beginning-position)))))

(ert-deftest markdown-table-wrap-test-inside-code-fence-with-lang ()
  "Detect fenced code blocks with language specifier."
  (with-temp-buffer
    (insert "```python\n| pipe |\n```\n")
    (goto-char (point-min))
    (search-forward "| pipe")
    (should (markdown-table-wrap-inside-code-fence-p
             (line-beginning-position)))))

(ert-deftest markdown-table-wrap-test-inside-code-fence-no-fence ()
  "Return nil when no fences exist."
  (with-temp-buffer
    (insert "| real table |\n|---|\n| data |\n")
    (should-not (markdown-table-wrap-inside-code-fence-p (point-min)))))

;;;; Backtick Parity Helper

(ert-deftest markdown-table-wrap-test-odd-backtick-line-p-detects-odd ()
  "Return non-nil when any line has an odd backtick count."
  (should (markdown-table-wrap--odd-backtick-line-p '("one `")))
  (should (markdown-table-wrap--odd-backtick-line-p '("even ``" "odd `")))
  (should (markdown-table-wrap--odd-backtick-line-p '("`a` b `c"))))

(ert-deftest markdown-table-wrap-test-odd-backtick-line-p-nil-for-even ()
  "Return nil when all lines have zero or even backtick counts."
  (should-not (markdown-table-wrap--odd-backtick-line-p '("no backticks")))
  (should-not (markdown-table-wrap--odd-backtick-line-p '("`a`" "`b`")))
  (should-not (markdown-table-wrap--odd-backtick-line-p '("``" "")))
  (should-not (markdown-table-wrap--odd-backtick-line-p nil)))

;;;; Backtick Leaking Across Rows

(ert-deftest markdown-table-wrap-test-no-odd-backticks-per-line ()
  "Wrapped table must never produce lines with an odd number of backticks.
When a cell contains nested backticks (e.g. `cmd \\=`sub\\=` more`),
the wrapping may split the content across visual rows.  If a row ends
up with an odd backtick count, markdown-mode's multiline inline-code
regex will match across rows, causing code-face to leak into unrelated
cells — including other columns on continuation rows.

Root cause: LLM output often wraps shell commands in backticks while
the commands themselves contain backtick command-substitution, creating
backticks-inside-backticks.  Example cell content:
  `env \\| grep \\=`echo $HOME\\=` \\| awk '{print $1}'`

markdown-mode sees three backtick pairs here, but the outer pair is
not valid markdown — the inner backticks break it.  When wrapping
splits this across rows, one row gets an opening backtick without
a close, and the next row gets a close without an open.  The
multiline regex in `markdown-regex-code' then matches across the
row boundary, applying inline-code-face to everything in between."
  (let* ((table
          (concat
           "| Description | Command |\n"
           "| --- | --- |\n"
           "| Count unique log entries | "
           "`cat /var/log/syslog \\| grep \"error\" \\| sort"
           " \\| uniq -c \\| sort -rn` |\n"
           "| Inline backtick and variables | "
           "`env \\| grep `echo $HOME`"
           " \\| awk -F= '{print $1, \"=>\", $2}'` |")))
    (dolist (width '(60 70 80))
      (let* ((wrapped (markdown-table-wrap table width))
             (lines (split-string wrapped "\n")))
        (dolist (line lines)
          (let ((bt-count (length (replace-regexp-in-string "[^`]" "" line))))
            (should (or (= bt-count 0)
                        (= (mod bt-count 2) 0)))))))))

;;;; Table Unwrapping

(ert-deftest markdown-table-wrap-test-unwrap-single-row-no-wrapping ()
  "An unwrapped table passes through with content unchanged.
The only difference allowed is re-formatting (alignment, padding)."
  (let* ((input "| A | B |\n|---|---|\n| x | y |")
         (result (markdown-table-wrap-unwrap input))
         (parsed (markdown-table-wrap-parse result)))
    ;; Same headers and data
    (should (equal (nth 0 parsed) '("A" "B")))
    (should (equal (nth 2 parsed) '(("x" "y"))))))

(ert-deftest markdown-table-wrap-test-unwrap-simple-continuation ()
  "Continuation rows in a single logical row are merged."
  (let* ((wrapped (concat "| Cmd      | Desc            |\n"
                          "| -------- | --------------- |\n"
                          "| npm      | Install all     |\n"
                          "| install  | project         |\n"
                          "|          | dependencies    |"))
         (result (markdown-table-wrap-unwrap wrapped))
         (parsed (markdown-table-wrap-parse result)))
    (should (= (length (nth 2 parsed)) 1))
    (should (equal (car (nth 2 parsed))
                   '("npm install" "Install all project dependencies")))))

(ert-deftest markdown-table-wrap-test-unwrap-multi-row ()
  "Multiple logical rows with different wrapping are correctly detected.
The heuristic detects new rows when a previously-empty column reappears."
  (let* ((wrapped (concat "| Cmd      | Desc            |\n"
                          "| -------- | --------------- |\n"
                          "| npm      | Install all     |\n"
                          "| install  | project deps    |\n"
                          "|          | from pkg        |\n"
                          "| npm test | Run tests       |"))
         (result (markdown-table-wrap-unwrap wrapped))
         (parsed (markdown-table-wrap-parse result)))
    (should (= (length (nth 2 parsed)) 2))
    (should (equal (car (nth 2 parsed))
                   '("npm install" "Install all project deps from pkg")))
    (should (equal (cadr (nth 2 parsed))
                   '("npm test" "Run tests")))))

(ert-deftest markdown-table-wrap-test-unwrap-first-column-wraps ()
  "Column 1 wrapping does not confuse the boundary heuristic.
The heuristic detects the boundary when a previously-empty column
reappears with content."
  (let* ((wrapped (concat "| Cmd      | Desc    |\n"
                          "| -------- | ------- |\n"
                          "| npm      | Install |\n"
                          "| install  | deps    |\n"
                          "|          | from    |\n"
                          "|          | pkg     |\n"
                          "| npm test | Run all |"))
         (result (markdown-table-wrap-unwrap wrapped))
         (parsed (markdown-table-wrap-parse result)))
    (should (= (length (nth 2 parsed)) 2))
    (should (equal (car (car (nth 2 parsed))) "npm install"))
    (should (equal (car (cadr (nth 2 parsed))) "npm test"))))

(ert-deftest markdown-table-wrap-test-unwrap-preserves-alignment ()
  "Alignment markers survive unwrapping."
  (let* ((wrapped (concat "| L    | R    | C    |\n"
                          "| :--- | ---: | :--: |\n"
                          "| left | right | center |"))
         (result (markdown-table-wrap-unwrap wrapped))
         (parsed (markdown-table-wrap-parse result)))
    (should (equal (nth 1 parsed) '(left right center)))))

(ert-deftest markdown-table-wrap-test-unwrap-empty-cells ()
  "Genuinely empty cells are preserved (not confused with continuations)."
  (let* ((input (concat "| A | B | C |\n"
                        "|---|---|---|\n"
                        "| val |  | data |\n"
                        "| x | y | z |"))
         (result (markdown-table-wrap-unwrap input))
         (parsed (markdown-table-wrap-parse result)))
    ;; Two rows; first has empty col 2
    (should (= (length (nth 2 parsed)) 2))
    (should (equal (nth 1 (car (nth 2 parsed))) ""))))

(ert-deftest markdown-table-wrap-test-unwrap-idempotent ()
  "Unwrapping an already-unwrapped table is a no-op (content-wise)."
  (let* ((input "| A | B |\n|---|---|\n| hello world | foo bar |")
         (once (markdown-table-wrap-unwrap input))
         (twice (markdown-table-wrap-unwrap once))
         (parsed1 (markdown-table-wrap-parse once))
         (parsed2 (markdown-table-wrap-parse twice)))
    (should (equal (nth 0 parsed1) (nth 0 parsed2)))
    (should (equal (nth 2 parsed1) (nth 2 parsed2)))))

(ert-deftest markdown-table-wrap-test-unwrap-header-wrapping ()
  "Wrapped header lines are merged back into a single header row."
  (let* ((wrapped (concat "| Comman | Statu |\n"
                          "| d      | s     |\n"
                          "| ------ | ----- |\n"
                          "| short  | done  |"))
         (result (markdown-table-wrap-unwrap wrapped))
         (parsed (markdown-table-wrap-parse result)))
    (should (equal (nth 0 parsed) '("Comman d" "Statu s")))))

(ert-deftest markdown-table-wrap-test-unwrap-roundtrip ()
  "Roundtrip: wrap(unwrap(wrap(t, w)), w) == wrap(t, w).
Uses a table where the short column (Cmd) finishes before the long
column (Desc), creating the empty-column signal that the heuristic
needs to detect logical row boundaries."
  (let* ((md (concat "| Cmd | Desc |\n|---|---|\n"
                     "| run | Install all project dependencies from the package manifest |\n"
                     "| test | Run all unit and integration tests in the project |\n"
                     "| build | Compile the TypeScript source code into JavaScript |"))
         (widths '(30 40 50)))
    (dolist (width widths)
      (let* ((wrapped (markdown-table-wrap md width))
             (unwrapped (markdown-table-wrap-unwrap wrapped))
             (rewrapped (markdown-table-wrap unwrapped width)))
        (ert-info ((format "w%d" width))
          (should (equal wrapped rewrapped)))))))

(ert-deftest markdown-table-wrap-test-unwrap-roundtrip-row-count ()
  "Unwrapping fixture tables preserves the correct number of logical rows."
  (let ((tables (markdown-table-wrap-test--fixture-tables))
        (widths '(40 60 80)))
    (cl-loop for tbl in tables for tbl-idx from 1 do
      (let* ((original-parsed (markdown-table-wrap-parse tbl))
             (original-rows (length (nth 2 original-parsed))))
        (dolist (width widths)
          (unless (markdown-table-wrap-test--too-narrow-p tbl width)
            (let* ((wrapped (markdown-table-wrap tbl width))
                   (unwrapped (markdown-table-wrap-unwrap wrapped))
                   (unwrapped-parsed (markdown-table-wrap-parse unwrapped))
                   (unwrapped-rows (length (nth 2 unwrapped-parsed))))
              (ert-info ((format "T%d w%d: expected %d rows got %d"
                                 tbl-idx width original-rows unwrapped-rows))
                (should (= unwrapped-rows original-rows))))))))))

(ert-deftest markdown-table-wrap-test-unwrap-rewrap-wider ()
  "Wrap narrow, unwrap, rewrap wider produces fewer lines."
  (let* ((md (concat "| Cmd | Desc |\n|---|---|\n"
                     "| npm install | Install all project dependencies |\n"
                     "| npm test | Run all unit and integration tests |"))
         (narrow (markdown-table-wrap md 30))
         (unwrapped (markdown-table-wrap-unwrap narrow))
         (wide (markdown-table-wrap unwrapped 80)))
    ;; Wider rendering should have fewer or equal lines
    (should (<= (length (split-string wide "\n" t))
                (length (split-string narrow "\n" t))))))

(ert-deftest markdown-table-wrap-test-render-row-separator-unwrap-roundtrip ()
  "Wrap with auto-separators, unwrap, rewrap compact matches compact wrap."
  (let* ((md (concat "| A | B |\n|---|---|\n"
                     "| hello world | foo bar baz qux |\n"
                     "| x | y |"))
         ;; Default: separators auto-inserted when wrapping occurs
         (with-sep (markdown-table-wrap md 25))
         (unwrapped (markdown-table-wrap-unwrap with-sep))
         ;; Compact: no separators
         (compact-wrap (markdown-table-wrap md 25 nil nil t)))
    ;; unwrap(default) rewrapped compact == compact
    (should (equal (markdown-table-wrap unwrapped 25 nil nil t) compact-wrap))))

;;;; Row Separator Lines

(ert-deftest markdown-table-wrap-test-row-separator-default-on-when-wrapping ()
  "Default behavior inserts empty rows between data rows when wrapping occurs.
A 3-row table where wrapping happens should get 2 separator rows."
  (let* ((md (concat "| Feature | Status |\n|---|---|\n"
                     "| Authentication system | Done |\n"
                     "| Database migration | WIP |\n"
                     "| API endpoints | Plan |"))
         ;; Width narrow enough to force wrapping
         (result (markdown-table-wrap md 30))
         (lines (split-string result "\n" t)))
    ;; Count empty-cell rows (all cells are whitespace-only)
    (let ((empty-count 0))
      (dolist (line lines)
        (let ((cells (markdown-table-wrap-test--extract-cells line)))
          (when (and cells
                     (not (markdown-table-wrap-test--separator-line-p line))
                     (cl-every (lambda (c) (string-empty-p (string-trim c)))
                               cells))
            (cl-incf empty-count))))
      ;; 3 data rows → 2 separator empty rows
      (should (= empty-count 2)))))

(ert-deftest markdown-table-wrap-test-row-separator-not-inserted-without-wrapping ()
  "No separators when all rows fit on one line — breathing room not needed."
  (let* ((md (concat "| A | B |\n|---|---|\n"
                     "| x | y |\n"
                     "| p | q |"))
         ;; Wide enough that no wrapping occurs
         (result (markdown-table-wrap md 30))
         (lines (split-string result "\n" t)))
    ;; Should have: header, separator, row1, row2 = 4 lines (no empty rows)
    (should (= (length lines) 4))))

(ert-deftest markdown-table-wrap-test-row-separator-alignment ()
  "Empty separator rows have pipes at the same positions as all other rows."
  (let* ((md (concat "| Feature | Status | Description |\n"
                     "|:---|---:|:---:|\n"
                     "| Auth | Done | OAuth2 flow with refresh tokens |\n"
                     "| DB | WIP | PostgreSQL connection pooling |"))
         ;; Narrow enough to force wrapping
         (result (markdown-table-wrap md 40))
         (lines (split-string result "\n" t))
         (sep (cl-find-if #'markdown-table-wrap-test--separator-line-p lines))
         (ref-positions (markdown-table-wrap-test--pipe-positions sep)))
    ;; Every line (including empty separator rows) must align
    (dolist (line lines)
      (unless (markdown-table-wrap-test--separator-line-p line)
        (let ((positions (markdown-table-wrap-test--pipe-positions line)))
          (should (equal positions ref-positions)))))))

(ert-deftest markdown-table-wrap-test-row-separator-single-row ()
  "Single data row table produces no separator rows even with wrapping."
  (let* ((md "| A | B |\n|---|---|\n| hello world foo bar | y |")
         (default-result (markdown-table-wrap md 20))
         (compact-result (markdown-table-wrap md 20 nil nil t)))
    ;; With only one data row, no separator to insert regardless
    (should (equal default-result compact-result))))

(ert-deftest markdown-table-wrap-test-row-separator-compact-opt-out ()
  "Passing COMPACT t suppresses separators even when wrapping occurs."
  (let* ((md (concat "| A | B |\n|---|---|\n"
                     "| hello world | foo bar baz |\n"
                     "| x | y |"))
         (result (markdown-table-wrap md 20 nil nil t))
         (lines (split-string result "\n" t)))
    ;; No empty-cell rows should appear
    (let ((empty-count 0))
      (dolist (line lines)
        (let ((cells (markdown-table-wrap-test--extract-cells line)))
          (when (and cells
                     (not (markdown-table-wrap-test--separator-line-p line))
                     (cl-every (lambda (c) (string-empty-p (string-trim c)))
                               cells))
            (cl-incf empty-count))))
      (should (= empty-count 0)))))

(ert-deftest markdown-table-wrap-test-row-separator-batch ()
  "Batch rendering matches individual calls (default separators)."
  (let ((md (concat "| A | B |\n|---|---|\n"
                    "| hello world | foo bar |\n"
                    "| x | y |"))
        (widths '(20 30 50)))
    (let ((batch (markdown-table-wrap-batch md widths))
          (individual (mapcar (lambda (w)
                                (markdown-table-wrap md w))
                              widths)))
      (should (equal batch individual)))))

(ert-deftest markdown-table-wrap-test-row-separator-with-height-cap ()
  "Separators work correctly alongside MAX-CELL-HEIGHT truncation."
  (let* ((md (concat "| A | B |\n|---|---|\n"
                     "| one two three four five six | alpha |\n"
                     "| x | y |"))
         (result (markdown-table-wrap md 20 2))
         (lines (split-string result "\n" t)))
    ;; Find the empty separator row
    (let ((has-empty nil))
      (dolist (line lines)
        (let ((cells (markdown-table-wrap-test--extract-cells line)))
          (when (and cells
                     (not (markdown-table-wrap-test--separator-line-p line))
                     (cl-every (lambda (c) (string-empty-p (string-trim c)))
                               cells))
            (setq has-empty t))))
      ;; Should have an empty row — wrapping occurred + height cap
      (should has-empty))
    ;; Should also have truncation indicator
    (should (string-match-p "…" result))))

;;;; End-to-End: Structural Invariants

(defconst markdown-table-wrap-test--e2e-widths '(30 40 60 80 120 200)
  "Widths to test in E2E structural tests.")

(defun markdown-table-wrap-test--pipe-positions (line)
  "Return a list of display-column positions of | characters in LINE.
Positions are measured in display columns (string-width), not bytes."
  (let ((positions nil)
        (col 0))
    (dotimes (i (length line))
      (when (= (aref line i) ?|)
        (push col positions))
      (setq col (+ col (char-width (aref line i)))))
    (nreverse positions)))

(defun markdown-table-wrap-test--separator-line-p (line)
  "Return non-nil if LINE is a table separator (dashes and colons).
A separator line contains only pipe, dash, colon, and space characters,
and EVERY cell (trimmed) is a non-empty string of dashes and/or colons.
Data lines have empty cells (from wrapping), so this requirement
distinguishes separators from data content."
  (and (string-match-p "^|" line)
       (not (string-match-p "[^| :-]" line))
       ;; Every cell must be a non-empty string of dashes/colons
       (let ((cells (mapcar #'string-trim
                            (split-string
                             (replace-regexp-in-string
                              "\\`|\\||\\'" ""
                              (string-trim line))
                             "|"))))
         (and cells
              (cl-every (lambda (cell)
                          (and (not (string-empty-p cell))
                               (string-match-p "\\`[-:]+\\'" cell)))
                        cells)))))

(defun markdown-table-wrap-test--too-narrow-p (text width)
  "Return non-nil if WIDTH is too narrow to render TEXT as a table."
  (let* ((parsed (markdown-table-wrap-parse text))
         (num-cols (length (nth 0 parsed))))
    (or (= num-cols 0)
        (< (- width (+ (* 3 num-cols) 1)) num-cols))))

(ert-deftest markdown-table-wrap-test-e2e-pipe-alignment ()
  "Pipe characters fall at identical display-column positions on every line.
This is the fundamental visual invariant: columns must line up.
Uses the separator line as reference (it never overflows).
Lines where a markup token overflows the column width are excluded —
they are wider by design (preserving markup integrity)."
  (let ((tables (markdown-table-wrap-test--fixture-tables))
        (widths markdown-table-wrap-test--e2e-widths))
    (cl-loop for tbl in tables for tbl-idx from 1 do
      (dolist (width widths)
        (unless (markdown-table-wrap-test--too-narrow-p tbl width)
          (let* ((wrapped (markdown-table-wrap tbl width))
                 (lines (split-string wrapped "\n"))
                 ;; Use separator as reference — it never overflows
                 (sep (cl-find-if
                       #'markdown-table-wrap-test--separator-line-p lines))
                 (ref-positions
                  (markdown-table-wrap-test--pipe-positions sep)))
            (cl-loop for line in lines for line-no from 1 do
              (unless (or (markdown-table-wrap-test--separator-line-p line)
                          (> (string-width line) width))
                (let ((positions (markdown-table-wrap-test--pipe-positions
                                  line)))
                  (ert-info ((format "T%d w%d L%d: expected %S got %S"
                                     tbl-idx width line-no
                                     ref-positions positions))
                    (should (equal positions ref-positions))))))))))))

(ert-deftest markdown-table-wrap-test-e2e-separator-structure ()
  "Exactly one separator line per table, matching alignment marker syntax."
  (let ((tables (markdown-table-wrap-test--fixture-tables))
        (widths markdown-table-wrap-test--e2e-widths))
    (cl-loop for tbl in tables for tbl-idx from 1 do
      (dolist (width widths)
        (unless (markdown-table-wrap-test--too-narrow-p tbl width)
          (let* ((wrapped (markdown-table-wrap tbl width))
                 (lines (split-string wrapped "\n"))
                 (sep-count 0)
                 (sep-line nil))
            (dolist (line lines)
              (when (markdown-table-wrap-test--separator-line-p line)
                (cl-incf sep-count)
                (setq sep-line line)))
            (ert-info ((format "T%d w%d: %d separators" tbl-idx width sep-count))
              (should (= sep-count 1)))))))))

(ert-deftest markdown-table-wrap-test-e2e-overflow-is-unbreakable ()
  "Lines that exceed the target width do so only because of unbreakable
content: markup spans (links, images, bold-italic) or wide characters
(CJK) whose display width exceeds the column allocation.  This ensures
overflow is intentional rather than a padding/alignment bug."
  (let ((tables (markdown-table-wrap-test--fixture-tables))
        (widths markdown-table-wrap-test--e2e-widths))
    (cl-loop for tbl in tables for tbl-idx from 1 do
      (dolist (width widths)
        (unless (markdown-table-wrap-test--too-narrow-p tbl width)
          (let* ((wrapped (markdown-table-wrap tbl width))
                 (lines (split-string wrapped "\n")))
            (cl-loop for line in lines for line-no from 1 do
              (when (> (string-width line) width)
                (let* ((cells (markdown-table-wrap-test--extract-cells line))
                       (has-unbreakable
                        (cl-some
                         (lambda (cell)
                           (let ((trimmed (string-trim cell)))
                             (and (> (length trimmed) 0)
                                  (or (cl-some
                                       (lambda (tok)
                                         (markdown-table-wrap--markup-span-parts tok))
                                       (markdown-table-wrap--tokenize-cell-text
                                        trimmed))
                                      (cl-some
                                       (lambda (ch) (> (char-width ch) 1))
                                       trimmed)))))
                         cells)))
                  (ert-info ((format "T%d w%d L%d: overflow w/o cause, width=%d"
                                     tbl-idx width line-no
                                     (string-width line)))
                    (should has-unbreakable)))))))))))

(ert-deftest markdown-table-wrap-test-e2e-height-cap ()
  "When max-cell-height is set, no logical row exceeds that many lines."
  (let ((tables (markdown-table-wrap-test--fixture-tables))
        (widths '(40 80))
        (cap 3))
    (cl-loop for tbl in tables for tbl-idx from 1 do
      (dolist (width widths)
        (unless (markdown-table-wrap-test--too-narrow-p tbl width)
          (let* ((wrapped (markdown-table-wrap tbl width cap))
                 (lines (split-string wrapped "\n"))
                 (parsed (markdown-table-wrap-parse tbl))
                 (num-cols (length (nth 0 parsed)))
                 (row-height 0))
            ;; A continuation row has empty first cell (just spaces).
            ;; A breathing-room separator has ALL cells empty -- it marks
            ;; a row boundary, not a continuation.
            (cl-loop for line in lines for line-no from 1 do
              (let* ((cells (markdown-table-wrap-test--extract-cells line))
                     (is-sep (markdown-table-wrap-test--separator-line-p line))
                     (all-empty (and cells
                                     (cl-every (lambda (c)
                                                 (string-empty-p
                                                  (string-trim c)))
                                               cells)))
                     ;; A continuation: first cell empty but not all cells
                     (is-continuation
                      (and cells (not is-sep) (not all-empty)
                           (> row-height 0)
                           (string-empty-p (string-trim (car cells))))))
                (cond
                 (is-continuation (cl-incf row-height))
                 (all-empty (setq row-height 0))  ; breathing room resets
                 (t (setq row-height 1)))
                (unless (or is-sep all-empty)
                  (ert-info ((format "T%d w%d L%d: row-height %d > cap %d"
                                     tbl-idx width line-no row-height cap))
                    (should (<= row-height cap))))))))))))

(ert-deftest markdown-table-wrap-test-e2e-too-narrow-identity ()
  "When width is too narrow to render, output equals input unchanged."
  (let ((tables (markdown-table-wrap-test--fixture-tables)))
    (cl-loop for tbl in tables for tbl-idx from 1 do
      (dolist (width '(5 10 15))
        (when (markdown-table-wrap-test--too-narrow-p tbl width)
          (ert-info ((format "T%d w%d" tbl-idx width))
            (should (equal (markdown-table-wrap tbl width) tbl))))))))

;;;; End-to-End: Markup Leak Detection

(ert-deftest markdown-table-wrap-test-e2e-no-orphan-markup ()
  :expected-result :failed
  "No output cell should contain unrecognized markup delimiter tokens.
When a cell contains a token with * or ~ that is not a matched
markup span, the delimiter could leak across the | column boundary
in a markdown renderer (e.g. markdown-mode font-lock), causing
bold/italic/strikethrough to bleed into adjacent columns.

This test wraps all fixture tables at various widths and verifies
that every cell in the output has balanced, recognized markup.

Currently expected to fail due to two known tokenizer limitations:
1. Nested delimiters: *italic containing **bold** inside* — the
   italic regex cannot match past inner ** markers.
2. CJK text without whitespace: 年由*出版社*出版 — the tokenizer
   splits on whitespace, so CJK cells with inline markup become
   one giant plain token.  Force-breaking then cuts through markers."
  (let ((tables (markdown-table-wrap-test--fixture-tables))
        (widths '(30 40 60 80 120 200)))
    (cl-loop for tbl in tables for tbl-idx from 1 do
      (dolist (width widths)
        (let* ((wrapped (markdown-table-wrap tbl width))
               (lines (split-string wrapped "\n")))
          (cl-loop for line in lines for line-no from 1 do
            (dolist (cell (markdown-table-wrap-test--extract-cells line))
              (let ((orphan (markdown-table-wrap-test--find-orphan-delimiter
                             cell)))
                (ert-info ((format "T%d w%d L%d orphan=%S cell=%.40S"
                                   tbl-idx width line-no orphan cell))
                  (should-not orphan))))))))))

(provide 'markdown-table-wrap-test)
;;; markdown-table-wrap-test.el ends here
