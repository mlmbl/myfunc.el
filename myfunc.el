;;; myfunc.el --- My custom functions and utilities -*- lexical-binding: t -*-

;; Copyright (C) 2025 mlmbl (Yuji TAKENOSHITA)

;; Author: Yuji TAKENOSHITA <yuji.takenoshita@gmail.com>
;; URL: https://github.com/yourusername/myfunc.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

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

;; This package provides custom functions and utilities for my Emacs
;; configuration.  It includes:
;;
;; - Utility functions for text manipulation
;; - Helper functions for package integration
;; - Custom workflows and shortcuts

;;; Code:

;; pdf2bibを用いてPDFからbibtexエントリを生成する
(defun myfunc-insert-bibtex-from-pdf (pdf-file)
  "Insert BibTeX entry at point from PDF using pdf2bib.
Prompts for PDF-FILE if not provided."
  (interactive "fPDF file: ")
  (let* ((pdf-path (expand-file-name pdf-file))
         (output (string-trim
                  (shell-command-to-string
                   (format "pdf2bib %s | tail -n +2" (shell-quote-argument pdf-path))))))
    (if (or (string-empty-p output)
            (string-match-p "error\\|warning\\|failed" (downcase output)))
        (message "No bib-info found for: %s" (file-name-nondirectory pdf-path))
      (insert output)
      (bibtex-mode)  ; 一時的にBibTeXモードを有効化してフォーマット
      (bibtex-clean-entry)
      (message "BibTeX entry inserted from: %s" (file-name-nondirectory pdf-path)))))

;; 自分ルールのbibtex keyを生成する
(require 'bibtex)

(defun myfunc-generate-bibtex-key ()
  "Generate a new BibTeX key basd on author and year.
Format: lastname + year + \"-\" + two randome lowercase latters.
Example: lenon1967-gb"
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((entry (bibtex-parse-entry t))
	   (author (bibtex-text-in-field "author"))
	   (year (bibtex-text-in-field "year"))
	   (lastname (myfunc--extract-first-author-lastname author))
	   (random-suffix (myfunc--random-letters 2)))
      (if (and lastname year)
	  (format "%s%s-%s"
		  (downcase lastname)
		  year
		  random-suffix)
	(error "Could not extract author or year from entry")))))

(defun myfunc--extract-first-author-lastname (author-string)
  "Extract the last name of the first author from AUTHOR-STRING.
Handles formats like:
  - \"Lenon, Joh and McCartney, Paul\"
  - \"John Lenon and Paul McCartney\"
  - \"Lenon, J. and McCartney, P.\""
  (when author-string
    (let* ((first-author (car (split-string author-string " and " t)))
	   (fisrt-author (string-trim first-author)))
      (cond
       ;; Format: "Lastname, Firstname"
       ((string-match "\\([^,]+\\)," first-author)
	(match-string 1 fisrt-author))
       ;; Format: "Firstname Lastname"
       ((string-match "\\s-+\\([^ ]+\\)\\s-*$" first-author)
	(match-string 1 first-author))
       ;;Fallback: use the whole string
       (t first-author)))))

(defun myfunc--random-letters (n)
  "Generate N random lowercase letters."
  (let ((letters "abcdefghijklmnopqrstuvwxyz")
	(result ""))
    (dotimes (_ n)
      (setq result (concat result
			   (string (aref letters (random (length letters)))))))
    result))


(defun myfunc-regenerate-bibtex-key ()
  "Regenerate BibTeX key for the entry at point."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((old-key (bibtex-key-in-head))
           (new-key (myfunc-generate-bibtex-key)))
      (if old-key
          (progn
            ;; エントリタイプを取得（@article, @book など）
            (bibtex-beginning-of-entry)
            (looking-at "@\\([a-zA-Z]+\\){")
            (let ((entry-type (match-string 1)))
              ;; 古いキーを新しいキーに置換
              (search-forward (concat "{" old-key))
              (replace-match (concat "{" new-key))
              (message "BibTeX key updated: %s → %s" old-key new-key)))
        (error "No BibTeX entry found at point")))))


;; カスタム変数の定義
(defgroup myfunc nil
  "Custom functions and utilities."
  :group 'convenience
  :prefix "myfunc-")

(defcustom myfunc-default-bib-file nil
  "Default BibTeX file for inserting entries.
If nil, prompts for a file each time."
  :type '(choice (const :tag "Always prompt" nil)
                 (file :tag "Default .bib file"))
  :group 'myfunc)


(defcustom myfunc-default-bib-file nil
  "Default BibTeX file for inserting entries.
If nil, prompts for a file each time."
  :type '(choice (const :tag "Always prompt" nil)
                 (file :tag "Default .bib file"))
  :group 'myfunc)

;; (defun myfunc-add-pdf-to-bib (pdf-file bib-file)
;;   "Add BibTeX entry from PDF-FILE to BIB-FILE with custom key.
;; PDF-FILE is determined by context (dired, pdf-tools, or prompt).
;; BIB-FILE defaults to `myfunc-default-bib-file' or prompts if nil."
;;   (interactive
;;    (list (myfunc--get-pdf-file)
;;          (or myfunc-default-bib-file
;; 	     (car org-cite-global-bibliography)
;;              (read-file-name "BibTeX file: " nil nil t nil
;;                              (lambda (name) (string-match-p "\\.bib\\'" name))))))
;;   (let ((pdf-path (expand-file-name pdf-file))
;;         (bib-path (expand-file-name bib-file)))
;;     ;; 一時バッファでBibTeX生成を試みる
;;     (with-temp-buffer
;;       (let ((output (string-trim
;;                      (shell-command-to-string
;;                       (format "pdf2bib %s | tail -n +2" 
;;                               (shell-quote-argument pdf-path))))))
;;         (if (or (string-empty-p output)
;;                 (string-match-p "error\\|warning\\|failed" (downcase output)))
;;             (progn
;;               (message "No bib-info found for: %s" (file-name-nondirectory pdf-path))
;;               nil) ; 解析失敗で終了
;;           ;; 解析成功: BibTeXエントリを挿入
;;           (insert output)
;;           (bibtex-mode)
;;           (bibtex-clean-entry)
          
;;           ;; キーを再生成
;;           (goto-char (point-min))
;;           (bibtex-beginning-of-entry)
;;           (let* ((old-key (bibtex-key-in-head))
;;                  (new-key (myfunc-generate-bibtex-key)))
;;             (when old-key
;;               (search-forward (concat "{" old-key))
;;               (replace-match (concat "{" new-key)))
            
;;             ;; bibファイルの末尾に追加
;;             (let ((entry-text (buffer-string)))
;;               (find-file bib-path)
;;               (goto-char (point-max))
;;               ;; 末尾に改行がなければ追加
;;               (unless (bolp) (insert "\n"))
;;               (insert "\n" entry-text "\n")
;;               (bibtex-mode)
;;               (save-buffer)
;;               (message "Added entry with key '%s' to %s" 
;;                        new-key 
;;                        (file-name-nondirectory bib-path))
;;               ;; bibファイルを表示
;;               (switch-to-buffer (current-buffer))
;;               (goto-char (point-max))
;;               (bibtex-beginning-of-entry))))))))

(defun myfunc-add-pdf-to-bib (pdf-file bib-file)
  "Add BibTeX entry from PDF-FILE to BIB-FILE with custom key.
PDF-FILE is determined by context (dired, pdf-tools, or prompt).
BIB-FILE defaults to `myfunc-default-bib-file' or prompts if nil.
Checks for duplicate DOI before adding."
  (interactive
   (list (myfunc--get-pdf-file)
         (or myfunc-default-bib-file
             (car org-cite-global-bibliography)
             (read-file-name "BibTeX file: " nil nil t nil
                             (lambda (name) (string-match-p "\\.bib\\'" name))))))
  (let ((pdf-path (expand-file-name pdf-file))
        (bib-path (expand-file-name bib-file)))
    ;; 一時バッファでBibTeX生成を試みる
    (with-temp-buffer
      (let ((output (string-trim
                     (shell-command-to-string
                      (format "pdf2bib %s | tail -n +2" 
                              (shell-quote-argument pdf-path))))))
        (if (or (string-empty-p output)
                (string-match-p "error\\|warning\\|failed" (downcase output)))
            (progn
              (message "No bib-info found for: %s" (file-name-nondirectory pdf-path))
              nil) ; 解析失敗で終了
          ;; 解析成功: BibTeXエントリを挿入
          (insert output)
          (bibtex-mode)
          (bibtex-clean-entry)
          
          ;; DOIを抽出
          (goto-char (point-min))
          (bibtex-beginning-of-entry)
          (let* ((entry (bibtex-parse-entry t))
                 (doi (bibtex-text-in-field "doi" entry))
                 (existing-entry (when doi (myfunc--find-entry-by-doi doi bib-path))))
            
            ;; 重複チェック
            (if existing-entry
                (progn
                  (find-file bib-path)
                  (goto-char existing-entry)
                  (bibtex-beginning-of-entry)
                  (message "Entry with DOI '%s' already exists in %s" 
                           doi 
                           (file-name-nondirectory bib-path)))
              
              ;; 重複なし: キーを再生成して追加
              (goto-char (point-min))
              (bibtex-beginning-of-entry)
              (let* ((old-key (bibtex-key-in-head))
                     (new-key (myfunc-generate-bibtex-key)))
                (when old-key
                  (search-forward (concat "{" old-key))
                  (replace-match (concat "{" new-key)))
                
                ;; bibファイルの末尾に追加
                (let ((entry-text (buffer-string)))
                  (find-file bib-path)
                  (goto-char (point-max))
                  ;; 末尾に改行がなければ追加
                  (unless (bolp) (insert "\n"))
                  (insert "\n" entry-text "\n")
                  (bibtex-mode)
                  (save-buffer)
                  (message "Added entry with key '%s' to %s" 
                           new-key 
                           (file-name-nondirectory bib-path))
                  ;; bibファイルを表示
                  (switch-to-buffer (current-buffer))
                  (goto-char (point-max))
                  (bibtex-beginning-of-entry))))))))))

(defun myfunc--find-entry-by-doi (doi bib-file)
  "Search for BibTeX entry with DOI in BIB-FILE.
Returns the position of the entry if found, nil otherwise."
  (when (and doi (not (string-empty-p doi)))
    (with-current-buffer (find-file-noselect bib-file)
      (save-excursion
        (goto-char (point-min))
        (let ((search-doi (replace-regexp-in-string "[{}]" "" doi))
              (found-pos nil))
          (while (and (not found-pos)
                      (re-search-forward "^[ \t]*doi[ \t]*=[ \t]*[{\"]*\\([^,}\"]+\\)" nil t))
            (let ((current-doi (replace-regexp-in-string "[{}\" ]" "" (match-string 1))))
              (when (string= (downcase search-doi) (downcase current-doi))
                (setq found-pos (save-excursion
                                  (bibtex-beginning-of-entry)
                                  (point))))))
          found-pos)))))


(defun myfunc--get-pdf-file ()
  "Get PDF file from context (dired or pdf-tools) or prompt user.
Returns the PDF file path."
  (cond
   ;; pdf-toolsで閲覧中
   ((and (eq major-mode 'pdf-view-mode)
         (boundp 'buffer-file-name)
         buffer-file-name)
    buffer-file-name)
   
   ;; diredバッファでPDFファイルにカーソルがある
   ((and (eq major-mode 'dired-mode)
         (dired-get-filename nil t)
         (string-match-p "\\.pdf\\'" (dired-get-filename nil t)))
    (dired-get-filename nil t))
   
   ;; それ以外: ユーザーに入力を求める
   (t
    (read-file-name "PDF file: " nil nil t nil
                    (lambda (name) (string-match-p "\\.pdf\\'" name))))))


;; myfunc-add-pdf-to-bib にもうひとつ機能を追加してほしい。
;; PDF-FILEから抽出したdoiをもつbibエントリがBIB-FILEに含まれていないか確認し、
;; あった場合は「そのエントリはすでにある」という意味のメッセージを出すとともに、
;; 該当するエントリの先頭行に移動してほしい


```elisp
```


(provide 'myfunc)
;;; myfunc.el ends here
