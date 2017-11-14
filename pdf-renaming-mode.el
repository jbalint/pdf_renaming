;; TODO use docview mode or gs to show pages graphically

(require 'subr-x)

(add-hook 'dired-mode-hook
  (lambda ()
	(local-unset-key (kbd "C-x p"))
    (define-key dired-mode-map (kbd "C-x p") 'dired-pdf-rename-files)
	(local-unset-key (kbd "C-o"))
    (define-key dired-mode-map (kbd "C-o") 'dired-pdf-open-external)))

;; TODO needs to be extended to have an external list of files
;; and go to the next one when the rename is killed
(defun dired-pdf-rename-files ()
  "Dired-mode utility to handle renaming current or list of marked files"
  (interactive)
  (mapc 'pdf-rename-file (dired-get-marked-files)))

(defun dired-pdf-open-external ()
  "Dired-mode utility to open documents externally"
  (interactive)
  (pdf--open-external (dired-get-file-for-visit)))

(defun pdf-to-text (filename page-num)
  "Convert page `page-num' of `filename' to text and show the
output in the current buffer"
  (interactive)
  (if (< page-num 1)
	  (user-error (format "Page number is not valid: %d" page-num)))
  ;; Ghostscript versions
  ;; (let ((first-args (split-string (format "-q -dNODISPLAY -P- -dSAFER -dDELAYBIND -dREALLYDELAYBIND -dWRITESYSTEMDICT -dSIMPLE -dFirstPage=%d -dLastPage=%d -c save -f ps2ascii.ps" page-num page-num)))
  ;;   	(rest-args `(,filename "-c" "quit")))
  ;; (let ((first-args (split-string (format "-q -dBATCH -dNOPAUSE -dSIMPLE -sDEVICE=txtwrite -dFirstPage=%d -dLastPage=%d -sOutputFile=-" page-num page-num)))
  ;;   	(rest-args (list filename)))
  ;; https://stackoverflow.com/a/6189489/1090617
  (let ((first-args (split-string (format "-f %d -l %d -layout -nopgbrk" page-num page-num)))
    	(rest-args (list filename "-")))
	(set (make-local-variable 'pdf-page-num) page-num)
	(apply 'call-process "pdftotext" nil t nil (append first-args rest-args))))

(defun pdf-rename-kill ()
  "Kill the current PDF-renaming buffer"
  (interactive)
  (let ((pdf-renaming-buffer (current-buffer)))
	(if pdf-dired-buffer
	  (delete-window))
	(kill-buffer pdf-renaming-buffer)))

;; This next/prev page stuff might not be the best option
;; or.. it might be
(defun pdf-next-page ()
  "Go to the next page in the current PDF-renaming buffer"
  (interactive)
  (erase-buffer)
  (pdf-to-text pdf-rename-filename (+ 1 pdf-page-num)))

(defun pdf-prev-page ()
  "Go to the previous page in the current PDF-renaming buffer"
  (interactive)
  (erase-buffer)
  (pdf-to-text pdf-rename-filename (- pdf-page-num 1)))

(defun pdf-rename-file (filename)
  "Main function to initiate renaming of `filename'"
  (interactive)
  (let ((content-buf (generate-new-buffer "*pdf-renaming*"))
		(dired-buf (if (eq 'dired-mode major-mode) (current-buffer))))
	(with-current-buffer content-buf
	  (pdf-renaming-mode)
	  (pdf-to-text filename 1)
	  (set (make-local-variable 'pdf-rename-filename) filename)
	  (set (make-local-variable 'pdf-dired-buffer) dired-buf)
	(switch-to-buffer-other-window content-buf))))

(defun pdf--do-rename (new-filename)
  "Rename the file and kill the buffer"
  (let ((old-filename pdf-rename-filename))
	(if pdf-dired-buffer
		(with-current-buffer pdf-dired-buffer
		  (dired-rename-file old-filename new-filename nil))
	  (rename-file old-filename new-filename nil))
    (pdf-rename-kill)
    (message (concat "Renamed to: " new-filename))))

(defun pdf-do-rename-with-buffer-contents ()
  "Perform the renaming using the buffer contents"
  (interactive)
  (let ((new-filename (if (use-region-p)
                          (buffer-substring (mark) (point))
                        (buffer-string))))
    (pdf--do-rename new-filename)))

(defun pdf-do-rename-with-current-line ()
  "Perform the renaming using the current line with PDF appended"
  (interactive)
  (let* ((cur-line (buffer-substring (point-at-bol) (point-at-eol)))
         (candidate1 (string-trim (replace-regexp-in-string ":" " -" cur-line)))
         (new-filename (concat candidate1 ".pdf")))
    (pdf--do-rename new-filename)))

(defun pdf--open-external (filename)
  "Internal version to open document externally"
  (pcase (substring filename -4 nil)
	(".pdf" (start-process "pdf" "pdf" (executable-find "chromium") "--new-window" filename))
	("djvu" (start-process "pdf" "pdf" (executable-find "djview") filename))
	(t (error (concat "No program to open " filename)))))

(defun pdf-open-external ()
  "Open the PDF file externally for viewing"
  (interactive)
  (pdf--open-external pdf-rename-filename))

;; (defun kill-buffer-not-region ()
;;   ""
;;   (interactive)
;;   (copy-to-register ?p (mark) (point))
;;   (erase-buffer)
;;   (insert-register ?p))

(defvar pdf-renaming-mode-map
  (let ((map (make-keymap)))

	(define-key map (kbd "C-c C-k") 'pdf-rename-kill)

	(define-key map (kbd "C-c C-c") 'pdf-do-rename-with-buffer-contents)

	(define-key map (kbd "C-c C-n") 'pdf-next-page)
	(define-key map (kbd "C-c C-p") 'pdf-prev-page)

	(define-key map (kbd "C-c C-o") 'pdf-open-external)

    map)
  "Keymap for PDF renaming major mode")

(defun pdf-renaming-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map pdf-renaming-mode-map)
  (setq major-mode 'pdf-renaming-mode
		mode-name "PDF-renaming"))

(provide 'pdf-renaming-mode)
