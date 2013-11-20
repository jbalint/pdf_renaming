(add-hook 'dired-mode-hook
  (lambda ()
	(local-unset-key (kbd "C-x p"))
    (define-key dired-mode-map (kbd "C-x p") 'dired-pdf-rename-files)))

;; TODO needs to be extended to have an external list of files
;; and go to the next one when the rename is killed
(defun dired-pdf-rename-files ()
  "Dired-mode utility to handle renaming current or list of marked files"
  (interactive)
  (mapc 'pdf-rename-file (dired-get-marked-files)))

(defun pdf-to-text (filename page-num)
  "Convert page `page-num' of `filename' to text and show the
output in the current buffer"
  (interactive)
  (let ((first-args (split-string (format "-q -dNODISPLAY -P- -dSAFER -dDELAYBIND -dWRITESYSTEMDICT -dSIMPLE -dFirstPage=%d -dLastPage=%d -c save -f ps2ascii.ps" page-num page-num)))
		(rest-args `(,filename "-c" "quit")))
	(set (make-local-variable 'pdf-page-num) page-num)
	(apply 'call-process "gs" nil t nil (append first-args rest-args))))

(defun pdf-rename-kill ()
  "Kill the current PDF-renaming buffer"
  (interactive)
  (let ((pdf-renaming-buffer (current-buffer)))
	(if pdf-dired-buffer
	  (delete-window))
	(kill-buffer pdf-renaming-buffer)))

(defun pdf-rename-next-page ()
  ""
  (interactive)
  )

(defun pdf-rename-prev-page ()
  ""
  (interactive)
  )

(defun pdf-rename-file (filename)
  ""
  (interactive)
  (let ((content-buf (generate-new-buffer "*pdf-renaming*"))
		(dired-buf (if (eq 'dired-mode major-mode) (current-buffer))))
	(with-current-buffer content-buf
	  (pdf-to-text filename 1)
	  (pdf-renaming-mode)
	  (set (make-local-variable 'pdf-rename-filename) filename)
	  (set (make-local-variable 'pdf-dired-buffer) dired-buf)
	(switch-to-buffer-other-window content-buf))))

(defun pdf-do-rename-with-buffer-contents ()
  ""
  (interactive)
  (let ((new-filename (if (use-region-p) (buffer-substring (mark) (point)) (buffer-string)))
		(old-filename pdf-rename-filename))
	(if pdf-dired-buffer
		(with-current-buffer pdf-dired-buffer
		  (dired-rename-file old-filename new-filename nil)))
	  (rename-file old-filename new-filename nil)
	  (pdf-rename-kill)))

;; (defun kill-buffer-not-region ()
;;   ""
;;   (interactive)
;;   (copy-to-register ?p (mark) (point))
;;   (erase-buffer)
;;   (insert-register ?p))

(defvar pdf-renaming-mode-map
  (let ((map (make-keymap)))

	;; TODO rename with buffer contents

	(define-key map (kbd "C-c C-k") 'pdf-rename-cancel)

	;; TODO next page

	;; TODO previous page

    (define-key map "\C-j" (lambda () (interactive) (message "This is PDF renaming mode!")))

    map)
  "Keymap for PDF renaming major mode")

(defun pdf-renaming-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map pdf-renaming-mode-map)
  (setq major-mode 'pdf-renaming-mode
		mode-name "PDF-renaming"))

(provide 'pdf-renaming-mode)
