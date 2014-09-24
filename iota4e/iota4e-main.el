(require 'iota4e-utils)    ;; utility functions

(defconst iota4e~main-buffer-name " *iota4e-main*"
  "*internal* Name of the iota4e main view buffer.")

(defvar iota4e-main-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "s" 'iota4e-headers-search)
    (define-key map "q" 'iota4e-quit)
    map)

  "Keymap for the *iota4e-main* buffer.")
(fset 'iota4e-main-mode-map iota4e-main-mode-map)

(defvar iota4e-main-mode-abbrev-table nil)
(define-derived-mode iota4e-main-mode special-mode "iota4e:main"
  "Major mode for the iota4e main screen.
\\{iota4e-main-mode-map}."
  (use-local-map iota4e-main-mode-map)
  (setq
    truncate-lines t
    overwrite-mode 'overwrite-mode-binary))


(defun iota4e~main-action-str (str &optional func-or-shortcut)
  "Highlight the first occurence of [..] in STR.
If FUNC-OR-SHORTCUT is non-nil and if it is a function, call it
when STR is clicked (using RET or mouse-2); if FUNC-OR-SHORTCUT
is a string, execute the corresponding keyboard action when it is
clicked."
  (let ((newstr
	  (replace-regexp-in-string
	    "\\[\\(\\w+\\)\\]"
	    (lambda(m)
	      (format "[%s]"
		(propertize (match-string 1 str) 'face 'iota4e-highlight-face)))
	    str))
	 (map (make-sparse-keymap))
	 (func (if (functionp func-or-shortcut)
		 func-or-shortcut
		 (if (stringp func-or-shortcut)
		   (lexical-let ((macro func-or-shortcut))
		     (lambda()(interactive)
		       (execute-kbd-macro macro)))))))
    (define-key map [mouse-2] func)
    (define-key map (kbd "RET") func)
    (put-text-property 0 (length newstr) 'keymap map newstr)
    (put-text-property (string-match "\\w" newstr)
      (- (length newstr) 1) 'mouse-face 'highlight newstr) newstr))


(defun iota4e~main-view ()
  "Show the iota4e main view."
  (let ((buf (get-buffer-create iota4e~main-buffer-name))
	 (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (insert
	"* "
	(propertize "iota4e - iota for emacs version " 'face 'iota4e-title-face)
	(propertize  iota4e-iota-version 'face 'iota4e-view-header-key-face)

	"\n\n"
	(propertize "  Basics\n\n" 'face 'iota4e-title-face)
	(iota4e~main-action-str "\t* enter a [s]earch query\n" 'iota4e-search)
	"\n"
	(iota4e~main-action-str "\t* [q]uit\n" 'iota4e-quit))
      (iota4e-main-mode)
      (switch-to-buffer buf))))


(provide 'iota4e-main)
