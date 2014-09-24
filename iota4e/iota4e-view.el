(require 'iota4e-utils) ;; utility functions
(require 'iota4e-vars)
(require 'iota4e-proc)

(require 'comint)
(require 'browse-url)
(require 'button)
(require 'epa)
(require 'epg)

(eval-when-compile (byte-compile-disable-warning 'cl-functions))
(require 'cl)


;; the paper view
(defgroup iota4e-view nil
  "Settings for the paper view."
  :group 'iota4e)

(defcustom iota4e-view-fields
  '(:title :1au :year)
  "Header fields to display in the paper view buffer.
For the complete list of available headers, see `iota4e-header-info'."
  :type (list 'symbol)
  :group 'iota4e-view)

(defvar iota4e-view-fill-headers t
  "If non-nil, automatically fill the headers when viewing them.")


(defcustom iota4e-view-date-format "%c"
  "Date format to use in the paper view.
In the format of `format-time-string'."
  :type 'string
  :group 'iota4e-view)

(defcustom iota4e-view-scroll-to-next t
  "If non-nil, move to the next paper when calling
`iota4e-view-scroll-up-or-next' (typically bound to SPC) when at the
end of a paper. Otherwise, don't move to the next paper.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst iota4e~view-url-regexp
  "\\(\\(https?\\://\\|mailto:\\)[-+\[:alnum:\].?_$%/+&#@!*~,:;=/()]+\\)"
  "Regexp that matches http:/https:/mailto: URLs; match-string 1
will contain the matched URL, if any.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun iota4e-view-paper-with-paperid (paperid)
  "View paper with PAPERID.
This is meant for external programs wanting to show specific
papers - for example, `iota4e-org'."
  (iota4e~proc-view paperid))

(defun iota4e~view-custom-field (paper field)
  "Show some custom header field, or raise an error if it is not
found."
  (let* ((item (or (assoc field iota4e-header-info-custom)
		 (iota4e-error "field %S not found" field)))
	  (func (or (plist-get (cdr-safe item) :function)
		  (iota4e-error "no :function defined for field %S %S"
		    field (cdr item)))))
    (funcall func paper)))


(defun iota4e-view-paper-text (paper)
  "Return the paper to display (as a string), based on the PAPER plist."
  (concat
   (iota4e-paper-field paper :title)
   "\n\n"
   (mapconcat 'identity (iota4e-paper-field paper :authors) ", ")
   "\n\n"
   (iota4e-paper-field paper :pdffile)
   "\n\n"
   (with-temp-buffer
     (let ((fill-column 80))
       (insert (iota4e-paper-field paper :abstract))
       (fill-region (point-min) (point-max))
       (goto-char (point-min))
       (buffer-string)))))

(defun iota4e-view (paper headersbuf)
  "Display the paper PAPER in a new buffer, and keep in sync with HDRSBUF.
'In sync' here means that moving to the next/previous paper in
the the paper view affects HDRSBUF."
  (let* ((buf (get-buffer-create iota4e~view-buffer-name)))
    (with-current-buffer buf
      (switch-to-buffer buf)
      (setq iota4e~view-paper paper)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (delete-all-overlays)
        (insert (iota4e-view-paper-text paper))
        (goto-char (point-min))
        ;; (iota4e~fontify-cited)
        ;; (iota4e~fontify-signature)
        ;; (iota4e~view-make-urls-clickable)	
        ;; (iota4e~view-show-images-maybe paper)
        (setq
         iota4e~view-buffer buf
         iota4e~view-headers-buffer headersbuf)
        (iota4e-view-mode)))))

(defun iota4e~view-get-property-from-event (prop)
  "Get the property PROP at point, or the location of the mouse.
The action is chosen based on the `last-command-event'.
Meant to be evoked from interactive commands."
  (if (and (eventp last-command-event)
	   (mouse-event-p last-command-event))
      (let ((posn (event-end last-command-event)))
        (when (numberp (posn-point posn))
          (get-text-property
           (posn-point posn)
           prop
           (window-buffer (posn-window posn)))
          ))
    (get-text-property (point) prop)))
 
(defun iota4e~view-construct-header (field val &optional dont-propertize-val)
  "Return header field FIELD (as in `iota4e-header-info') with value
VAL if VAL is non-nil. If DONT-PROPERTIZE-VAL is non-nil, do not
add text-properties to VAL."
  (let* ((info (cdr (assoc field
		      (append iota4e-header-info iota4e-header-info-custom))))
	  (key (plist-get info :name))
	  (help (plist-get info :help)))
    (if (and val (> (length val) 0))
    (with-temp-buffer
      (insert (propertize (concat key ":")
		'face 'iota4e-header-key-face
		'help-echo help) " "
	(if dont-propertize-val
	  val
	  (propertize val 'face 'iota4e-header-value-face)) "\n")
      (when iota4e-view-fill-headers
	;; temporarily set the fill column <margin> positions to the right, so
	;; we can indent the following lines correctly
	(let* ((margin 1)
		(fill-column (max (- fill-column margin) 0)))
	  (fill-region (point-min) (point-max))
	  (goto-char (point-min))
	  (while (and (zerop (forward-line 1)) (not (looking-at "^$")))
	    (indent-to-column margin))))
      (buffer-string))
    "")))


(defun iota4e-view-for-each-part (paper func)
  "Apply FUNC to each part in PAPER.
FUNC should be a function taking two arguments:
 1. the paper PAPER, and
 2. a plist describing the attachment. The plist looks like:
    	 (:index 1 :name \"test123.doc\"
          :mime-type \"application/msword\" :attachment t :size 1234)."
  (dolist (part (iota4e-paper-field paper :parts))
    (funcall func paper part)))


(defvar iota4e-view-mode-map nil
  "Keymap for \"*iota4e-view*\" buffers.")
(unless iota4e-view-mode-map
  (setq iota4e-view-mode-map
    (let ((map (make-sparse-keymap)))

      (define-key map "q" 'iota4e~view-quit-buffer)

      ;; note, 'z' is by-default bound to 'bury-buffer'
      ;; but that's not very useful in this case
      (define-key map "z" 'iota4e~view-quit-buffer)

      (define-key map "s" 'iota4e-headers-search)
      (define-key map "S" 'iota4e-view-search-edit)
      (define-key map "/" 'iota4e-view-search-narrow)

      (define-key map (kbd "<M-left>")  'iota4e-headers-query-prev)
      (define-key map (kbd "<M-right>") 'iota4e-headers-query-next)

      (define-key map "b" 'iota4e-headers-search-bookmark)
      (define-key map "B" 'iota4e-headers-search-bookmark-edit)

      (define-key map "v" 'iota4e-view-verify-paper-popup)

      (define-key map "g" 'iota4e-view-go-to-url)

      (define-key map "." 'iota4e-view-raw-paper)
      (define-key map "," 'iota4e-view-pdf-paper)
      (define-key map "|" 'iota4e-view-pipe)
      (define-key map "a" 'iota4e-view-action)

      ;; toggle header settings
      (define-key map "O" 'iota4e-headers-change-sorting)
      (define-key map "Q" 'iota4e-headers-toggle-full-search)

      ;; change the number of headers
      (define-key map (kbd "C-+") 'iota4e-headers-split-view-grow)
      (define-key map (kbd "C--") 'iota4e-headers-split-view-shrink)
      (define-key map (kbd "<C-kp-add>") 'iota4e-headers-split-view-grow)
      (define-key map (kbd "<C-kp-subtract>") 'iota4e-headers-split-view-shrink)

      ;; intra-paper navigation
      (define-key map (kbd "SPC") 'iota4e-view-scroll-up-or-next)
      (define-key map (kbd "<home>") 'beginning-of-buffer)
      (define-key map (kbd "<end>") 'end-of-buffer)
      (define-key map (kbd "RET") 'iota4e-scroll-up)
      (define-key map (kbd "<backspace>") 'iota4e-scroll-down)

      ;; navigation between papers
      (define-key map "p" 'iota4e-view-headers-prev)
      (define-key map "n" 'iota4e-view-headers-next)
      ;; the same
      (define-key map (kbd "<M-down>") 'iota4e-view-headers-next)
      (define-key map (kbd "<M-up>") 'iota4e-view-headers-prev)

      ;; switching to view mode (if it's visible)
      (define-key map "y" 'iota4e-select-other-view)

      ;; misc
      (define-key map "w" 'visual-line-mode)

      (define-key map "$" 'iota4e-show-log)
      (define-key map "H" 'iota4e-display-manual)

      ;; menu
      (define-key map [menu-bar] (make-sparse-keymap))
      (let ((menumap (make-sparse-keymap "View")))
	(define-key map [menu-bar headers] (cons "View" menumap))

	(define-key menumap [quit-buffer]
	  '("Quit view" . iota4e~view-quit-buffer))
	(define-key menumap [display-help] '("Help" . iota4e-display-manual))

	(define-key menumap [sepa0] '("--"))
	(define-key menumap [wrap-lines]
	  '("Toggle wrap lines" . visual-line-mode))
	(define-key menumap [hide-cited]
	  '("Toggle hide cited" . iota4e-view-toggle-hide-cited))
	(define-key menumap [raw-view]
	  '("View raw paper" . iota4e-view-raw-paper))
	(define-key menumap [pipe]
	  '("Pipe through shell" . iota4e-view-pipe))
	;; (define-key menumap [inspect]
	;;   '("Inspect with guile" . iota4e-inspect-paper))

	(define-key menumap [sepa8] '("--"))
	(define-key menumap [open-att]
	  '("Open attachment" . iota4e-view-open-attachment))
	(define-key menumap [extract-att]
	  '("Extract attachment" . iota4e-view-save-attachment))
	(define-key menumap [goto-url]
	  '("Visit URL" . iota4e-view-go-to-url))

	(define-key menumap [sepa1] '("--"))
	(define-key menumap [sepa2] '("--"))
	(define-key menumap [compose-new]  '("Compose new" . iota4e-compose-new))
	(define-key menumap [forward]  '("Forward" . iota4e-compose-forward))
	(define-key menumap [reply]  '("Reply" . iota4e-compose-reply))
	(define-key menumap [sepa3] '("--"))


	(define-key menumap [query-next]
	  '("Next query" . iota4e-headers-query-next))
	(define-key menumap [query-prev]
	  '("Previous query" . iota4e-headers-query-prev))
	(define-key menumap [narrow-search]
	  '("Narrow search" . iota4e-headers-search-narrow))
	(define-key menumap [bookmark]
	  '("Search bookmark" . iota4e-headers-search-bookmark))
	(define-key menumap [jump]
	  '("Jump to maildir" . iota4e~headers-jump-to-maildir))
	(define-key menumap [refresh]
	  '("Refresh" . iota4e-headers-rerun-search))
	(define-key menumap [search]
	  '("Search" . iota4e-headers-search))


	(define-key menumap [sepa4] '("--"))
	(define-key menumap [next]  '("Next" . iota4e-view-headers-next))
	(define-key menumap [previous]  '("Previous" . iota4e-view-headers-prev)))
      map)))

(fset 'iota4e-view-mode-map iota4e-view-mode-map)

(defcustom iota4e-view-mode-hook nil
  "Hook run when entering Iota3e-View mode."
  :options '(turn-on-visual-line-mode)
  :type 'hook
  :group 'iota4e-view)

(defvar iota4e-view-mode-abbrev-table nil)
(define-derived-mode iota4e-view-mode special-mode "iota4e:view"
  "Major mode for viewing an e-mail paper in iota4e.
\\{iota4e-view-mode-map}."
  (use-local-map iota4e-view-mode-map)

  (make-local-variable 'iota4e~view-headers-buffer)
  (make-local-variable 'iota4e~view-paper)
  (make-local-variable 'iota4e~view-link-map)
  (make-local-variable 'iota4e~view-attach-map)
  (make-local-variable 'iota4e~view-cited-hidden)

  (setq buffer-undo-list t) ;; don't record undo info

  ;; autopair mode gives error when pressing RET
  ;; turn it off
  (when (boundp 'autopair-dont-activate)
    (setq autopair-dont-activate t)))

(defun iota4e~view-browse-url-func (url)
  "Return a function that executes `browse-url' with URL.
What browser is called is depending on
`browse-url-browser-function' and `browse-url-mailto-function'."
  (save-match-data
    (if (string-match "^mailto:" url)
      (lexical-let ((url url))
	(lambda ()
	  (interactive)
	  (iota4e~compose-browse-url-mail url)))
      (lexical-let ((url url))
	(lambda ()
	  (interactive)
	  (browse-url url))))))

(defun iota4e~view-browse-url-from-binding (&optional url)
  "View in browser the url at point, or click location.
If the optional argument URL is provided, browse that instead.
If the url is mailto link, start writing an email to that address."
  (interactive)
  (let* (( url (or url (iota4e~view-get-property-from-event 'iota4e-url))))
    (when url
      (if (string-match-p "^mailto:" url)
	  (iota4e~compose-browse-url-mail url)
	(browse-url url)))))

(defmacro iota4e~view-in-headers-context (&rest body)
  "Evaluate BODY in the context of the headers buffer connected to
this view."
  `(progn
     (unless (buffer-live-p iota4e~view-headers-buffer)
       (iota4e-error "no headers-buffer connected"))
     (let* ((paper (iota4e-paper-at-point))
            (docid (iota4e-paper-field paper :docid)))
       (iota4e-message (format "docid %s" docid))
       (unless docid
         (iota4e-error "paper without docid: action is not possible."))
       (with-current-buffer iota4e~view-headers-buffer
         (if (iota4e~headers-goto-docid docid)
             ,@body
           (iota4e-error "cannot find paper in headers buffer."))))))

(defun iota4e-view-headers-next(&optional n)
  "Move point to the next paper header in the headers buffer
connected with this paper view. If this succeeds, return the new
docid. Otherwise, return nil. Optionally, takes an integer
N (prefix argument), to the Nth next header."
  (interactive "P")
  (iota4e~view-in-headers-context
    (iota4e~headers-move (or n 1))))

(defun iota4e-view-headers-prev(&optional n)
  "Move point to the previous paper header in the headers buffer
connected with this paper view. If this succeeds, return the new
docid. Otherwise, return nil. Optionally, takes an integer
N (prefix argument), to the Nth previous header."
  (interactive "P")
  (iota4e~view-in-headers-context
    (iota4e~headers-move (- (or n 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interactive functions

(defun iota4e-view-refresh ()
  "Redisplay the current paper."
  (interactive)
  (iota4e-view iota4e~view-paper iota4e~view-headers-buffer)
  (setq iota4e~view-cited-hidden nil))

(defun iota4e-view-search-narrow ()
  "Run `iota4e-headers-search-narrow' in the headers buffer."
  (interactive)
  (iota4e~view-in-headers-context (iota4e-headers-search-narrow nil)))

(defun iota4e-view-search-edit ()
  "Run `iota4e-headers-search-edit' in the headers buffer."
  (interactive)
  (iota4e~view-in-headers-context (iota4e-headers-search-edit)))

;; handler-function to handle the response we get from the server when we
;; want to do something with one of the attachments.
(defun iota4e~view-temp-handler (path what docid param)
  "Handler function for doing things with temp files (ie.,
attachments) in response to a (iota4e~proc-extract 'temp ... )."
  (cond
    ((string= what "open-with")
      ;; 'param' will be the program to open-with
      (start-process "*iota4e-open-with-proc*" "*iota4e-open-with*" param path))
    ((string= what "pipe")
      ;; 'param' will be the pipe command, path the infile for this
      (iota4e-process-file-through-pipe path param))
    ;; if it's iota4e, it's some embedded paper; 'param' may contain the docid of
    ;; the parent paper.
    ((string= what "iota4e")
      ;; remember the mapping path->docid, which maps the path of the embedded
      ;; paper to the docid of its parent
      (puthash path docid iota4e~path-parent-docid-map)
      (iota4e~proc-view-path path))
    ((string= what "emacs")
      (find-file path)
      ;; make the buffer read-only since it usually does not make
      ;; sense to edit the temp buffer; use C-x C-q if you insist...
      (setq buffer-read-only t))
    (t (iota4e-error "Unsupported action %S" what))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun iota4e~view-split-view-p ()
  "Return t if we're in split-view, nil otherwise."
  (member iota4e-split-view '(horizontal vertical)))

(defun iota4e-view-scroll-up-or-next ()
  "Scroll-up the current paper.
If `iota4e-view-scroll-to-next' is non-nil, and we can't scroll-up
anymore, go the next paper."
  (interactive)
  (condition-case nil
    (scroll-up)
    (error
      (when iota4e-view-scroll-to-next
	(iota4e-view-headers-next)))))

(defun iota4e-scroll-up ()
  "Scroll text of selected window up one line."
  (interactive)
  (scroll-up 1))

(defun iota4e-scroll-down ()
  "Scroll text of selected window down one line."
  (interactive)
  (scroll-down 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst iota4e~view-raw-buffer-name " *iota4e-raw-view*"
  "*internal* Name for the raw paper view buffer")

(defun iota4e-view-raw-paper ()
  "Display the raw contents of paper at point in a new buffer."
  (interactive)
  (let ((path (concat (file-name-as-directory (iota4e-paper-field-at-point :path))
                      (iota4e-paper-field-at-point :bibfile)))
        (buf (get-buffer-create iota4e~view-raw-buffer-name)))
    (unless (and path (file-readable-p path))
      (iota4e-error "Not a readable file: %S" path))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-file-contents path)
        (view-mode)
        (goto-char (point-min))))
    (switch-to-buffer buf)))

(defconst iota4e~view-pdf-buffer-name " *iota4e-pdf-view*"
  "*internal* Name for the pdf paper view buffer")

(defun iota4e-view-pdf-paper ()
  "Display the raw contents of paper at point in a new buffer."
  (interactive)
  (let ((path (concat (file-name-as-directory (iota4e-paper-field-at-point :path))
                      (iota4e-paper-field-at-point :pdffile)))
        (buf (get-buffer-create iota4e~view-pdf-buffer-name)))
    (unless (and path (file-readable-p path))
      (iota4e-error "Not a readable file: %S" path))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-file-contents path)
        (doc-view-mode)
        (goto-char (point-min))))
    (switch-to-buffer buf)))


(defun iota4e~view-quit-buffer ()
  "Quit the iota4e-view buffer.
This is a rather complex function, to ensure we don't disturb
other windows."
  (interactive)
  (unless (eq major-mode 'iota4e-view-mode)
    (iota4e-error "Must be in iota4e-view-mode (%S)" major-mode))
  (let ((curbuf (current-buffer)) (curwin (selected-window))
	 (headers-win))
    (walk-windows
      (lambda (win)
	;; check whether the headers buffer window is visible
	(when (eq iota4e~view-headers-buffer (window-buffer win))
	  (setq headers-win win))
	;; and kill any _other_ (non-selected) window that shows the current
	;; buffer
	(when
	  (and
	    (eq curbuf (window-buffer win)) ;; does win show curbuf?
	    (not (eq curwin win))	    ;; but it's not the curwin?
	    (not (one-window-p))) ;; and not the last one on the frame?
	  (delete-window win))))  ;; delete it!
    ;; now, all *other* windows should be gone.
    ;; if the headers view is also visible, kill ourselves + window; otherwise
    ;; switch to the headers view
    (if (window-live-p headers-win)
      ;; headers are visible
      (progn
	(kill-buffer-and-window) ;; kill the view win
        (setq iota4e~headers-view-win nil)
	(select-window headers-win)) ;; and switch to the headers win...
      ;; headers are not visible...
      (progn
	(kill-buffer)
        (setq iota4e~headers-view-win nil)
	(when (buffer-live-p iota4e~view-headers-buffer)
	  (switch-to-buffer iota4e~view-headers-buffer))))))

(provide 'iota4e-view)
;; end of iota4e-view

