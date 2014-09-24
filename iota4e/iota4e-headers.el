(eval-when-compile (byte-compile-disable-warning 'cl-functions))
(require 'cl)

(require 'fringe)
(require 'hl-line)

(require 'iota4e-utils)    ;; utility functions
(require 'iota4e-proc)
(require 'iota4e-vars)
(require 'iota4e-paper)

;; the headers view
(defgroup iota4e-headers nil
  "Settings for the headers view."
  :group 'iota4e)

(defcustom iota4e-headers-fields
  '( (:1au    . 20)
     (:journal   .  12)
     (:year   .  6)
     (:title  . nil))
  "A list of header fields to show in the headers buffer.
Each element has the form (HEADER . WIDTH), where HEADER is one
of the available headers (see `iota4e-header-info') and WIDTH is
the respective width in characters.  A width of `nil' means
'unrestricted', and this is best reserved for the
rightmost (last) field."
  :type `(repeat (cons (choice ,@(mapcar (lambda (h)
					   (list 'const :tag
						 (plist-get (cdr h) :help)
						 (car h)))
					 iota4e-header-info))
		       (choice (integer :tag "width")
			       (const :tag "unrestricted width" nil))))
  :group 'iota4e-headers)

(defcustom iota4e-headers-visible-lines 10
  "Number of lines to display in the header view when using the
horizontal split-view. This includes the header-line at the top,
and the mode-line."
  :type 'integer
  :group 'iota4e-headers)

(defcustom iota4e-headers-visible-columns 30
  "Number of columns to display for the header view when using the
vertical split-view."
  :type 'integer
  :group 'iota4e-headers)

(defcustom iota4e-headers-auto-update t
  "Whether to automatically update the current headers buffer if an
indexing operation showed changes."
  :type 'boolean
  :group 'iota4e-headers)

(defcustom iota4e-headers-results-limit 50
  "Maximumm number of results to show; this affects performance
quite a bit, especially when `iota4e-headers-include-related' is
non-nil. Set to -1 for no limits, and you temporarily (for one
query) ignore the limit by pressing a C-u before invoking the
search."
  :type '(choice (const :tag "Unlimited" -1)
                 (integer :tag "Limit"))
  :group 'iota4e-headers)

(defvar iota4e-headers-full-search nil
  "Whether to show all results.
If this is nil show results up to `iota4e-search-results-limit')")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; internal variables/constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; docid cookies
(defconst iota4e~headers-docid-pre (purecopy "\376")
  "Each header starts (invisibly) with the `iota4e~headers-docid-pre',
followed by the docid, followed by `iota4e~headers-docid-post'.")
(defconst iota4e~headers-docid-post (purecopy "\377")
  "Each header starts (invisibly) with the `iota4e~headers-docid-pre',
followed by the docid, followed by `iota4e~headers-docid-post'.")

(defvar iota4e~headers-view-win nil
  "The view window connected to this headers view.")

(defvar iota4e~headers-sort-field-choices
  '( ("docid"	. :docid)
     ("title"   . :title)
     ("year"    . :year)
     ("journal"    . :journal)
     ("1au"     . :1au))
  "List of cells describing the various sort-options.
In the format needed for `iota4e-read-option'.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun iota4e~headers-clear ()
  "Clear the header buffer and related data structures."
  (when (buffer-live-p iota4e~headers-buffer)
    (let ((inhibit-read-only t))
      (with-current-buffer iota4e~headers-buffer
	(erase-buffer)
	(iota4e~mark-clear)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handler functions
;;
;; next are a bunch of handler functions; those will be called from iota4e~proc in
;; response to output from the server process

(defun iota4e~headers-view-handler (paper)
  "Handler function for displaying a paper."
  (iota4e-view paper iota4e~headers-buffer))

(defun iota4e~headers-update-handler (paper is-move)
  "Update handler, will be called when a paper has been updated
in the database. This function will update the current list of
headers."
  (when (buffer-live-p iota4e~headers-buffer)
    (with-current-buffer iota4e~headers-buffer
      (let* ((docid (iota4e-paper-field paper :docid))
 	      (point (iota4e~headers-docid-pos docid)))
	(when point ;; is the paper present in this list?

	  ;; if it's marked, unmark it now
	  (when (iota4e-mark-docid-marked-p docid)
	    (iota4e-mark-set 'unmark))

 	  ;; re-use the thread info from the old one; this is needed because
 	  ;; *update* papers don't have thread info by themselves (unlike
 	  ;; search results)
	  ;; since we still have the search results, re-use
 	  ;; those
 	  (plist-put paper :thread
 	    (iota4e~headers-field-for-docid docid :thread))

	  ;; first, remove the old one (otherwise, we'd have two headers with
	  ;; the same docid...
	  (iota4e~headers-remove-handler docid)

	  ;; if we we're actually viewing this paper (in iota4e-view mode), we
	  ;; update it; that way, the flags can be updated, as well as the path
	  ;; (which is useful for viewing the raw paper)
	  (let ((viewbuf (get-buffer iota4e~view-buffer-name)))
	    (when (and viewbuf (buffer-live-p viewbuf))
	      (with-current-buffer viewbuf
		(when (eq docid (plist-get iota4e~view-paper :docid))
		  (iota4e-view paper iota4e~headers-buffer)))))

	  ;; now, if this update was about *moving* a paper, we don't show it
	  ;; anymore (of course, we cannot be sure if the paper really no
	  ;; longer matches the query, but this seem a good heuristic.
	  ;; if it was only a flag-change, show the paper with its updated flags.
	  (unless is-move
	    (iota4e~headers-header-handler paper point))

	  ;; attempt to highlight the corresponding line and make it visible
	  (iota4e~headers-highlight docid))))))


(defun iota4e~headers-remove-handler (docid)
  "Remove handler, will be called when a paper with DOCID has
been removed from the database. This function will hide the removed
paper from the current list of headers. If the paper is not
present, don't do anything."
  (when (buffer-live-p iota4e~headers-buffer)
    (with-current-buffer iota4e~headers-buffer
      (iota4e~headers-remove-header docid t))))


;; note: this function is very performance-sensitive
(defun iota4e~headers-header-handler (paper &optional point)
  "Create a one line description of PAPER in this buffer, at POINT,
if provided, or at the end of the buffer otherwise."
  (let ((docid (iota4e-paper-field paper :docid)) (line ""))
    (dolist (f-w iota4e-headers-fields)
      (let ((field (car f-w)) (width (cdr f-w))
            (val (iota4e-paper-field paper (car f-w))) (str))
        (setq str
              (case field
                ((:title :1au :journal) val)
                (:year (format "%d" val))
                (t (iota4e-error "Unsupported header field (%S)" field))))
        (when str
          (setq line
                (concat line
                        (if (not width)
                            str
                          (truncate-string-to-width str width 0 ?\s t)) " ")))))
    ;; now, append the header line
    (iota4e~headers-add-header line docid point paper)))


(defconst iota4e~no-matches     (purecopy "No matching papers found"))
(defconst iota4e~end-of-results (purecopy "End of search results"))

(defun iota4e~headers-found-handler (count)
  "Create a one line description of the number of headers found
after the end of the search results."
  (when (buffer-live-p iota4e~headers-buffer)
  (with-current-buffer iota4e~headers-buffer
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t)
	     (str (if (= 0 count)
		    iota4e~no-matches
		    iota4e~end-of-results)))
	(insert (propertize str 'face 'iota4e-system-face 'intangible t))
	(unless (= 0 count)
	  (iota4e-message "Found %d matching paper%s"
	    count (if (= 1 count) "" "s"))
	  ;; highlight the first message
	  (iota4e~headers-highlight (iota4e~headers-docid-at-point (point-min)))))))))

;;; headers-mode and mode-map ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar iota4e-headers-mode-map nil
  "Keymap for *iota4e-headers* buffers.")
(unless iota4e-headers-mode-map
  (setq iota4e-headers-mode-map
    (let ((map (make-sparse-keymap)))

      (define-key map "s" 'iota4e-headers-search)
      (define-key map "S" 'iota4e-headers-search-edit)

      (define-key map "/" 'iota4e-headers-search-narrow)

      (define-key map (kbd "<M-left>")  'iota4e-headers-query-prev)
      (define-key map (kbd "\\")        'iota4e-headers-query-prev)
      (define-key map (kbd "<M-right>") 'iota4e-headers-query-next)

      (define-key map "b" 'iota4e-headers-search-bookmark)
      (define-key map "B" 'iota4e-headers-search-bookmark-edit)

      (define-key map "O" 'iota4e-headers-change-sorting)
      (define-key map "Q" 'iota4e-headers-toggle-full-search)

      (define-key map "q" 'iota4e~headers-quit-buffer)
      (define-key map "z" 'iota4e~headers-quit-buffer)

      ;; navigation between papers
      (define-key map "p" 'iota4e-headers-prev)
      (define-key map "n" 'iota4e-headers-next)
      (define-key map (kbd "<M-up>") 'iota4e-headers-prev)
      (define-key map (kbd "<M-down>") 'iota4e-headers-next)

      ;; change the number of headers
      (define-key map (kbd "C-+") 'iota4e-headers-split-view-grow)
      (define-key map (kbd "C--") 'iota4e-headers-split-view-shrink)
      (define-key map (kbd "<C-kp-add>") 'iota4e-headers-split-view-grow)
      (define-key map (kbd "<C-kp-subtract>") 'iota4e-headers-split-view-shrink)


      ;; switching to view mode (if it's visible)
      (define-key map "y" 'iota4e-select-other-view)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define-key map "A" 'iota4e-headers-action)

      (define-key map (kbd "RET") 'iota4e-headers-view-paper)
      (define-key map [mouse-2]   'iota4e-headers-view-paper)

      (define-key map "$" 'iota4e-show-log)
      (define-key map "H" 'iota4e-display-manual)

      ;; menu
      (define-key map [menu-bar] (make-sparse-keymap))
      (let ((menumap (make-sparse-keymap "Headers")))
	(define-key map [menu-bar headers] (cons "Headers" menumap))

	(define-key menumap [iota4e~headers-quit-buffer]
	  '("Quit view" . iota4e~headers-quit-buffer))
	(define-key menumap [display-help] '("Help" . iota4e-display-manual))

	(define-key menumap [sepa0] '("--"))

	(define-key menumap [sepa1] '("--"))

	(define-key menumap [sepa2] '("--"))

	(define-key menumap [query-next]  '("Next query" . iota4e-headers-query-next))
	(define-key menumap [query-prev]  '("Previous query" .
					     iota4e-headers-query-prev))
	(define-key menumap [narrow-search] '("Narrow search" .
					       iota4e-headers-search-narrow))
	(define-key menumap [bookmark]  '("Search bookmark" .
					   iota4e-headers-search-bookmark))
	(define-key menumap [refresh]  '("Refresh" . iota4e-headers-rerun-search))
	(define-key menumap [search]  '("Search" . iota4e-headers-search))

	(define-key menumap [sepa3] '("--"))

	(define-key menumap [view]  '("View" . iota4e-headers-view-paper))
	(define-key menumap [next]  '("Next" . iota4e-headers-next))
	(define-key menumap [previous]  '("Previous" . iota4e-headers-prev))
	(define-key menumap [sepa4] '("--")))
      map)))
(fset 'iota4e-headers-mode-map iota4e-headers-mode-map)


(defun iota4e~header-line-format ()
  "Get the format for the header line."
  (cons
    (make-string
      (+ 0 (floor (fringe-columns 'left t))) ?\s)
    (mapcar
      (lambda (item)
	(let* ((field (car item)) (width (cdr item))
		(info (cdr (assoc field iota4e-header-info)))
		(sortable (plist-get info :sortable))
		(help (plist-get info :help))
		(uparrow   (if iota4e-use-fancy-chars " ▲" " ^"))
		(downarrow (if iota4e-use-fancy-chars " ▼" " V"))
		;; triangle to mark the sorted-by column
		(arrow
		  (when (and sortable (eq (car item) iota4e~headers-sort-field))
		    (if (eq iota4e~headers-sort-direction 'descending) downarrow uparrow)))
		(name (concat (plist-get info :shortname) arrow))
		(map (make-sparse-keymap)))
	  (when sortable
	    (define-key map [header-line mouse-1]
	      (lambda (&optional e)
		;; getting the field, inspired by `tabulated-list-col-sort'
		(interactive "e")
		(let* ((obj (posn-object (event-start e)))
			(field
			  (and obj (get-text-property 0 'field (car obj)))))
		  ;; "t": if we're already sorted by field, the sort-order is
		  ;; changed
		  (iota4e-headers-change-sorting field t)))))
	  (concat
	    (propertize
	      (if width
		(truncate-string-to-width name width 0 ?\s t)
		name)
	      'face (when arrow 'bold)
	      'help-echo help
	      'mouse-face (when sortable 'highlight)
	      'keymap (when sortable map)
	      'field field) " ")))
      iota4e-headers-fields)))


(defvar iota4e-headers-mode-abbrev-table nil)
(define-derived-mode iota4e-headers-mode special-mode
    "iota4e:headers"
  "Major mode for displaying iota4e search results.
\\{iota4e-headers-mode-map}."
  (use-local-map iota4e-headers-mode-map)

  (make-local-variable 'iota4e~headers-proc)
  (make-local-variable 'iota4e~highlighted-docid)
  (make-local-variable 'global-mode-string)
  (set (make-local-variable 'hl-line-face) 'iota4e-header-highlight-face)

  ;; maybe update the current headers upon indexing changes
  (add-hook 'iota4e-index-updated-hook
    (defun iota4e~headers-auto-update ()
      "Update the current headers buffer after indexing has brought
some changes, `iota4e-headers-auto-update' is non-nil and there is no
user-interaction ongoing."
      (when (and iota4e-headers-auto-update       ;; iotast be set
	      (zerop (iota4e-mark-marks-num))     ;; non active marks
	      (not (active-minibuffer-window))) ;; no user input
	(with-current-buffer iota4e~headers-buffer
	  (iota4e-headers-rerun-search)))) nil t)

  (setq
    truncate-lines t
    buffer-undo-list t ;; don't record undo information
    overwrite-mode 'overwrite-mode-binary
    header-line-format (iota4e~header-line-format))

  (hl-line-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; higlighting
(defvar iota4e~highlighted-docid nil
  "The highlighted docid")

(defun iota4e~headers-highlight (docid)
  "Highlight the header with DOCID, or do nothing if it's not found.
Also, unhighlight any previously highlighted headers."
  (with-current-buffer iota4e~headers-buffer
    (save-excursion
      ;; first, unhighlight the previously highlighted docid, if any
      (when (and iota4e~highlighted-docid
	      (iota4e~headers-goto-docid iota4e~highlighted-docid))
	(hl-line-unhighlight))
      ;; now, highlight the new one
      (when (iota4e~headers-goto-docid docid)
	(hl-line-highlight)))
    (setq iota4e~highlighted-docid docid)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun iota4e~headers-select-window ()
  "When there is a visible window for the headers buffer, make sure
to select it. This is needed when adding new headers, otherwise
adding a lot of new headers looks really choppy."
  (let ((win (get-buffer-window iota4e~headers-buffer)))
    (when win (select-window win))))

;;;; headers in the buffer are prefixed by an invisible string with the docid
;;;; followed by an EOT ('end-of-transmission', \004, ^D) non-printable ascii
;;;; character. this string also has a text-property with the docid. the former
;;;; is used for quickly finding a certain header, the latter for retrieving the
;;;; docid at point without string matching etc.

(defsubst iota4e~headers-docid-cookie (docid)
  "Create an invisible string containing DOCID; this is to be used
at the beginning of lines to identify headers."
  (propertize (format "%s%d%s"
		iota4e~headers-docid-pre docid iota4e~headers-docid-post)
    'docid docid 'invisible t));;

(defsubst iota4e~headers-docid-at-point (&optional point)
  "Get the docid for the header at POINT, or at current (point) if
nil. Returns the docid, or nil if there is none."
    (save-excursion
      (when point
	(goto-char point))
      (get-text-property (line-beginning-position) 'docid)))

(defun iota4e~headers-goto-docid (docid &optional to-mark)
  "Go to the beginning of the line with the header with docid
DOCID, or nil if it cannot be found. If the optional TO-MARK is
non-nil, go to the point directly *after* the docid-cookie instead
of the beginning of the line."
  (let ((oldpoint (point)) (newpoint))
    (goto-char (point-min))
    (setq newpoint
      (search-forward (iota4e~headers-docid-cookie docid) nil t))
    (unless to-mark
      (if (null newpoint)
	(goto-char oldpoint) ;; not found; restore old pos
	(progn
	  (beginning-of-line) ;; found, move to beginning of line
	  (setq newpoint (point)))))
    newpoint)) ;; return the point, or nil if not found

(defsubst iota4e~headers-docid-pos (docid)
  "Return the pos of the beginning of the line with the header with
docid DOCID, or nil if it cannot be found."
  (let ((pos))
    (save-excursion
      (setq pos (iota4e~headers-goto-docid docid)))
    pos))

(defsubst iota4e~headers-field-for-docid (docid field)
  "Get FIELD (a symbol, see `iota4e-headers-names') for the paper
with DOCID which must be present in the headers buffer."
  (save-excursion
    (when (iota4e~headers-goto-docid docid)
      (iota4e-paper-field (iota4e-paper-at-point) field))))

(defsubst iota4e~headers-add-header (str docid point &optional paper)
  "Add header STR with DOCID to the buffer at POINT if non-nil, or
at (point-max) otherwise. If PAPER is not nil, add it as the
text-property `paper'."
  (when (buffer-live-p iota4e~headers-buffer)
    (with-current-buffer iota4e~headers-buffer
      (let ((inhibit-read-only t)
	     (is-first-header (= (point-min) (point-max))))
	(save-excursion
	  (goto-char (if point point (point-max)))
	  (insert
	    (propertize
	      (concat
		(iota4e~headers-docid-cookie docid)
		str "\n")
	      'docid docid 'paper paper)))))))

(defun iota4e~headers-remove-header (docid &optional ignore-missing)
  "Remove header with DOCID at point.
When IGNORE-MISSING is non-nill, don't raise an error when the
docid is not found."
  (with-current-buffer iota4e~headers-buffer
    (if (iota4e~headers-goto-docid docid)
      (let ((inhibit-read-only t))
	(delete-region (line-beginning-position) (line-beginning-position 2)))
      (unless ignore-missing
	(iota4e-error "Cannot find paper with docid %S" docid)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun iota4e~headers-search-execute (expr ignore-history)
  "Search in the iota database for EXPR, and switch to the output
buffer for the results. If IGNORE-HISTORY is true, do *not* update
the query history stack."
  ;; note: we don't want to update the history if this query comes from
  ;; `iota4e~headers-query-next' or `iota4e~headers-query-prev'.
  (iota4e-hide-other-iota4e-buffers)
  (let* ((buf (get-buffer-create iota4e~headers-buffer-name))
         (inhibit-read-only t)
         (maxnum (unless iota4e-headers-full-search iota4e-headers-results-limit)))
    (with-current-buffer buf
      (iota4e-headers-mode)
      (unless ignore-history
        ;; save the old present query to the history list
        (when iota4e~headers-last-query
          (iota4e~headers-push-query iota4e~headers-last-query 'past)))
      (setq
       iota4e~headers-buffer buf
       mode-name "iota4e-headers"
       iota4e~headers-last-query expr
       global-mode-string (propertize iota4e~headers-last-query
                                      'face 'iota4e-title-face)))
    (switch-to-buffer buf)
    (iota4e~proc-find
     expr
     iota4e~headers-sort-field
     iota4e~headers-sort-direction
     maxnum)))

(defun iota4e~headers-redraw-get-view-window ()
  "Close all windows, redraw the headers buffer based on the value
of `iota4e-split-view', and return a window for the paper view."
  (iota4e-hide-other-iota4e-buffers)
  (unless (buffer-live-p iota4e~headers-buffer)
    (iota4e-error "No headers buffer available"))
  (switch-to-buffer iota4e~headers-buffer)
  ;; kill the existing view win
  (when (buffer-live-p iota4e~view-buffer)
    (kill-buffer iota4e~view-buffer))
  ;; get a new view window
  (setq iota4e~headers-view-win
    (cond
      ((eq iota4e-split-view 'horizontal) ;; split horizontally
	(split-window-vertically iota4e-headers-visible-lines))
      ((eq iota4e-split-view 'vertical) ;; split vertically
	(split-window-horizontally iota4e-headers-visible-columns))
      (t ;; no splitting; just use the currently selected one
	(selected-window))))
  iota4e~headers-view-win)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; search-based marking

(defun iota4e-headers-for-each (func)
  "Call FUNC for each header, moving point to the header.
FUNC takes one argument, the paper s-expression for the corresponding
header."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward iota4e~headers-docid-pre nil t)
      ;; not really sure why we need to jump to bol; we we need
      ;; to, otherwise we miss lines sometimes...
      (let ((paper (get-text-property (line-beginning-position) 'paper)))
	(when paper
	  (funcall func paper))))))

(defvar iota4e~headers-regexp-hist nil
  "History list of regexps used.")

(defun iota4e-headers-mark-for-each-if (markpair mark-pred &optional param)
  "Mark all headers for which predicate function MARK-PRED returns
non-nil with MARKPAIR. MARK-PRED is function that takes two
arguments, PAPER (the paper at point) and PARAM (a user-specified
parameter). MARKPAIR is a cell (MARK . TARGET); see
`iota4e-mark-at-point' for details about marks."
  (iota4e-headers-for-each
    (lambda (paper)
      (when (funcall mark-pred paper param)
	(iota4e-mark-at-point (car markpair) (cdr markpair))))))

(defun iota4e-headers-mark-pattern ()
  "Ask user for a kind of mark (move, delete etc.), a field to
match and a regular expression to match with. Then, mark all
matching papers with that mark."
  (interactive)
  (let ((markpair (iota4e~mark-get-markpair "Mark matched papers with: " t))
	 (field (iota4e-read-option "Field to match: "
		  '( ("subject" . :subject)
		     ("from"    . :from)
		     ("to"      . :to))))
	  (pattern (read-string
		     (iota4e-format "Regexp:")
		     nil 'iota4e~headers-regexp-hist)))
    (iota4e-headers-mark-for-each-if
      markpair
      (lambda (paper param)
	(let* ((do-mark) (value (iota4e-paper-field paper field)))
	  (setq do-mark
	    (if (member field '(:to :from :cc :bcc :reply-to))
	      (find-if (lambda (contact)
			 (let ((name (car contact)) (email (cdr contact)))
			   (or (and name (string-match pattern name))
			     (and email (string-match pattern email))))) value)
	      (string-match pattern (or value "")))))))))

(defun iota4e-headers-mark-custom ()
  "Mark papers based on a user-provided predicate function."
  (interactive)
  (let* ((pred (iota4e-read-option "Match function: "
		 iota4e-headers-custom-markers))
	  (param (when (cdr pred) (eval (cdr pred))))
	  (markpair (iota4e~mark-get-markpair "Mark matched papers with: " t)))
    (iota4e-headers-mark-for-each-if markpair (car pred) param)))

(defun iota4e~headers-get-thread-info (paper what)
  "Get WHAT (a symbol, either path or thread-id) for PAPER."
  (let* ((thread (or (iota4e-paper-field paper :thread)
		   (iota4e-error "No thread info found")))
	  (path  (or (plist-get thread :path)
		   (iota4e-error "No threadpath found"))))
    (case what
      (path path)
      (thread-id
	(save-match-data
	  ;; the thread id is the first segment of the thread path
	  (when (string-match "^\\([[:xdigit:]]+\\):?" path)
	    (match-string 1 path))))
      (otherwise (iota4e-error "Not supported")))))


(defun iota4e-headers-mark-thread (&optional subthread)
  "Mark the thread at point.
If SUBTHREAD is non-nil, marking is limited to the paper at
point and its descendants."
  ;; the tread id is shared by all papers in a thread
  (interactive "P")
  (let* ((paper (iota4e-paper-at-point))
	  (thread-id (iota4e~headers-get-thread-info paper 'thread-id))
	  (path     (iota4e~headers-get-thread-info paper 'path))
	  (markpair
	    (iota4e~mark-get-markpair
	      (if subthread "Mark subthread with: " "Mark whole thread with: ")
	      t))
	  (last-marked-point))
    (iota4e-headers-for-each
      (lambda (mypaper)
 	(let ((my-thread-id (iota4e~headers-get-thread-info mypaper 'thread-id)))
	  (if subthread
	    ;; subthread matching; mypaper's thread path should have path as its
	    ;; prefix
	    (when (string-match (concat "^" path)
		    (iota4e~headers-get-thread-info mypaper 'path))
	      (iota4e-mark-at-point (car markpair) (cdr markpair))
	      (setq last-marked-point (point)))
	    ;; nope; not looking for the subthread; looking for the whole thread
	    (when (string= thread-id
		    (iota4e~headers-get-thread-info mypaper 'thread-id))
	      (iota4e-mark-at-point (car markpair) (cdr markpair))
	      (setq last-marked-point (point)))))))
    (when last-marked-point
      (goto-char last-marked-point)
      (iota4e-headers-next))))

(defun iota4e-headers-mark-subthread ()
  "Like `iota4e-mark-thread', but only for a sub-thread."
  (interactive)
  (iota4e-headers-mark-thread t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; the query past / present / future ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar iota4e~headers-query-past nil
  "Stack of queries before the present one.")
(defvar iota4e~headers-query-future nil
  "Stack of queries after the present one.")
(defvar iota4e~headers-query-stack-size 20
  "Maxiiotam size for the query stacks.")

(defun iota4e~headers-push-query (query where)
  "Push QUERY to one of the query stacks.
WHERE is a symbol telling us where to push; it's a symbol, either
'future or 'past. Functional also removes duplicats, limits the
stack size."
  (let ((stack
	  (case where
	    (past   iota4e~headers-query-past)
	    (future iota4e~headers-query-future))))
     ;; only add if not the same item
    (unless (and stack (string= (car stack) query))
      (push query stack)
      ;; limit the stack to `iota4e~headers-query-stack-size' elements
      (when (> (length stack) iota4e~headers-query-stack-size)
	(setq stack (subseq stack 0 iota4e~headers-query-stack-size)))
      ;; remove all duplicates of the new element
      (remove-if (lambda (elm) (string= elm (car stack))) (cdr stack))
      ;; update the stacks
      (case where
	(past   (setq iota4e~headers-query-past   stack))
	(future (setq iota4e~headers-query-future stack))))))

(defun iota4e~headers-pop-query (whence)
    "Pop a query from the stack.
WHENCE is a symbol telling us where to get it from; it's a
symbol, either 'future or 'past."
  (case whence
    (past
      (unless iota4e~headers-query-past
	(iota4e-warn "No more previous queries"))
      (pop iota4e~headers-query-past))
    (future
      (unless iota4e~headers-query-future
	(iota4e-warn "No more next queries"))
      (pop iota4e~headers-query-future))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; interactive functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar iota4e~headers-search-hist nil
  "History list of searches.")

(defun iota4e-headers-search (&optional expr prompt edit ignore-history)
  "Search in the iota database for EXPR, and switch to the output
buffer for the results. This is an interactive function which ask
user for EXPR. PROMPT, if non-nil, is the prompt used by this
function (default is \"Search for:\"). If EDIT is non-nil, instead
of executing the query for EXPR, let the user edit the query before
executing it. If IGNORE-HISTORY is true, do *not* update the query
history stack."
  ;; note: we don't want to update the history if this query comes from
  ;; `iota4e~headers-query-next' or `iota4e~headers-query-prev'."
  (interactive)
  (let* ((prompt (iota4e-format (or prompt "Search for: ")))
	  (expr
	    (if edit
	      (read-string prompt expr)
	      (or expr
		(read-string prompt nil 'iota4e~headers-search-hist)))))
    (iota4e~headers-search-execute expr
      ignore-history)))

(defun iota4e-headers-search-edit ()
  "Edit the last search expression."
  (interactive)
  (iota4e-headers-search iota4e~headers-last-query nil t))

(defun iota4e-headers-search-bookmark (&optional expr edit)
  "Search using some bookmarked query EXPR.
If EDIT is non-nil, let the user edit the bookmark before starting
the search."
  (interactive)
  (let ((expr
	  (or expr
	    (iota4e-ask-bookmark (if edit "Select bookmark: " "Bookmark: ")))))
    (iota4e-headers-search expr (when edit "Edit bookmark: ") edit)))

(defun iota4e-headers-search-bookmark-edit ()
  "Edit an existing bookmark before executing it."
  (interactive)
  (iota4e-headers-search-bookmark nil t))


(defun iota4e-headers-search-narrow (filter )
  "Narrow the last search by appending search expression FILTER to
the last search expression."
  (interactive
    (let ((filter
  	    (read-string (iota4e-format "Narrow down to: ")
  	      nil 'iota4e~headers-search-hist nil t)))
      (list filter)))
  (unless iota4e~headers-last-query
    (iota4e-warn "There's nothing to filter"))
  (iota4e-headers-search
    (format "(%s) AND %s" iota4e~headers-last-query filter)))


(defvar iota4e~headers-sort-field :docid
  "Field to sort the headers by.
Field must be a symbol, one of: :docid")

(defvar iota4e~headers-sort-direction 'descending
  "Direction to sort by; a symbol either `descending' (sorting
  Z->A) or `ascending' (sorting A->Z).")

(defun iota4e-headers-change-sorting (&optional field dir)
  "Change the sorting/threading parameters.
FIELD is the field to sort by; DIR is a symbol: either 'ascending,
'descending, 't (meaning: if FIELD is the same as the current
sortfield, change the sort-order) or nil (ask the user)."
  (interactive)
  (let* ((field
	   (or field
	     (iota4e-read-option "Sortfield: " iota4e~headers-sort-field-choices)))
	  ;; note: 'sortable' is either a boolean (meaning: if non-nil, this is
	  ;; sortable field), _or_ another field (meaning: sort by this other field).
	  (sortable (plist-get (cdr (assoc field iota4e-header-info)) :sortable))
	  ;; error check
	  (sortable
	    (if sortable
	      sortable
	      (iota4e-error "Not a sortable field")))
	  (sortfield (if (booleanp sortable) field sortable))
	  (dir
	    (case dir
	      ((ascending descending) dir)
	      ;; change the sort order if field = curfield
	      (t
		(if (eq sortfield iota4e~headers-sort-field)
		  (if (eq iota4e~headers-sort-direction 'ascending)
		    'descending 'ascending)))
	      (iota4e-read-option "Direction: "
		'(("ascending" . 'ascending) ("descending" . 'descending))))))
    (setq
      iota4e~headers-sort-field sortfield
      iota4e~headers-sort-direction dir)
    (iota4e-message "Sorting by %s (%s)"
      (symbol-name sortfield)
      (symbol-name iota4e~headers-sort-direction))
    (iota4e-headers-rerun-search)))

(defun iota4e~headers-toggle (name togglevar dont-refresh)
  "Toggle variable TOGGLEVAR for feature NAME. Unless DONT-REFRESH is non-nil,
re-run the last search."
  (set togglevar (not (symbol-value togglevar)))
  (iota4e-message "%s turned %s%s"
    name
    (if (symbol-value togglevar) "on" "off")
    (if dont-refresh
      " (press 'g' to refresh)" ""))
  (unless dont-refresh
    (iota4e-headers-rerun-search)))

(defun iota4e-headers-toggle-threading (&optional dont-refresh)
  "Toggle `iota4e-headers-show-threads'. With prefix-argument, do
_not_ refresh the last search with the new setting for threading."
  (interactive "P")
  (iota4e~headers-toggle "Threading" 'iota4e-headers-show-threads dont-refresh))

(defun iota4e-headers-toggle-full-search (&optional dont-refresh)
  "Toggle `iota4e-headers-full-search'. With prefix-argument, do
_not_ refresh the last search with the new setting for threading."
  (interactive "P")
  (iota4e~headers-toggle "Full-search"
    'iota4e-headers-full-search dont-refresh))

(defvar iota4e~headers-loading-buf nil
  "A buffer for loading a paper view.")

(defun iota4e~headers-get-loading-buf ()
  "Get a buffer to give feedback while loading a paper view."
  (unless (buffer-live-p iota4e~headers-loading-buf)
    (setq iota4e~headers-loading-buf
      (get-buffer-create " *iota4e-loading*"))
    (with-current-buffer iota4e~headers-loading-buf
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert (propertize "Waiting for paper..."
		  'face 'iota4e-system-face 'intangible t)))
      (setq buffer-read-only t)))
  iota4e~headers-loading-buf)

(defun iota4e-headers-view-paper ()
  "View paper at point.
If there's an existing window for the view, re-use that one. If
not, create a new one, depending on the value of
`iota4e-split-view': if it's a symbol `horizontal' or `vertical',
split the window accordingly; if it is nil, replace the current
window. "
  (interactive)
  (unless (eq major-mode 'iota4e-headers-mode)
    (iota4e-error "Must be in iota4e-headers-mode (%S)" major-mode))
  (let* ((paper (iota4e-paper-at-point))
         (docid (or (iota4e-paper-field paper :docid)
                    (iota4e-warn "No paper at point")))
         ;; decrypt (or not), based on `iota4e-decryption-policy'.
         (viewwin (iota4e~headers-redraw-get-view-window)))
    (unless (window-live-p viewwin)
      (iota4e-error "Cannot get a paper view"))
    (select-window viewwin)
    (switch-to-buffer (iota4e~headers-get-loading-buf))
    (iota4e~proc-view docid)))

(defun iota4e-headers-rerun-search ()
  "Rerun the search for the last search expression."
  (interactive)
  (iota4e-headers-search iota4e~headers-last-query))

(defun iota4e~headers-query-navigate (whence)
  "Execute the previous query from the query stacks.
WHENCE determines where the query is taken from and is a symbol,
either `future' or `past'."
  (let ((query (iota4e~headers-pop-query whence))
	 (where (if (eq whence 'future) 'past 'future)))
    (when query
      (iota4e~headers-push-query iota4e~headers-last-query where)
      (iota4e-headers-search query nil nil t))))

(defun iota4e-headers-query-next ()
  "Execute the previous query from the query stacks."
  (interactive)
  (iota4e~headers-query-navigate 'future))

(defun iota4e-headers-query-prev ()
  "Execute the previous query from the query stacks."
  (interactive)
  (iota4e~headers-query-navigate 'past))

;; forget the past so we don't repeat it :/
(defun iota4e-headers-forget-queries ()
  "Forget all the complete query history."
  (interactive)
  (setq
    ;; note: don't forget the present one
    iota4e~headers-query-past nil
    iota4e~headers-query-future nil)
  (iota4e-message "Query history cleared"))

(defun iota4e~headers-move (lines)
  "Move point LINES lines forward (if LINES is positive) or
backward (if LINES is negative). If this succeeds, return the new
docid. Otherwise, return nil."
  (unless (eq major-mode 'iota4e-headers-mode)
    (iota4e-error "Must be in iota4e-headers-mode (%S)" major-mode))
  (let ((succeeded (zerop (forward-line lines)))
	 (docid (iota4e~headers-docid-at-point)))
    ;; move point, even if this function is called when this window is not
    ;; visible
    (when docid
      ;; update all windows showing the headers buffer
      (walk-windows
	(lambda (win)
	  (when (eq (window-buffer win) iota4e~headers-buffer)
	    (set-window-point win (point))))
	nil t)
      ;; attempt to highlight the new line, display the paper
      (iota4e~headers-highlight docid)
      ;; update paper view if it was already showing
      (when (window-live-p iota4e~headers-view-win)
	(iota4e-headers-view-paper))
      docid)))

(defun iota4e-headers-next (&optional n)
  "Move point to the next paper header.
If this succeeds, return the new docid. Otherwise, return nil.
Optionally, takes an integer N (prefix argument), to the Nth next
header."
  (interactive "P")
  (iota4e~headers-move (or n 1)))

(defun iota4e-headers-prev (&optional n)
  "Move point to the previous paper header.
If this succeeds, return the new docid. Otherwise, return nil.
Optionally, takes an integer N (prefix argument), to the Nth
previous header."
  (interactive "P")
  (iota4e~headers-move (- (or n 1))))

(defun iota4e-headers-split-view-grow (n)
  "In split-view, grow the headers window.
In horizontal split-view, increase the number of lines shown by N.
In vertical split-view, increase the number of columns shown by N.
If N is negative shrink the headers window.  When not in split-view
do nothing."
  (interactive "P")
  (let ((n (or n 1))
	 (hwin (get-buffer-window iota4e~headers-buffer)))
  (when (and (buffer-live-p iota4e~view-buffer) (window-live-p hwin))
     (let ((n (or n 1)))
       (case iota4e-split-view
	 ;; emacs has weird ideas about what horizontal, vertical means...
	 (horizontal
	   (window-resize hwin n nil)
	   (incf iota4e-headers-visible-lines n))
	 (vertical
	   (window-resize hwin n t)
	   (incf iota4e-headers-visible-columns n)))))))

(defun iota4e-headers-split-view-shrink (n)
  "In split-view, shrink the headers window.
In horizontal split-view, decrease the number of lines shown by N.
In vertical split-view, decrease the number of columns shown by N.
If N is negative grow the headers window.
When not in split-view do nothing."
  (interactive "P")
  (iota4e-headers-split-view-grow (- n)))

(defun iota4e-headers-action ()
  "Ask user what to do with paper-at-point, then do it.
The actions are specified in `iota4e-headers-actions'."
  (interactive)
  (let ((paper (iota4e-paper-at-point))
	 (actionfunc (iota4e-read-option "Action: " iota4e-headers-actions)))
    (funcall actionfunc paper)))

(defun iota4e-headers-mark-and-next (mark)
  "Set mark MARK on the paper at point or on all papers in the
region if there is a region, then move to the next paper."
  (interactive)
  (iota4e-mark-set mark)
  (iota4e-headers-next))

(defun iota4e~headers-quit-buffer ()
  "Quit the iota4e-headers buffer.
This is a rather complex function, to ensure we don't disturb
other windows."
  (interactive)
  (unless (eq major-mode 'iota4e-headers-mode)
    (iota4e-error "Iotast be in iota4e-headers-mode (%S)" major-mode))
  (let ((curbuf (current-buffer)) (curwin (selected-window))
	 (headers-visible))
    (walk-windows
      (lambda (win)
	(with-selected-window win
	  ;; if we the view window connected to this one, kill it
	  (when (and (not (one-window-p win)) (eq iota4e~headers-view-win win))
	    (delete-window win)
	    (setq iota4e~headers-view-win nil)))
	;; and kill any _other_ (non-selected) window that shows the current
	;; buffer
	(when (and
		(eq curbuf (window-buffer win)) ;; does win show curbuf?
		(not (eq curwin win))	        ;; it's not the curwin?
		(not (one-window-p)))           ;; and not the last one?
	  (delete-window win))))  ;; delete it!
    ;; now, all *other* windows should be gone. kill ourselves, and return
    ;; to the main view
    (kill-buffer)
    (iota4e~main-view)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'iota4e-headers)
