(eval-when-compile (byte-compile-disable-warning 'cl-functions))
(require 'cl)

(require 'iota4e-vars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun iota4e-format (frm &rest args)
  "Create [iota4e]-prefixed string based on format FRM and ARGS."
  (concat
    "[" (propertize "iota4e" 'face 'iota4e-title-face) "] "
    (apply 'format frm args)))

(defun iota4e-message (frm &rest args)
  "Like `message', but prefixed with iota4e.
If we're waiting for user-input, don't show anyhting."
  (unless (active-minibuffer-window)
    (message "%s" (apply 'iota4e-format frm args))
    nil))

(defun iota4e-error (frm &rest args)
  "Create [iota4e]-prefixed error based on format FRM and ARGS.
Does a local-exit and does not return, and raises a
debuggable (backtrace) error."
  (iota4e-log 'error (apply 'iota4e-format frm args))
  (error "%s" (apply 'iota4e-format frm args)))

;; the user-error function is only available in emacs-trunk
(unless (fboundp 'user-error)
  (defalias 'user-error 'error))

(defun iota4e-warn (frm &rest args)
  "Create [iota4e]-prefixed warning based on format FRM and ARGS.
Does a local-exit and does not return. In emacs versions below
24.2, the functions is the same as `iota4e-error'."
  (iota4e-log 'error (apply 'iota4e-format frm args))
  (user-error "%s" (apply 'iota4e-format frm args)))

;; (defun iota4e~read-char-choice (prompt choices)
;;   "Compatiblity wrapper for `read-char-choice'.
;; That function is available which emacs-24 only."
;;   (let ((choice) (ok) (inhibit-quit nil))
;;     (while (not ok)
;;       (message nil);; this seems needed...
;;       (setq choice (read-char-exclusive prompt))
;;       (setq ok (member choice choices)))
;;     choice))

;; (defun iota4e-read-option (prompt options)
;;   "Ask user for an option from a list on the input area.
;; PROMPT describes a iotaltiple-choice question to the user.
;; OPTIONS describe the options, and is a list of cells describing
;; particular options. Cells have the following structure:

;;    (OPTIONSTRING . RESULT)

;; where OPTIONSTRING is a non-empty string describing the
;; option. The first character of OPTIONSTRING is used as the
;; shortcut, and obviously all shortcuts iotast be different, so you
;; can prefix the string with an uniquifying character.

;; The options are provided as a list for the user to choose from;
;; user can then choose by typing CHAR.  Example:
;;   (iota4e-read-option \"Choose an animal: \"
;;               '((\"Monkey\" . monkey) (\"Gnu\" . gnu) (\"xMoose\" . moose)))

;; User now will be presented with a list: \"Choose an animal:
;;    [M]onkey, [G]nu, [x]Moose\".

;; Function will return the cdr of the list element."
;;   (let* ((prompt (iota4e-format "%s" prompt))
;; 	  (chosen)
;; 	  (optionsstr
;; 	    (mapconcat
;; 	      (lambda (option)
;; 		;; try to detect old-style options, and warn
;; 		(when (characterp (car-safe (cdr-safe option)))
;; 		  (iota4e-error (concat "Please use the new format for options/actions; "
;; 				"see the manual")))
;; 		(let* ((kar (substring (car option) 0 1))
;; 			(val (cdr option)))
;; 		  (concat
;; 		    "[" (propertize kar 'face 'iota4e-highlight-face) "]"
;; 		    (substring (car option) 1))))
;; 	      options ", "))
;; 	  (response
;; 	    (iota4e~read-char-choice
;; 	      (concat prompt optionsstr
;; 		" [" (propertize "C-g" 'face 'iota4e-highlight-face) " to cancel]")
;; 	      ;; the allowable chars
;; 	      (map 'list (lambda(elm) (string-to-char (car elm))) options)))
;; 	  (chosen
;; 	    (find-if
;; 	      (lambda (option) (eq response (string-to-char (car option))))
;; 	      options)))
;;     (if chosen
;;       (cdr chosen)
;;       (iota4e-warn "Unknown shortcut '%c'" response))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; logging / debugging
(defvar iota4e~log-max-lines 1200
  "*internal* Last <n> number of lines to keep around in the buffer.")
(defconst iota4e~log-buffer-name "*iota4e-log*"
  "*internal* Name of the logging buffer.")

(defun iota4e-log (type frm &rest args)
  "Write a message of TYPE with format-string FRM and ARGS in
*iota4e-log* buffer, if the variable iota4e-debug is non-nil. Type is
either 'to-server, 'from-server or 'misc. This function is meant for debugging."
  (when iota4e-debug
    (with-current-buffer (get-buffer-create iota4e~log-buffer-name)
      (view-mode)
      (setq buffer-undo-list t)
      (let* ((inhibit-read-only t)
	      (tstamp (propertize (format-time-string "%Y-%m-%d %T" (current-time))
			'face 'font-lock-string-face))
	      (msg-face
		(case type
		  (from-server 'font-lock-type-face)
		  (to-server   'font-lock-function-name-face)
		  (misc        'font-lock-variable-name-face)
		  (error       'font-lock-warning-face)
		  (otherwise   (iota4e-error "Unsupported log type"))))
	      (msg (propertize (apply 'format frm args) 'face msg-face)))
	(goto-char (point-max))
	(insert tstamp
	  (case type
	    (from-server " <- ")
	    (to-server   " -> ")
	    (error       " !! ")
	    (otherwise   " "))
	  msg "\n")

	;; if `iota4e-log-max-lines is specified and exceeded, clearest the oldest
	;; lines
	(when (numberp iota4e~log-max-lines)
	  (let ((lines (count-lines (point-min) (point-max))))
	    (when (> lines iota4e~log-max-lines)
	      (goto-char (point-max))
	      (forward-line (- iota4e~log-max-lines lines))
	      (beginning-of-line)
	      (delete-region (point-min) (point)))))))))

(defun iota4e-toggle-logging ()
  "Toggle between enabling/disabling debug-mode (in debug-mode,
iota4e logs some of its internal workings to a log-buffer. See
`iota4e-visit-log'."
  (interactive)
  (iota4e-log 'misc "logging disabled")
  (setq iota4e-debug (not iota4e-debug))
  (iota4e-message "debug logging has been %s"
    (if iota4e-debug "enabled" "disabled"))
  (iota4e-log 'misc "logging enabled"))

(defun iota4e-show-log ()
  "Visit the iota4e debug log."
  (interactive)
  (let ((buf (get-buffer iota4e~log-buffer-name)))
    (unless (buffer-live-p buf)
      (iota4e-warn "No debug log available"))
    (switch-to-buffer buf)))


(defun iota4e-hide-other-iota4e-buffers ()
  "Bury iota4e-buffers (main, headers, view) (and delete all windows
displaying it). Do _not_ bury the current buffer, though."
  (interactive)
  (let ((curbuf (current-buffer)))
    ;; note: 'walk-windows' does not seem to work correctly when modifying
    ;; windows; therefore, the doloops here
    (dolist (frame (frame-list))
      (dolist (win (window-list frame nil))
        (with-current-buffer (window-buffer win)
          (unless (eq curbuf (current-buffer))
            (when (member major-mode '(iota4e-headers-mode iota4e-view-mode))
              (when (eq t (window-deletable-p win))
                (delete-window win))))))) t))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start and stopping
(defun iota4e~check-requirements ()
  "Check for the settings required for running iota4e."
  (unless (>= emacs-major-version 24)
    (iota4e-error "Emacs >= 24.x is required for iota4e")))

(defun iota4e-running-p ()
  "Whether iota4e is running.
Checks whether the server process is live."
  (iota4e~proc-running-p))

(defun iota4e~start (&optional func)
  "If iota4e is already running, execute function FUNC (if non-nil).
Otherwise, check various requirements, then start iota4e. When
successful, call FUNC (if non-nil) afterwards."
  ;; if we're already running, simply go to the main view
  (when (not (iota4e-running-p))   ;; already running?
    (iota4e~proc-start)
    (progn 
      (lexical-let ((func func))
        (iota4e~check-requirements)
        (setq iota4e-pong-func
              (lambda (props)
                (setq iota4e~server-props props) ;; save the props we got from the server
                (let ((version (plist-get props :version)))
                  (iota4e-message "Started iota %s" version)))))
        (iota4e~proc-ping))
      (when func
        (funcall func))))

(defun iota4e~stop ()
  "Stop the iota4e session."
  (iota4e~proc-kill)
  ;; kill all main/view/headers buffer
  (mapcar
    (lambda (buf)
      (with-current-buffer buf
	(when (member major-mode
		'(iota4e-headers-mode iota4e-view-mode iota4e-main-mode))
	  (kill-buffer))))
    (buffer-list)))

(provide 'iota4e-utils)
;;; End of iota4e-utils.el
