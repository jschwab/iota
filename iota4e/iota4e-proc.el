(require 'iota4e-vars)
(require 'iota4e-utils)
(require 'iota4e-meta)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal vars

(defvar iota4e~proc-buf nil
  "Buffer (string) for data received from the backend.")
(defconst iota4e~proc-name " *iota4e-proc*"
  "Name of the server process, buffer.")
(defvar iota4e~proc-process nil
  "The iota-server process.")

;; dealing with the length cookie that precedes expressions
(defconst iota4e~cookie-pre "\376"
  "Each expression we get from the backend (iota server) starts with
a length cookie:
  <`iota4e~cookie-pre'><length-in-hex><`iota4e~cookie-post'>.")
(defconst iota4e~cookie-post "\377"
    "Each expression we get from the backend (iota server) starts with
a length cookie:
  <`iota4e~cookie-pre'><length-in-hex><`iota4e~cookie-post'>.")
(defconst iota4e~cookie-matcher-rx
  (purecopy (concat iota4e~cookie-pre "\\([[:xdigit:]]+\\)" iota4e~cookie-post))
  "Regular expression matching the length cookie.
Match 1 will be the length (in hex).")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun iota4e~proc-start()
  "Start the iota server process."
  (unless (file-executable-p iota4e-iota-binary)
    (iota4e-error (format "`iota4e-iota-binary' (%S) not found" iota4e-iota-binary)))
  (let* ((process-connection-type nil) ;; use a pipe
	  (args '("server")))
    (setq iota4e~proc-buf "")
    (setq iota4e~proc-process (apply 'start-process
			      iota4e~proc-name iota4e~proc-name
			      iota4e-iota-binary args))
    (unless iota4e~proc-process
      (iota4e-error "Failed to start the iota4e backend"))
    (set-process-query-on-exit-flag iota4e~proc-process nil)
    (set-process-coding-system iota4e~proc-process 'binary 'utf-8-unix)
    (set-process-filter iota4e~proc-process 'iota4e~proc-filter)
    (set-process-sentinel iota4e~proc-process 'iota4e~proc-sentinel)))

(defun iota4e~proc-kill()
  "Kill the iota server process."
  (let* ((buf (get-buffer iota4e~proc-name))
	  (proc (and (buffer-live-p buf) (get-buffer-process buf))))
    (when proc
      (let ((delete-exited-processes t))
	;; the iota server signal handler will make it quit after 'quit'
	(iota4e~proc-send-command "quit"))
	;; try sending SIGINT (C-c) to process, so it can exit gracefully
      (ignore-errors
	(signal-process proc 'SIGINT))))
  (setq
    iota4e~proc-process nil
    iota4e~proc-buf nil))

(defun iota4e~proc-running-p()
  "Whether the iota process is running."
  (when (and iota4e~proc-process
	  (memq (process-status iota4e~proc-process)
	    '(run open listen connect stop)))
    t))

(defsubst iota4e~proc-eat-sexp-from-buf ()
  "'Eat' the next s-expression from `iota4e~proc-buf'.
Note: this is a string, not an emacs-buffer. `iota4e~proc-buf gets
its contents from the iota-servers in the following form:
   <`iota4e~cookie-pre'><length-in-hex><`iota4e~cookie-post'>
Function returns this sexp, or nil if there was none.
`iota4e~proc-buf' is updated as well, with all processed sexp data
removed."
  (ignore-errors ;; the server may die in the middle...
    (let ((b (string-match iota4e~cookie-matcher-rx iota4e~proc-buf))
	   (sexp-len) (objcons))
      (when b
	(setq sexp-len (string-to-number (match-string 1 iota4e~proc-buf) 16))
	;; does iota4e~proc-buf contain the full sexp?
	(when (>= (length iota4e~proc-buf) (+ sexp-len (match-end 0)))
	  ;; clear-up start
	  (setq iota4e~proc-buf (substring iota4e~proc-buf (match-end 0)))
	  ;; note: we read the input in binary mode -- here, we take the part
	  ;; that is the sexp, and convert that to utf-8, before we interpret
	  ;; it.
	  (setq objcons (read-from-string
			  (decode-coding-string
			    (substring iota4e~proc-buf 0 sexp-len)
			    'utf-8 t)))
	  (when objcons
	    (setq iota4e~proc-buf (substring iota4e~proc-buf sexp-len))
	    (car objcons)))))))

(defun iota4e~proc-escape (str)
  "Escape STRING for transport -- put it in quotes, and escape existing quotation.
In particular, backslashes and double-quotes."
  (let ((esc (replace-regexp-in-string "\\\\" "\\\\\\\\" str)))
    (format "\"%s\"" (replace-regexp-in-string "\"" "\\\\\"" esc))))



(defsubst iota4e~proc-filter (proc str)
  "A process-filter for the 'iota server' output.
It accumulates the strings into valid sexps by checking of the
';;eox' end-of-sexp marker, and then evaluating them."

  (iota4e-log 'misc "* Received %d byte(s)" (length str))
  (setq iota4e~proc-buf (concat iota4e~proc-buf str)) ;; update our buffer

  (let ((sexp (iota4e~proc-eat-sexp-from-buf)))
    (with-local-quit
      (while sexp
        (iota4e-log 'from-server "%S" sexp)
        (cond 

         ;; the header sexps
         ((plist-get sexp :header)
            (funcall iota4e-header-func sexp))

          ;; the found sexp, we receive after getting all the headers
          ((plist-get sexp :found)
            (funcall iota4e-found-func (plist-get sexp :found)))

          ;; the view sexp
          ((plist-get sexp :view)
            (funcall iota4e-view-func (plist-get sexp :view)))

         ;; receive an erase message
         ((plist-get sexp :erase)
          (iota4e-log 'from-server "%S" sexp))

         ;; receive a pong message
         ((plist-get sexp :pong)
          (iota4e-log 'from-server "%S" sexp))

         ;; receive an unknown message
         (t (iota4e-message "Unexpected data from server [%S]" sexp)))

        (setq sexp (iota4e~proc-eat-sexp-from-buf))))))


(defun iota4e~proc-sentinel (proc msg)
  "Function that will be called when the iota-server process terminates."
  (let ((status (process-status proc)) (code (process-exit-status proc)))
    (setq iota4e~proc-process nil)
    (setq iota4e~proc-buf ""))) ;; clear any half-received sexps

(defsubst iota4e~proc-send-command (frm &rest args)
  "Send as command to the iota server process.
Start the process if needed."
  (unless (iota4e~proc-running-p)
    (iota4e~proc-start))
  (let ((cmd (apply 'format frm args)))
    (iota4e-log 'to-server "%s" cmd)
    (process-send-string iota4e~proc-process (concat cmd "\n"))))

(defun iota4e~proc-find (query sortfield sortdir maxnum)
  "Start a database query for QUERY."
  (iota4e~proc-send-command 
   "find query:%s sortfield:%s reverse:%s maxnum:%d"
   (iota4e~proc-escape query)
;;   (if (null sortfield) "None" (substring (symbol-name sortfield) 1))
   (iota4e~proc-escape "year")
   (if (eq sortdir 'descending) "True" "False")
   (if maxnum maxnum 10)))

(defun iota4e~proc-ping ()
  "Sends a ping to the iota server, expecting a (:pong ...) in response."
  (iota4e~proc-send-command "ping"))

(defun iota4e~proc-view (docid)
  "Get one particular paper based on its DOCID.
The result will be delivered to the function registered as
`iota4e-paper-func'."
  (iota4e~proc-send-command
    "view docid:%s"
    docid))


(provide 'iota4e-proc)
;; End of iota3-proc.el
