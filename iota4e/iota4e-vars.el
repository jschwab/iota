(require 'iota4e-meta)

(defgroup iota4e nil
  "iota4e - iota for emacs"
  :group 'mail)

;; headers info
(defconst iota4e-header-info
  '( (:title .
             ( :name "Title"
               :shortname "Title"
               :help "Paper Title"
               :sortable t))
     (:1au .
             ( :name "First Author"
               :shortname "1st Author"
               :help "Paper First Author"
               :sortable t))
     (:journal .
             ( :name "Journal"
               :shortname "Journal"
               :help "Journal Name"
               :sortable t))
     (:year .
             ( :name "Year"
               :shortname "Year"
               :help "Publication Year"
               :sortable t))))

(defcustom iota4e-iota-home nil
  "Location of the iota homedir, or nil for the default."
  :group 'iota4e
  :type '(choice (const :tag "Default location" nil)
                 (directory :tag "Specify location"))
  :safe 'stringp)

(defcustom iota4e-iota-binary (executable-find "iota")
  "Name of the iota-binary to use.
If it cannot be found in your PATH, you can specify the full
path."
  :type 'file
  :group 'iota4e
  :safe 'stringp)

(defcustom iota4e-paperdir (expand-file-name "~/Papers")
  "Your Paperdir directory."
  :type 'directory
  :safe 'stringp
  :group 'iota4e)

(defvar iota4e-debug t
  "When set to non-nil, log debug information to the *iota4e-log* buffer.")

(defvar iota4e~server-props nil
  "Properties we receive from the iota4e server process.
\(in the 'pong-handler').")

;; headers
(defconst iota4e~headers-buffer-name "*iota4e-headers*"
  "Name of the buffer for message headers.")
(defvar iota4e~headers-buffer nil "Buffer for message headers.")
; view
(defconst iota4e~view-buffer-name "*iota4e-view*"
  "Name for the message view buffer.")

(defcustom iota4e-use-fancy-chars nil
  "Whether to use fancy (non-ascii) characters."
  :type 'boolean
  :group 'iota4e)

(defvar iota4e~headers-last-query nil
  "The present (most recent) query.")


(defun iota4e~default-handler (&rest args)
  "*internal* Dummy handler function."
  (error "Not handled: %S" args))

(defvar iota4e-header-func  'iota4e~default-handler
  "A function called for each message returned from the server
process; the function is passed a msg plist as argument. See
`iota4e~proc-filter' for the format.")

(defvar iota4e-found-func  'iota4e~default-handler
  "A function called for when we received a :found sexp after the
headers have returns, to report on the number of matches. See
`iota4e~proc-filter' for the format.")

(defvar iota4e~view-paper nil "The paper being viewed in view mode.")

(defconst iota4e~view-buffer-name "*iota4e-view*"
  "Name for the message view buffer.")

(defconst iota4e~view-embedded-buffer-name " *iota4e-embedded-view*"
  "Name for the embedded message view buffer.")

(defvar iota4e~view-buffer nil "The view buffer.")

(defcustom iota4e-split-view 'horizontal
  "How to show messages / headers.
A symbol which is either:
 * `horizontal':   split horizontally (headers on top)
 * `vertical':     split vertically (headers on the left).
 * anything else:  don't split (show either headers or messages,
                  not both)
Also see `iota4e-headers-visible-lines'
and `iota4e-headers-visible-columns'."
  :type '(choice (const :tag "Split horizontally" horizontal)
                 (const :tag "Split vertically" vertical)
                 (const :tag "Don't split" nil))
  :group 'iota4e-headers)

(defvar iota4e-header-info-custom
  '( (:recipnum .
       ( :name "Number of recipients"
         :shortname "Recip#"
         :help "Number of recipients for this message"
         :function
         (lambda (msg)
           (format "%d"
             (+ (length (iota4e-message-field msg :to))
               (length (iota4e-message-field msg :cc))))))))
"A list of custom (user-defined) headers. The format is similar
to `iota4e-header-info', but addds a :function property, which should
point to a function that takes a message p-list as argument, and
returns a string. See the default value of `iota4e-header-info-custom
for an example.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'iota4e-vars)
;;; End of iota4e-vars.el
