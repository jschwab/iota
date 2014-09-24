(eval-when-compile (byte-compile-disable-warning 'cl-functions))

(require 'iota4e-view)
(require 'iota4e-vars)
(require 'iota4e-utils)

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defsubst iota4e-paper-field-raw (paper field)
  "Retrieve FIELD from paper plist PAPER.
FIELD is one of :from, :to, :cc, :bcc, :subject, :data,
:paper-id, :path, :maildir, :priority, :attachments,
:references, :in-reply-to, :body-txt, :body-html

Returns `nil' if the field does not exist.

A paper plist looks something like:
\(:docid 32461
\))."
  (if paper
    (plist-get paper field)
    (iota4e-error "paper must be non-nil")))

(defsubst iota4e-paper-field (paper field)
  "Retrieve FIELD from paper plist PAPER.
Like `iota4e-paper-field-nil', but will sanitize `nil' values:
- all string field except body-txt/body-html: nil -> \"\"
- numeric fields + dates                    : nil -> 0
- all others                                : return the value
Thus, function will return nil for empty lists, non-existing body-txt or body-html."
  (let ((val (iota4e-paper-field-raw paper field)))
    (cond
      (val
	val)   ;; non-nil -> just return it
      ((member field '(:subject :paper-id :path :maildir :in-reply-to))
	"")    ;; string fields except body-txt, body-html: nil -> ""
      ((member field '(:body-html :body-txt))
	val)
      ((member field '(:docid :size))
	0)     ;; numeric type: nil -> 0
      (t
	val)))) ;; otherwise, just return nil

(defsubst iota4e-paper-has-field (paper field)
  "Return t if PAPER contains FIELD, nil otherwise."
  (plist-member paper field))

(defsubst iota4e-paper-at-point (&optional noerror)
  "Get the paper s-expression for the paper at point in either
the headers buffer or the view buffer, or nil if there is no such
paper. If optional NOERROR is non-nil, do not raise an error when
there is no paper at point."
  (let ((paper (or (get-text-property (point) 'paper) iota4e~view-paper)))
    (if paper
      paper
      (unless noerror (iota4e-warn "No paper at point")))))

(defsubst iota4e-paper-field-at-point (field)
  "Get the field FIELD from the paper at point.
This is equivalent to:
  (iota4e-paper-field (iota4e-paper-at-point) FIELD)."
  (iota4e-paper-field (iota4e-paper-at-point) field))

(defsubst iota4e-paper-part-field  (paperpart field)
  "Get some field in a paper part; a part would look something like:
  (:index 2 :name \"photo.jpg\" :mime-type \"image/jpeg\" :size 147331)."
  (plist-get paperpart field))


(provide 'iota4e-paper)
