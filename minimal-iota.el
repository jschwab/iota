;;; Minimal setup to load latest `iota3e'

;; activate debugging
(setq debug-on-error t
      debug-on-signal nil
      debug-on-quit nil)

;; edebug trace
(setq edebug-trace t)

;; add latest org-mode to load path
(add-to-list 'load-path (expand-file-name "/home/jschwab/Software/iota/iota4e/"))

(require 'iota4e)
