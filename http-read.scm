;;; http-read.scm
(module http-read
    (http-read response-body response-header response-status)
  (import scheme chicken srfi-1 srfi-13 srfi-14 irregex extras ports data-structures)
  (include "main.scm")
  )
