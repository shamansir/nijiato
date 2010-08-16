(in-package :cl-user)

(defpackage #:nijiato-recognition
    (:nicknames #:nijiato)
    (:use :cl)
    (:export
     
     #:get-finger-value
     #:color-from-finger-value
     #:hand-from-value
     #:finger-from-value
    
     #:*fingers-values*
     #:*fingers-deltas*
     #:*fingers-colors*
     #:*fingers-shifts*
     #:*hands-positions*))
