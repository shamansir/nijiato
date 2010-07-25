(defpackage #:nijiato-recognition
    (:nicknames #:nijiato)
    (:use :cl)
    (:export
     
     #:get-finger-value
     #:color-from-finger-value
    
     #:*fingers-values*    
     #:*fingers-deltas*
     #:*fingers-colors*
     #:*fingers-shifts*
     #:*hands-positions*))
