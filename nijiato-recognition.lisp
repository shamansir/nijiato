(in-package :nijiato-recognition)

;;; ================================================================== Defines >
;;; ----------------------------------------------------------------------------

;; fingers-deltas                              r    g    b
(defvar *fingers-deltas* (list :thumb  (vector 0.09 0.07 0.04)
                               :index  (vector 0.04 0.05 0.05)
                               :middle (vector 0.07 0.06 0.06)
                               :ring   (vector 0.05 0.06 0.06)
                               :little (vector 0.08 0.07 0.07)))

;; fingers-colors:
;; thumb red #77275a 119:39:90 r > b, b > g; (b - g) ~ (r - b)
;; index orange #a36675 163:102:117 r > g, r > b; g ~ b;
;; middle yellow #949c6d 148:156:109 r ~ g; r > b, g > b; (r - b) ~ (g - b) 
;; ring green #1d585a 29:88:90 g > r, g > b; r ~ b; (g - r) ~ (g - b)
;; little blue #15599b 21:89:155 b > g > r; (b - g) ~ (g - b)
;;                                             r    g    b
(defvar *fingers-colors* (list :thumb  (vector 0.47 0.15 0.35)
                               :index  (vector 0.64 0.40 0.46)
                               :middle (vector 0.58 0.61 0.43)
                               :ring   (vector 0.11 0.34 0.35)
                               :little (vector 0.08 0.35 0.61)))

;; shifts
(defvar *fingers-shifts* (list :thumb  0
                               :index  10
                               :middle 20
                               :ring   30
                               :little 40))

;; right-hand positions                                         x   y   z 
(defparameter *rh-positions* (list :thumb  (list :start (vector nil nil nil) 
                                                 :end   (vector nil nil nil))
                                   :index  (list :start (vector nil nil nil) 
                                                 :end   (vector nil nil nil))
                                   :middle (list :start (vector nil nil nil) 
                                                 :end   (vector nil nil nil))
                                   :ring   (list :start (vector nil nil nil) 
                                                 :end   (vector nil nil nil))
                                   :little (list :start (vector nil nil nil) 
                                                 :end   (vector nil nil nil))))

;; left-hand positions                                          x   y   z 
(defparameter *lh-positions* (list :thumb  (list :start (vector nil nil nil) 
                                                 :end   (vector nil nil nil))
                                   :index  (list :start (vector nil nil nil) 
                                                 :end   (vector nil nil nil))
                                   :middle (list :start (vector nil nil nil) 
                                                 :end   (vector nil nil nil))
                                   :ring   (list :start (vector nil nil nil) 
                                                 :end   (vector nil nil nil))
                                   :little (list :start (vector nil nil nil) 
                                                 :end   (vector nil nil nil))))

(defparameter *hands-positions* (list :right *rh-positions*
                                      :left  *lh-positions*))

;; 2-dimensional array (width * height) filled with values for colors:
;; 00 for none
;; 01-09 for r-thumb
;; 10-19 for r-index
;; 20-29 for r-middle
;; 30-39 for r-ring
;; 40-49 for r-little
;; 50-59 for l-thumb
;; 60-69 for l-index
;; 70-79 for l-middle
;; 80-89 for l-ring
;; 90-99 for l-little
(defparameter *fingers-values* nil)

;;; ========================================================= Inner Parameters >
;;; ----------------------------------------------------------------------------

(defvar *error-tolerance* 4) ; how many pixels in the line can be skipped when
                             ; determining finger angle
                             
(defvar *finger-window-side* 15) ; pixels-length of the line of same color when                           
                                 ; decision that it is a finger is made

;;; ============================================================ Special Macro >
;;; ----------------------------------------------------------------------------

(defmacro .print-log. (&body body)
  `(format *error-output* ,@body))

(defmacro %send-out (&body body)
  `(format *standard-output* ,@body))

;;; ==================================================================== Macro >
;;; ----------------------------------------------------------------------------

;; Usage: (.plist/map. (list :a 2 :b 3 :c 4) (lambda (k v) (format t "~a~%" (list k v)))
(defmacro .plist/map. (plist func)
  (let ((key-name (gensym)) 
        (value-name (gensym))) 
    `(loop for (,key-name ,value-name . nil) on ,plist by #'cddr do 
                          (funcall (,@func) ,key-name ,value-name))))

;; Usage: (.plist/map-inline. (list :a 2 :b 3 :c 4) (lambda (k v l) (format t "~a~%" (list k v l)))
(defmacro .plist/map-inline. (plist func) 
  (let ((key-name (gensym)) 
        (value-name (gensym))) 
    `(loop for (,key-name ,value-name . nil) on ,plist by #'cddr do 
                          (funcall (,@func) ,key-name ,value-name ,plist))))

;;; ================================================================ Functions >
;;; ----------------------------------------------------------------------------

(defun init-fingers-values (win-width win-height)
    (setq *fingers-values* (make-array (* win-width win-height)
                           :element-type '(unsigned-byte 8)
                           :initial-element 0)))

;; returns 00-99 value for (un)detected finger states for concrete pixel to 
;; put it in *colors-values* array

(defun get-finger-value (r g b)
  (.plist/map. *fingers-colors* 
     (lambda (finger-name required-colors)
        (let* ((deltas (getf *fingers-deltas* finger-name)) 
               (rr (elt required-colors 0))
               (gg (elt required-colors 1))
               (bb (elt required-colors 2))
               (dr (elt deltas 0))
	           (dg (elt deltas 1))
	           (db (elt deltas 2)))
	   ; (format t "~a got: ~a required: ~a~% deltas: ~a~%" finger-name (list r g b) required-colors deltas)
	   (if (and 
             (plusp  (+ (- r rr) dr))
             (minusp (- (- r rr) dr))
             (plusp  (+ (- g gg) dg))
             (minusp (- (- g gg) dg))
             (plusp  (+ (- b bb) db))
             (minusp (- (- b bb) db)))
             (return-from get-finger-value (+ 9 (getf *fingers-shifts* finger-name)))))))
  0)

(defun color-from-finger-value (value)
  (cond ((= value 0)   (vector 0   0   0  ))
        ((< value 10)  (vector 255 0   50 )) ; r-thumb  / red
        ((< value 20)  (vector 255 100 100)) ; r-index  / orange
        ((< value 30)  (vector 255 255 100)) ; r-middle / yellow
        ((< value 40)  (vector 0   255 0  )) ; r-ring   / green
        ((< value 50)  (vector 0   0   255)) ; r-little / blue
        ((< value 60)  (vector 255 0   50 )) ; l-thumb  / red
        ((< value 70)  (vector 255 100 100)) ; l-index  / orange
        ((< value 80)  (vector 255 255 100)) ; l-middle / yellow
        ((< value 90)  (vector 0   255 0  )) ; l-ring   / green
        ((< value 100) (vector 0   0   255)) ; l-little / blue
        (t             (vector 0   0   0  ))
  ))
  
(defun coords-for-angle (angle)
  (let ((coords (list)))
     (if (or (and (> angle (/      pi  4)) (< angle (/ (* 3 pi) 4)))  ;  45 < angle < 135
             (and (> angle (/ (* 5 pi) 4)) (< angle (/ (* 7 pi) 4)))) ; 225 < angle < 315
         (loop for y from (* -1 *finger-window-side*) to *finger-window-size* do (setf coords (append coords (list (round (* (cos angle) y)) y))))
         (loop for x from (* -1 *finger-window-side*) to *finger-window-size* do (setf coords (append coords (list x (round (* (sin angle) x)))))))
     coords))
  
(defun detect-fingers-positions (width height)
  (loop for y from 0 to (1- height) do
     (loop for x from 0 to (1- width) do 
        (let* ((cur-pos (+ (* width y) x))
               (val (elt *fingers-values* cur-pos))
            (when (> val 0) 
                ()))))))

(defun visualize-value (pos r g b)
    (setf (aref *fingers-values* pos) (get-finger-value (/ r 255) (/ g 255) (/ b 255)))

    (if (= (elt *fingers-values* pos) 0)
        (let ((result (vector 0 0 0)))
             (setf (elt result 0) r
                   (elt result 1) g
                   (elt result 2) b) result)
		(color-from-finger-value (elt *fingers-values* pos))))
