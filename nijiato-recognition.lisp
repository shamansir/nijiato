(in-package :nijiato-recognition)

;;; ================================================================== Defines >
;;; ----------------------------------------------------------------------------

;; fingers-deltas                                    r    g    b
(defparameter *fingers-deltas* (list :thumb  (vector 0.09 0.07 0.04)
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
;;                                                   r    g    b
(defparameter *fingers-colors* (list :thumb  (vector 0.47 0.15 0.35)
                                     :index  (vector 0.64 0.40 0.46)
                                     :middle (vector 0.58 0.61 0.43)
                                     :ring   (vector 0.11 0.34 0.35)
                                     :little (vector 0.08 0.35 0.61)))

;; shifts
(defparameter *fingers-shifts* (list :thumb  0
                                     :index  10
                                     :middle 20
                                     :ring   30
                                     :little 40))
                               
(defparameter *hands-shifts* (list :left   0
                                   :right  50))

;; right-hand positions
(defparameter *rh-positions* (list :thumb  (list :start (list :px nil :py nil :pz nil) 
                                                 :end   (list :px nil :py nil :pz nil)
                                                 :angle nil)
                                   :index  (list :start (list :px nil :py nil :pz nil) 
                                                 :end   (list :px nil :py nil :pz nil)
                                                 :angle nil)
                                   :middle (list :start (list :px nil :py nil :pz nil) 
                                                 :end   (list :px nil :py nil :pz nil)
                                                 :angle nil)
                                   :ring   (list :start (list :px nil :py nil :pz nil) 
                                                 :end   (list :px nil :py nil :pz nil)
                                                 :angle nil)
                                   :little (list :start (list :px nil :py nil :pz nil) 
                                                 :end   (list :px nil :py nil :pz nil)
                                                 :angle nil)))

;; left-hand positions
(defparameter *lh-positions* (list :thumb  (list :start (list :px nil :py nil :pz nil) 
                                                 :end   (list :px nil :py nil :pz nil)
                                                 :angle nil)
                                   :index  (list :start (list :px nil :py nil :pz nil) 
                                                 :end   (list :px nil :py nil :pz nil)
                                                 :angle nil)
                                   :middle (list :start (list :px nil :py nil :pz nil) 
                                                 :end   (list :px nil :py nil :pz nil)
                                                 :angle nil)
                                   :ring   (list :start (list :px nil :py nil :pz nil) 
                                                 :end   (list :px nil :py nil :pz nil)
                                                 :angle nil)
                                   :little (list :start (list :px nil :py nil :pz nil) 
                                                 :end   (list :px nil :py nil :pz nil)
                                                 :angle nil)))

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

(defvar *finger-box-side-half* 15) ; half of the pixels-length of the line of 
                                   ; same color when decision that it is a finger is made
                                   ; so when 31 pixels is the length than required,
                                   ; set this var to 15, so the box side will be from
                                   ; -15 to 15, including 0 and both ends
                                 
(defvar *box-angle-step* (/ pi 20)) ; step between angles in a box where searching
                                    ; for such lines is made

(defvar *error-tolerance* 4) ; how many pixels in the line can be skipped when
                             ; determining finger angle
                             
(defvar *hands-allowed-distance* 80) ; the distance in pixels between same-colored
                                     ; fingers when they are determined as different
                             
(defvar *hits-pass* (- (1+ (* 2 *finger-box-side-half*)) *error-tolerance*))

;;; ============================================================ Special Macro >
;;; ----------------------------------------------------------------------------

(defmacro .print-log. (&body body)
  `(format *error-output* ,@body))

(defmacro %send-out (&body body)
  `(format *standard-output* ,@body))

;;; ==================================================================== Macro >
;;; ----------------------------------------------------------------------------

;; Scrolls through plist, passing each key and value to the specified function
;;
;; Usage: (.plist/map. (list :a 2 :b 3 :c 4) (lambda (k v) (format t "~a~%" (list k v)))
(defmacro .plist/map. (plist func)
  (let ((key-name (gensym)) 
        (value-name (gensym))) 
    `(loop for (,key-name ,value-name . nil) on ,plist by #'cddr do 
                          (funcall (,@func) ,key-name ,value-name))))

;; Scrolls through plist, passing key, value and current list state to the specified function
;;
;; Usage: (.plist/map-inline. (list :a 2 :b 3 :c 4) (lambda (k v l) (format t "~a~%" (list k v l)))
(defmacro .plist/map-inline. (plist func) 
  (let ((key-name (gensym)) 
        (value-name (gensym))) 
    `(loop for (,key-name ,value-name . nil) on ,plist by #'cddr do 
                          (funcall (,@func) ,key-name ,value-name ,plist))))
                          
;; Sets last not-true item of somelist to true (modifies passed list, returns list if changed, nil if not)
;; Do not pass lists, created in the place: (mark-last (list t t nil nil)) is wrong, pass only variables
;;
;; (defvar *somelist* (list t nil nil nil))
;; (mark-last *somelist*) => (list t t nil nil)
;; (mark-last *somelist*) => (list t t t nil)
;; (mark-last *somelist*) => (list t t t t)
;; (mark-last *somelist*) => nil ; list stays in the same state
(defmacro mark-last (somelist)
   (let ((index (gensym)))
     `(dotimes (,index (length ,somelist)) (when (not (elt ,somelist ,index)) (setf (elt ,somelist ,index) t) (return ,somelist)))))
     
;; Gets list of two items and checks if both elements are true
;;
;; (bothp (list t t)) => t
;; (bothp (list nil t)) => nil
;; (bothp (list t nil)) => nil
;; (bothp (list nil nil)) => nil
;; (bothp (list t t t)) => t
;; (bothp (list t t nil)) => t
(defmacro bothp (somelist)
    `(and (elt ,somelist 0) (elt ,somelist 1)))

;;; ================================================================ Functions >
;;; ----------------------------------------------------------------------------

(defun init-fingers-values (width height)
    (setq *fingers-values* (make-array (* width height)
                           :element-type '(unsigned-byte 8)
                           :initial-element 0)))

;; returns 00-99 value for (un)detected finger states for concrete pixel to 
;; put it in *fingers-values* array

(defun get-finger-value (hand r g b)
  (.plist/map. *fingers-colors* 
     (lambda (finger required-colors)
        (let* ((deltas (getf *fingers-deltas* finger)) 
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
             (return-from get-finger-value (+ (+ 9 (getf *fingers-shifts* finger)) 
                                              (getf *hands-shifts* hand)))))))
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
  
(defun hand-from-value (value)
  (cond ((= value 0)   nil)
        ((< value 50)  :left )
        ((< value 100) :right)))
  
(defun finger-from-value (value)
  (cond ((= value 0)   nil)
        ((< value 10)  :thumb ) ; r-thumb  / red
        ((< value 20)  :index ) ; r-index  / orange
        ((< value 30)  :middle) ; r-middle / yellow
        ((< value 40)  :ring  ) ; r-ring   / green
        ((< value 50)  :little) ; r-little / blue
        ((< value 60)  :thumb ) ; l-thumb  / red
        ((< value 70)  :index ) ; l-index  / orange
        ((< value 80)  :middle) ; l-middle / yellow
        ((< value 90)  :ring  ) ; l-ring   / green
        ((< value 100) :little) ; l-little / blue
        (t             nil)
  ))
  
(defun clear-hands-positions ()
   (.plist/map. (getf *hands-positions* :right)
       (lambda (finger positions)
         (declare (ignore finger))
         (setf positions (list :start (list :px nil :py nil :pz nil)
                               :end   (list :px nil :py nil :pz nil)
                               :angle nil)))))
  
;; calculates (possible) finger from RGB components using pre-calibrated colors
;; values and deltas, and stores the corresponding value in *fingers-values* array
  
(defun store-finger-value (pos r g b)
   (setf (elt *fingers-values* pos) (get-finger-value :left (/ r 255) (/ g 255) (/ b 255))))
   
;; takes an angle in radians (practically between 0 and pi) and translates it 
;; into the array of (x y) coordinates pairs of pixels that 'draw' the line
;; representation of this angle in the square box with side equal to 
;; ((*finger-box-side* * 2) + 1). coordinates values may vary from (-1 * *finger-box-side*) 
;; to *finger-box-side* and the center is (0 0)
  
(defun coords-for-angle (angle)
  (let ((coords (make-array (1+ (* *finger-box-side-half* 2)) :fill-pointer 0)))
     (if (or (and (> angle (/      pi  4)) (< angle (/ (* 3 pi) 4)))  ;  45 < angle < 135
             (and (> angle (/ (* 5 pi) 4)) (< angle (/ (* 7 pi) 4)))) ; 225 < angle < 315
         (loop for y from (* -1 *finger-box-side-half*) to *finger-box-side-half* do 
                       (vector-push (list (round (* (cos angle) y)) y) coords))
         (loop for x from (* -1 *finger-box-side-half*) to *finger-box-side-half* do 
                       (vector-push (list x (round (* (sin angle) x))) coords)))
     coords))
     
;; returns the possible hand for this coords. silently corrects the previously saved values
;; if new condition does not fits them

(defun predict-current-hand (x y finger hits)
   (declare (ignore y hits))
   (let ((left-hand-point-slot (getf (getf *hands-positions* :left) finger)))
        (if (or (eq (getf (getf left-hand-point-slot :start) :px) nil)
                (eq (getf (getf left-hand-point-slot :start) :py) nil)) 
            :left
            (let ((left-start-x (getf (getf left-hand-point-slot :start) :px)))
                 (if (> x left-start-x)
                     (if (> (- x left-start-x) *hands-allowed-distance*) :right :left)
                     :left)))))

#|
(defun predict-current-hand (x y finger hits)
   (declare (ignore y))
   :left)
|#
     
;; replaces concrete elements in *finges-values* array with values more 
;; than 100 to show where fingers lines may be located. in fact, it checks the previous
;; value to be greater than zero, and if it is so, tries to find a line of same values
;; inside the box with side of *finger-box-side* around this point (see 'coords-for-angle')
;; using angles between 0 and pi with step *box-angle-step* and allowed error of missing
;; *error-tolerance* pixels
  
(defun detect-fingers-positions (frame-num width height)
; (declare (ignore frame-num))
  (clear-hands-positions)
  (let ((hits (make-array 10 :initial-element nil)))
  (%send-out "frame ~d~%" frame-num)  
  (loop for y from 0 to (1- height) do
     (loop for x from 0 to (1- width) do 
        (let* ((cur-pos (+ (* width y) x))
               (val (elt *fingers-values* cur-pos)))
            (when (and (> val 0) (< val 100))
              (let* ((finger (finger-from-value val))
                     (hand (predict-current-hand x y finger hits))
                     (hand-shift (getf *hands-shifts* hand))
                     (val (+ val hand-shift)))
              (when (not (elt hits (floor val 10)))
                  (do ((angle 0 (+ angle *box-angle-step*))) ((> angle pi))
                      (let ((coords (coords-for-angle angle))
                            (hit-num 0))
                           (loop for point across coords do 
                              (let* ((in-x (+ (car point) x))
                                     (in-y (+ (car (cdr point)) y))
                                     (in-pos (+ (* width in-y) in-x)))
                                    (when (and (>= in-pos 0) (< in-pos (length *fingers-values*)))
                                          (let ((in-val (+ (elt *fingers-values* in-pos) hand-shift)))
                                               (when (and (> in-val 0)
                                                          (< in-val 100))
                                                     (if (= in-val val)
                                                         (incf hit-num)))))))
                           (when (>= hit-num *hits-pass*)
                              (setf (elt hits (floor val 10)) t)
                              (loop for point across coords do 
                                 (let* ((in-x (+ (car point) x))
                                        (in-y (+ (car (cdr point)) y))
                                        (in-pos (+ (* width in-y) in-x)))
                                     (when (and (>= in-pos 0) (< in-pos (length *fingers-values*)))
                                           (setf (elt *fingers-values* in-pos) (+ val 100)))))
                              (let ((point-slot (getf (getf *hands-positions* hand) finger))
                                    (start-x (+ (car (elt coords 0)) x))
                                    (start-y (+ (car (cdr (elt coords 0))) y))
                                    (end-x (+ (car (elt coords (1- (length coords)))) x))
                                    (end-y (+ (car (cdr (elt coords (1- (length coords))))) y)))
                                   (setf (getf point-slot :start) (list :px start-x :py start-y :pz nil))
                                   (setf (getf point-slot :end)   (list :px end-x   :py end-y   :pz nil))
                                   (setf (getf point-slot :angle) angle)
                                   (%send-out "~a - ~a : (~d ~d) (~d ~d) ~d~%" 
                                              hand finger start-x start-y end-x end-y (* angle (/ 180 pi)))))))))))))))
                
;; calculates new RGB components to show in UI from the *fingers-values*, if
;; finger (possibly) exists in this position

(defun visualize-value (pos)
    (when (not (= (elt *fingers-values* pos) 0))
          (color-from-finger-value (elt *fingers-values* pos))))
