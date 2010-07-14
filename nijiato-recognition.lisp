;; $ LD_PRELOAD=/usr/lib64/libv4l/v4l2convert.so [rlwrap] sbcl --load nijiato-recognition.lisp
;; $ LD_PRELOAD=/usr/lib/libv4l/v4l2convert.so [rlwrap] sbcl --load nijiato-recognition.lisp

;; This file is based on the example source code from CL-Video4Linux2 
;; distribution. CL-Video4Linux2 is distibuted under GNU General Public License 
;; (see <http://www.gnu.org/licenses/>), written by <v.mayatskih@gmail.com> and 
;; hosted at <http://www.cliki.net/CL-V4L2>

;; The recoginition/demonstration part is written by <shaman.sir@gmail.com>

(asdf:oos 'asdf:load-op :cl-v4l2)
(asdf:oos 'asdf:load-op :cl-gtk2-gtkglext)
(asdf:oos 'asdf:load-op :bordeaux-threads)

(defpackage :nijiato
  (:use :common-lisp :gtk :gtkglext))

(in-package :nijiato)

;; ===============
;; Camera settings
;; ===============

(defvar *capture-device* "/dev/video0")
(defparameter *capture-fd* nil)

;; what we want from camera
(defvar *want-width* 352)
(defvar *want-height* 288)
; (defvar *want-width* 640)
; (defvar *want-height* 480)

;; what we really get
(defparameter *got-width* nil)
(defparameter *got-height* nil)

;; ==================
;; GTK Window Helpers
;; ==================

(defparameter *camera-widget* nil)
(defparameter *camera-data* nil)
(defparameter *camera-data-lock* (bt:make-lock "Camera data lock"))

(defparameter *cap-thread-stop* nil)

(defparameter *render-thread-stop* (bt:make-condition-variable))
(defparameter *render-thread-lock* (bt:make-lock "Render thread lock"))

;; ==============
;; Nijiato things
;; ==============

;; fingers-deltas                              r   g   b
#|
(defvar *fingers-deltas* (list :thumb  (vector 0.02 0.01 0.01)
                               :index  (vector 0.02 0.01 0.01)
                               :middle (vector 0.02 0.02 0.01)
                               :ring   (vector 0.01 0.02 0.01)
                               :little (vector 0.01 0.01 0.02)))
|#
(defvar *fingers-deltas* (list :thumb  (vector 0.2 0.1 0.1)
                               :index  (vector 0.2 0.1 0.1)
                               :middle (vector 0.2 0.2 0.1)
                               :ring   (vector 0.1 0.2 0.1)
                               :little (vector 0.1 0.1 0.2)))

;; fingers-colors:
;; thumb red #f95f98 249:95:152 r > b, b > g; (b - g) ~ (r - b)
;; index orange #fa9eaa 250:158:170 r > g, r > b; g ~ b;
;; middle yellow #faffc9 250:255:201 r ~ g; r > b, g > b; (r - b) ~ (g - b) 
;; ring green #5b8b7d 91:139:125 g > r, g > b; r ~ b; (g - r) ~ (g - b)
;; little blue #5793c5 87:147:197 b > g > r; (b - g) ~ (g - b)
;;                                             r    g    b
(defvar *fingers-colors* (list :thumb  (vector 0.98 0.37 0.60)
                               :index  (vector 0.98 0.62 0.66)
                               :middle (vector 0.98 1.00 0.79)
                               :ring   (vector 0.36 0.55 0.49)
                               :little (vector 0.34 0.58 0.77)))

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

;; ====== Nijiato macro

(defmacro .print-log. (&body body)
  `(format *error-output* ,@body))

(defmacro %send-out (&body body)
  `(format *standard-output* ,@body))

;; Usage: (.plist/map. (list :a 2 :b 3 :c 4) (lambda (k v) (format t "~a~%" (list k v)))
(defmacro .plist/map. (plist func)
  (let ((key-name (gensym)) 
        (value-name (gensym))) 
    `(loop for (,key-name ,value-name . nil) on ,plist by #'cddr do 
                          (funcall (,@func) ,key-name ,value-name))))

;; Usage: (.plist/map. (list :a 2 :b 3 :c 4) (lambda (k v l) (format t "~a~%" (list k v l)))
(defmacro .plist/map-inline. (plist func) 
  (let ((key-name (gensym)) 
        (value-name (gensym))) 
    `(loop for (,key-name ,value-name . nil) on ,plist by #'cddr do 
                          (funcall (,@func) ,key-name ,value-name ,plist))))

;; ====== Nijiato funcs

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

(defun _cffv (value)
  (cond ((= value 0)   (return-from _cffv (vector 0 0 0)))
        ((< value 10)  (return-from _cffv (vector 255 0   50 ))) ; r-thumb  / red
        ((< value 20)  (return-from _cffv (vector 255 100 100))) ; r-index  / orange
        ((< value 30)  (return-from _cffv (vector 255 255 100))) ; r-middle / yellow
        ((< value 40)  (return-from _cffv (vector 0   255 0  ))) ; r-ring   / green
        ((< value 50)  (return-from _cffv (vector 0   0   255))) ; r-little / blue
        ((< value 60)  (return-from _cffv (vector 255 0   50 ))) ; l-thumb  / red
        ((< value 70)  (return-from _cffv (vector 255 100 100))) ; l-index  / orange
        ((< value 80)  (return-from _cffv (vector 255 255 100))) ; l-middle / yellow
        ((< value 90)  (return-from _cffv (vector 0   255 0  ))) ; l-ring   / green
        ((< value 100) (return-from _cffv (vector 0   0   255))) ; l-little / blue
  ) (vector 0 0 0))

(defun color-from-finger-value (value) (_cffv value))

;; ====== / Nijiato funcs

;; ============
;; Program code
;; ============

(defmacro without-errors (&body body)
  `(handler-case (progn ,@body)
     (error (c) (.print-log. "suppressed error: ~A~%" c) nil)))

(defun char-at (pos data)
  (code-char (ldb (byte 8 (* 8 pos)) data)))

(defun format-string (pixfmt)
  (format nil "~C~C~C~C"
	  (char-at 0 pixfmt)
	  (char-at 1 pixfmt)
	  (char-at 2 pixfmt)
	  (char-at 3 pixfmt)))

;; => diagnose

(defun diagnose (fd)
  (.print-log. "diagnose: started diagnostics~%")
  (let ((caps (v4l2:query-capabilities fd)))
    (format t (v4l2:%device-info caps))
    (unless (v4l2:capable caps v4l2:cap-video-capture)
      (error "not a capture device"))
    (unless (v4l2:capable caps v4l2:cap-streaming)
      (error "not a streaming device"))
    (when (v4l2:capable caps v4l2:cap-tuner)
      (without-errors
	  (loop for idx from 0 do
	       (progn
		      (v4l2:get-tuner-params fd idx)
		      ;; show tuner params
		   ))))
    (.print-log. "diagnose: capabilities test passed~%")

    (without-errors    
    (.print-log. "diagnose: started slots scanning~%")    
	(loop for idx from 0 do
	   (with-slots (v4l2:index v4l2:name v4l2:type v4l2:tuner)
                       (v4l2:get-input-params fd idx)
	       (.print-log. "diagnose: input [~D] name: ~A, type ~A~%"
                                                            v4l2:index
                                                            v4l2:name
                  (if (= v4l2:type v4l2:input-type-tuner) "tuner" "camera"))
               (when (= v4l2:type v4l2:input-type-tuner)
                  (.print-log. "diagnose: input [~D] connected to tuner ~D~%" v4l2:index v4l2:tuner))
               (without-errors
                   (.print-log. "diagnose: checking slots~%")
		   (loop for idx1 from 0 do
			  (with-slots (v4l2:index v4l2:name)
			    (v4l2:get-input-standard fd idx1)
			    (.print-log. "diagnose: input [~D] std [~D] name: ~A~%"
				                                     idx v4l2:index v4l2:name)))
	   (.print-log. "diagnose: slots scanning complete~%")))))

    (.print-log. "diagnose: setting input manually~%")
    (v4l2:set-input fd 0)	       ; some cameras don't set input by default

    (without-errors
    (.print-log. "diagnose: checking formats~%")    
	(loop for idx from 0 do
	     (with-slots (v4l2:index v4l2:pixelformat) (v4l2:get-format fd idx)
	        (.print-log. "diagnose: format [~D] ~S~%" v4l2:index
		                           (format-string v4l2:pixelformat))))
	(.print-log. "diagnose: formats check complete~%"))))

;; => device-init

(defun device-init (fd)
  (.print-log. "device-init: initializing devide using v4l2~%")
  (v4l2:set-input fd 0)
  (.print-log. "device-init: input assigned~%")
  (without-errors
      (v4l2:set-control fd v4l2:cid-exposure 0.05))
  (.print-log. "device-init: set ~Dx~D, format ~S~%" *want-width* *want-height*
	                                (format-string v4l2:pix-fmt-rgb24))
  (v4l2:set-image-format fd *want-width* *want-height* v4l2:pix-fmt-rgb24)
  (.print-log. "device-init: parameters are set~%")  
  (with-slots (v4l2:width v4l2:height v4l2:sizeimage v4l2:pixelformat)
      (v4l2:format-pix (v4l2:get-image-format fd))
      (setf *got-width* v4l2:width
	        *got-height* v4l2:height)
      (.print-log. "device-init: got ~Dx~D size ~D, format ~S~%"
	                           v4l2:width v4l2:height
	                           v4l2:sizeimage (format-string v4l2:pixelformat))
      (setq *camera-data* (make-array (* 4 *got-height* *got-width*)
				           :element-type '(unsigned-byte 8)
  				           :initial-element #xff)))
  (.print-log. "device-init: initialization finished~%"))

;; => video-init

(defun video-init (device)
  (.print-log. "video-init: initializing video~%")
  (let ((fd (isys:open device isys:o-rdwr)))
    (setq *capture-fd* fd)
    (diagnose fd) ; info about device
    (device-init fd) ; setup
    (let ((buffers (v4l2:map-buffers fd 4))) ; map 4 buffers into memory    
      (v4l2:stream-on fd buffers) ; start capturing
      (.print-log. "video-init: starting to capture~%")
      (values fd buffers))))

;; => video-uninit

(defun video-uninit (fd buffers)
  (.print-log. "video-unit: unititializing video~%")
  (v4l2:stream-off fd) ; stop capturing
  (v4l2:unmap-buffers buffers) ; throw away buffers from memory
  (isys:close fd) ; close device
  (.print-log. "video-unit: that's all!~%"))

;; => capture-thread

(defun capture-thread ()
  (.print-log. "capture-thread: capturing thread start~%")
  (multiple-value-bind (fd buffers)
    (video-init *capture-device*)
    (setq *fingers-values* (make-array (* *got-width* *got-height*)
                           :element-type '(unsigned-byte 8)
                           :initial-element 0))
    (setf frame-num 0)
    (.print-log. "capture-thread: returned after init~%")
    (loop thereis *cap-thread-stop* do
      ; get one frame from driver
	  (let ((frame (without-errors (v4l2:get-frame fd))))
	   (when frame	        ; errors from v4l2convert.so are highly possible
	     (multiple-value-bind (buffer address)
		 (values-list (nth frame buffers))
	       (declare (ignore buffer))
	       (setf frame-num (1+ frame-num))
	       ;; (%send-out "~d~%" frame-num)
	       ;; Silly rgb24->rgb32 converter
	       (bt:with-lock-held (*camera-data-lock*)
		 (declare (optimize (speed 3) (debug 0) (safety 0)))
                 (loop for i fixnum from 0 below (* *got-width* *got-height*) do
                      (let ((r (cffi:mem-aref address :uchar (+ (* 3 i) 0)))
                            (g (cffi:mem-aref address :uchar (+ (* 3 i) 1)))
                            (b (cffi:mem-aref address :uchar (+ (* 3 i) 2))))

			(setf (aref *fingers-values* i) (get-finger-value (/ r 255) (/ g 255) (/ b 255)))

			(if (= (elt *fingers-values* i) 0)
                            (setf (aref *camera-data* (+ (* 4 i) 0)) r
                                  (aref *camera-data* (+ (* 4 i) 1)) g
                                  (aref *camera-data* (+ (* 4 i) 2)) b)
			    (let ((new-color (color-from-finger-value (elt *fingers-values* i))))
			         (setf (aref *camera-data* (+ (* 4 i) 0)) (elt new-color 0)
				       (aref *camera-data* (+ (* 4 i) 1)) (elt new-color 1)
				       (aref *camera-data* (+ (* 4 i) 2)) (elt new-color 2))))))))

	     (when *camera-widget*
	       (with-main-loop
		   (widget-queue-draw *camera-widget*)))

	     (v4l2:put-frame fd frame))))	; put frame back to driver
    (video-uninit fd buffers))
  (.print-log. "capture-thread: capturing thread exit~%"))

;; => camera-init

(defun camera-init (widget)
  (declare (ignore widget))
  (.print-log. "camera-init: camera initialization, binding opengl texture~%")
  (gl:clear-color 0.8 0.8 0.8 0.8)
  (gl:enable :texture-rectangle-arb :depth-test)
  (gl:depth-func :lequal)

  (gl:bind-texture :texture-rectangle-arb 0)

  (.print-log. "camera-init: got-width: ~D; got-height: ~D~%" 
                                                       *got-width* *got-height*)
  (gl:tex-image-2d :texture-rectangle-arb
		   0
		   :rgb8
		   *got-width*
		   *got-height*
		   0
		   :rgba
		   :unsigned-byte
		   *camera-data*)

  (.print-log. "camera-init: after-binding~%")
  (gl:new-list 1 :compile)

  (.print-log. "camera-init: making quads~%")
  (gl:begin :quads)
  (gl:tex-coord 0 *got-height*)
  (gl:vertex 0.0 0.0)
  (gl:tex-coord 0 0)
  (gl:vertex 0.0 1.0)
  (gl:tex-coord *got-width* 0)
  (gl:vertex 1.0 1.0)
  (gl:tex-coord *got-width* *got-height*)
  (gl:vertex 1.0 0.0)
  (gl:end)
  (gl:end-list)
  (.print-log. "camera-init: quads made ~%")  

  (gl:clear-depth 1.0)
  (gl:flush)
  (.print-log. "camera-init: flush~%")  
  
  (.print-log. "camera-init: texture binding succesfull~%"))

;; => camera-draw

(defun camera-draw (widget event)
  (declare (ignore widget event))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:bind-texture :texture-rectangle-arb 0)

  (when *camera-data*
    (bt:with-lock-held (*camera-data-lock*)
      (gl:tex-sub-image-2d :texture-rectangle-arb 0
			   0 0
			   *got-width*
			   *got-height*
			   :rgba
			   :unsigned-byte
			   *camera-data*)))

  ;; Keep ratio 4:3
  (multiple-value-bind (w h)
      (gdk:drawable-get-size (widget-window widget))
      (let ((w1 w)
  	        (h1 h))
      (when (and (> w 0) (> h 0))
	        (if (> (/ w h) 4/3)
	            (setq h1 h
		              w1 (* h 4/3))
	            (setq w1 w
		              h1 (* w 3/4))))
      (gl:viewport 0 0 w1 h1)))

  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 19.0 1.0 1.0 10.0)

  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (glu:look-at 0.0 0.0 3.0
	       0.0 0.0 0.0
	       0.0 1.0 0.0)

  (gl:translate -0.5 -0.5 0.0)

  (gl:call-list 1)
  (gl:flush))

;; => run-nijiato

(defun _make-color-spin (name)
    (make-instance 'spin-button :label name
                                :adjustment (make-instance 'adjustment
                                                           :lower 0d0
                                                           :upper 255d0
                                                           :step-increment 1d0)))
(defun _make-delta-spin (name)
    (make-instance 'spin-button :label name
                                :adjustment (make-instance 'adjustment
                                                           :lower 0d0
                                                           :upper 100d0
                                                           :step-increment 1d0)))

(defun _color-spin-change-handler (widget finger color-slot-idx new-value)
    (setf (elt (getf *fingers-colors* finger) color-slot-idx) new-value)
    (format t "~a: finger ~a color slot value ~a changed to ~a value~%" widget finger color-slot-idx new-value))

(defun _delta-spin-change-handler (widget finger color-slot-idx new-value)
    (setf (elt (getf *fingers-deltas* finger) color-slot-idx) (/ new-value 100))
    (format t "~a: finger ~a delta slot value ~a changed to ~a value~%" widget finger color-slot-idx (/ new-value 100)))

(defun run-nijiato ()
  (.print-log. "test: staring test~%")
  (let ((cap-thread (bt:make-thread #'capture-thread :name "capturer")))
    (with-main-loop
      (.print-log. "test: creating gtk window objects~%")
      (let ((window (make-instance 'gtk-window
				   :type :toplevel
				   :window-position :center
				   :title "Nijiato"
				   :default-width *want-width*
				   :default-height *want-height*))
	    (hbox (make-instance 'h-box))
	    (vbox (make-instance 'v-box))
	    (quit-button (make-instance 'button :label "Quit"))
	    (bright-spin (make-instance 'spin-button :label "Brightness"
					:adjustment (make-instance 'adjustment
							 :lower 0d0
							 :upper 100d0
							 :step-increment 1d0)))
            (thumb-r-spin (_make-color-spin "Thumb: Red"))
            (thumb-g-spin (_make-color-spin "Thumb: Green"))
            (thumb-b-spin (_make-color-spin "Thumb: Blue"))
            (thumb-dr-spin (_make-delta-spin "Thumb Delta: Red"))
            (thumb-dg-spin (_make-delta-spin "Thumb Delta: Green"))
            (thumb-db-spin (_make-delta-spin "Thumb Delta: Blue")))

        (.print-log. "test: connecting to value-changing handlers~%")
	(gobject:connect-signal bright-spin "value-changed"
	   (lambda (widget)
              (let ((value (adjustment-value (spin-button-adjustment bright-spin))))
	          (format t "~A changed value to ~F~%" widget value)
		  (unless (without-errors
		      (format t "Previous value was: ~F~%" (v4l2:get-control *capture-fd* v4l2:cid-brightness))
                      (v4l2:set-control *capture-fd* v4l2:cid-brightness (/ value 100)))
                      (v4l2:set-control *capture-fd* v4l2:cid-exposure   (/ value 100))))))

        (gobject:connect-signal thumb-r-spin "value-changed" 
           (lambda (widget) (_color-spin-change-handler widget :thumb 0 (adjustment-value (spin-button-adjustment thumb-r-spin)))))
        (gobject:connect-signal thumb-g-spin "value-changed"
           (lambda (widget) (_color-spin-change-handler widget :thumb 1 (adjustment-value (spin-button-adjustment thumb-g-spin)))))
        (gobject:connect-signal thumb-b-spin "value-changed"
           (lambda (widget) (_color-spin-change-handler widget :thumb 2 (adjustment-value (spin-button-adjustment thumb-b-spin)))))

        (gobject:connect-signal thumb-dr-spin "value-changed"
           (lambda (widget) (_delta-spin-change-handler widget :thumb 0 (adjustment-value (spin-button-adjustment thumb-dr-spin)))))
        (gobject:connect-signal thumb-dg-spin "value-changed"
           (lambda (widget) (_delta-spin-change-handler widget :thumb 1 (adjustment-value (spin-button-adjustment thumb-dg-spin)))))
        (gobject:connect-signal thumb-db-spin "value-changed"
           (lambda (widget) (_delta-spin-change-handler widget :thumb 2 (adjustment-value (spin-button-adjustment thumb-db-spin)))))

        (.print-log. "test: connecting destorying handlers~%")
	(gobject:connect-signal quit-button "clicked"
				(lambda (widget)
				  (declare (ignore widget))
				  (bt:condition-notify *render-thread-stop*)))

	(gobject:connect-signal window "destroy"
				(lambda (widget)
				  (declare (ignore widget))
				  (bt:condition-notify *render-thread-stop*)))

        ;; Capture process needs to know which widget to ask for redraw
        (.print-log. "test: attaching window source to camera~%")
	(setq *camera-widget* (make-instance 'gl-drawing-area
					     :on-init #'camera-init
					     :on-expose #'camera-draw))
	(box-pack-start hbox vbox :expand nil)
	(box-pack-start hbox *camera-widget* :expand t)
	(box-pack-start vbox quit-button :expand nil)
	(box-pack-start vbox bright-spin :expand nil)
	(box-pack-start vbox thumb-r-spin :expand nil)
	(box-pack-start vbox thumb-g-spin :expand nil)
	(box-pack-start vbox thumb-b-spin :expand nil)
	(box-pack-start vbox thumb-dr-spin :expand nil)
        (box-pack-start vbox thumb-dg-spin :expand nil)
        (box-pack-start vbox thumb-db-spin :expand nil)
	(container-add window hbox)
	(widget-show window :all t)))

    (.print-log. "test: running main capturing thread~%")
    ;; Wait for window destruction
    (bt:with-lock-held (*render-thread-lock*)
      (bt:condition-wait *render-thread-stop* *render-thread-lock*))
    (setq *cap-thread-stop* t)
    (bt:join-thread cap-thread)))

(run-nijiato)
