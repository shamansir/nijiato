;; $ LD_PRELOAD=/usr/lib64/libv4l/v4l2convert.so sbcl --load example.lisp
;; $ LD_PRELOAD=/usr/lib/libv4l/v4l2convert.so sbcl --load example.lisp

;; File is written by v.mayatskih@gmail.cpm
;; noted in this post: http://13-49.blogspot.com/2009/09/eye-region-tracking-in-common-lisp.html

(asdf:oos 'asdf:load-op :cl-v4l2)
(asdf:oos 'asdf:load-op :cl-gtk2-gtkglext)
(asdf:oos 'asdf:load-op :bordeaux-threads)

(defpackage :test-v4l2
  (:use :common-lisp :gtk :gtkglext))

(in-package :test-v4l2)

(defvar *capture-device* "/dev/video0")
(defparameter *capture-fd* nil)

;; what we want from camera
(defvar *want-width* 352)
(defvar *want-height* 288)

;; what we really get
(defparameter *got-width* nil)
(defparameter *got-height* nil)

(defparameter *camera-widget* nil)
(defparameter *camera-data* nil)
(defparameter *camera-data-lock* (bt:make-lock "Camera data lock"))

(defparameter *cap-thread-stop* nil)

(defparameter *render-thread-stop* (bt:make-condition-variable))
(defparameter *render-thread-lock* (bt:make-lock "Render thread lock"))

(defmacro without-errors (&body body)
  `(handler-case (progn ,@body)
     (error (c) (format t "suppressed error: ~A~%" c) nil)))

(defun char-at (pos data)
  (code-char (ldb (byte 8 (* 8 pos)) data)))

(defun format-string (pixfmt)
  (format nil "~C~C~C~C"
	  (char-at 0 pixfmt)
	  (char-at 1 pixfmt)
	  (char-at 2 pixfmt)
	  (char-at 3 pixfmt)))

(defun diagnose (fd)
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

    (without-errors
	(loop for idx from 0 do
	     (with-slots (v4l2:index v4l2:name v4l2:type v4l2:tuner)
		 (v4l2:get-input-params fd idx)
	       (format t "input [~D] name: ~A, type ~A~%"
		       v4l2:index
		       v4l2:name
		       (if (= v4l2:type v4l2:input-type-tuner) "tuner" "camera"))
	       (when (= v4l2:type v4l2:input-type-tuner)
		 (format t "input [~D] connected to tuner ~D~%" v4l2:index v4l2:tuner))

	       (without-errors
		   (loop for idx1 from 0 do
			(with-slots (v4l2:index v4l2:name)
			    (v4l2:get-input-standard fd idx1)
			  (format t "input [~D] std [~D] name: ~A~%"
				  idx v4l2:index v4l2:name)))))))

    (v4l2:set-input fd 0)		; some cameras don't set input by default

    (without-errors
	(loop for idx from 0 do
	     (with-slots (v4l2:index v4l2:pixelformat) (v4l2:get-format fd idx)
	       (format t "format [~D] ~S~%" v4l2:index
		       (format-string v4l2:pixelformat)))))))

(defun device-init (fd)
  (v4l2:set-input fd 0)
  (without-errors
      (v4l2:set-control fd v4l2:cid-exposure 0.05))
  (format t "set ~Dx~D, format ~S~%" *want-width* *want-height*
	  (format-string v4l2:pix-fmt-rgb24))
  (v4l2:set-image-format fd *want-width* *want-height* v4l2:pix-fmt-rgb24)
  (with-slots (v4l2:width v4l2:height v4l2:sizeimage v4l2:pixelformat)
      (v4l2:format-pix (v4l2:get-image-format fd))
    (setf *got-width* v4l2:width
	  *got-height* v4l2:height)
    (format t "got ~Dx~D size ~D, format ~S~%"
	    v4l2:width v4l2:height
	    v4l2:sizeimage (format-string v4l2:pixelformat))
    (setq *camera-data* (make-array (* 4 *got-height* *got-width*)
				    :element-type '(unsigned-byte 8)
				    :initial-element #xff))))

(defun video-init (device)
  (let ((fd (isys:open device isys:o-rdwr)))
    (setq *capture-fd* fd)
    (diagnose fd)					; info about device
    (device-init fd)					; setup
    (let ((buffers (v4l2:map-buffers fd 4)))		; map 4 buffers into memory
      (v4l2:stream-on fd buffers)			; start capturing
      (values fd buffers))))

(defun video-uninit (fd buffers)
  (v4l2:stream-off fd)			; stop capturing
  (v4l2:unmap-buffers buffers)		; throw away buffers from memory
  (isys:close fd)				; close device
  (format t "that's all!~%"))

(defun capture-thread ()
  (format t "cap thread start~%")
  (multiple-value-bind (fd buffers)
      (video-init *capture-device*)
    (loop thereis *cap-thread-stop* do
	 (let ((frame (without-errors (v4l2:get-frame fd))))		; get one frame from driver
	   (when frame			; errors from v4l2convert.so are highly possible
	     (multiple-value-bind (buffer address)
		 (values-list (nth frame buffers))
	       (declare (ignore buffer))
	       ;; Silly rgb24->rgb32 converter
	       (bt:with-lock-held (*camera-data-lock*)
		 (declare (optimize (speed 3) (debug 0) (safety 0)))
		 (loop for i fixnum from 0 below (* *got-width* *got-height*) do
		      (let ((r (cffi:mem-aref address :uchar (+ (* 3 i) 0)))
			    (g (cffi:mem-aref address :uchar (+ (* 3 i) 1)))
			    (b (cffi:mem-aref address :uchar (+ (* 3 i) 2))))

			(setf (aref *camera-data* (+ (* 4 i) 0)) r
			      (aref *camera-data* (+ (* 4 i) 1)) g
			      (aref *camera-data* (+ (* 4 i) 2)) b)))))

	     (when *camera-widget*
	       (with-main-loop
		   (widget-queue-draw *camera-widget*)))

	     (v4l2:put-frame fd frame))))	; put frame back to driver
    (video-uninit fd buffers))
  (format t "cap thread exit~%"))

(defun camera-init (widget)
  (declare (ignore widget))
  (gl:clear-color 0.8 0.8 0.8 0.8)
  (gl:enable :texture-rectangle-arb :depth-test)
  (gl:depth-func :lequal)

  (gl:bind-texture :texture-rectangle-arb 0)

  (gl:tex-image-2d :texture-rectangle-arb
		   0
		   :rgb8
		   *got-width*
		   *got-height*
		   0
		   :rgba
		   :unsigned-byte
		   *camera-data*)

  (gl:new-list 1 :compile)

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

  (gl:clear-depth 1.0)
  (gl:flush))

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

(defun test ()
  (let ((cap-thread (bt:make-thread #'capture-thread :name "capturer")))
    (with-main-loop
      (let ((window (make-instance 'gtk-window
				   :type :toplevel
				   :window-position :center
				   :title "Hello world!"
				   :default-width *want-width*
				   :default-height *want-height*))
	    (hbox (make-instance 'h-box))
	    (vbox (make-instance 'v-box))
	    (quit-button (make-instance 'button :label "Quit"))
	    (bright-spin (make-instance 'spin-button :label "Brightness"
					:adjustment (make-instance 'adjustment
								   :lower 0d0
								   :upper 100d0
								   :step-increment 1d0))))


	(gobject:connect-signal bright-spin "value-changed"
				(lambda (widget)
				  (let ((value (adjustment-value (spin-button-adjustment bright-spin))))
				    (format t "~A changed value to ~F~%" widget value)
				    (unless (without-errors
						(format t "Previous value was: ~F~%"
							(v4l2:get-control *capture-fd* v4l2:cid-brightness))
					      (v4l2:set-control *capture-fd* v4l2:cid-brightness (/ value 100)))
				      (v4l2:set-control *capture-fd* v4l2:cid-exposure (/ value 100))))))

	(gobject:connect-signal quit-button "clicked"
				(lambda (widget)
				  (declare (ignore widget))
				  (bt:condition-notify *render-thread-stop*)))

	(gobject:connect-signal window "destroy"
				(lambda (widget)
				  (declare (ignore widget))
				  (bt:condition-notify *render-thread-stop*)))

;; Capture process needs to know which widget to ask for redraw
	(setq *camera-widget* (make-instance 'gl-drawing-area
					     :on-init #'camera-init
					     :on-expose #'camera-draw))
	(box-pack-start hbox vbox :expand nil)
	(box-pack-start hbox *camera-widget* :expand t)
	(box-pack-start vbox quit-button :expand nil)
	(box-pack-start vbox bright-spin :expand nil)
	(container-add window hbox)
	(widget-show window :all t)))

;; Wait for window destruction
    (bt:with-lock-held (*render-thread-lock*)
      (bt:condition-wait *render-thread-stop* *render-thread-lock*))
    (setq *cap-thread-stop* t)
    (bt:join-thread cap-thread)))

(test)
