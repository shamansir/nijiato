;; $ LD_PRELOAD=/usr/lib64/libv4l/v4l2convert.so sbcl --load example.lisp

;; File is written by v.mayatskih@gmail.cpm
;; noted in this post: http://13-49.blogspot.com/2009/09/eye-region-tracking-in-common-lisp.html

(asdf:oos 'asdf:test-op :iolib)
(asdf:oos 'asdf:load-op :cl-v4l2)
(asdf:oos 'asdf:load-op :cl-gtk2-gtkglext)
(asdf:oos 'asdf:load-op :bordeaux-threads)

(defpackage :eyes
  (:use :common-lisp :gtk :gtkglext)
  (:import-from :iolib.syscalls %sys-open %sys-close o-rdwr))

(in-package :eyes)

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

(defparameter *show-rgb* t)		; show original color image
(defparameter *show-grayscale* nil)	; show grayscale image
(defparameter *show-sobel* nil)		; show filtered image
(defparameter *show-hists* t)		; put histograms on top of image

(defparameter *hist-approximation* -3)	; aggressiveness of median filter
(defparameter *margin* 0.2)		; size of non-filtered margin [0.0...0.5]

(defparameter *sobel* nil)
;;;;;

(defun aver1 (list n)
  (loop
     with avg = (loop for i fixnum in list sum i into average finally (return (floor (/ average (length list)))))
     with w = (append (make-list (ash n -1) :initial-element avg)
		      list
		      (make-list (ash n -1) :initial-element avg))
     for i fixnum from 0 below (length list)
     collect (loop for j fixnum from i to (+ i (ash n -1))
		sum (elt w j) into average
		finally (return (floor (/ average n))))))

;; m = 5, k = 8
(defun aver (list)
  (loop
     with len = (length list)
     with avg = (loop for i fixnum in list sum i into average finally (return (floor (/ average len))))
     with w = (append (make-list 4 :initial-element avg)
		      list
		      (make-list 4 :initial-element avg))
     for i fixnum from 4 below (+ len 4)
     collect
       (floor (* 1/429
		 (+
		  (* 15 (elt w (- i 4)))
		  (* -55 (elt w (- i 3)))
		  (* 30 (elt w (- i 2)))
		  (* 135 (elt w (- i 1)))
		  (* 179 (elt w i))
		  (* 135 (elt w (+ i 1)))
		  (* 30 (elt w (+ i 2)))
		  (* -55 (elt w (+ i 3)))
		  (* 15 (elt w (+ i 4))))))))

(defparameter *data* nil)

(defun prepare (read w h)
  (setq *data* read)

;; Sobels operator
  (loop for y fixnum from 1 below (1- h) do
       (loop for x fixnum from 1 below (1- w) do
	    (let ((_00 (aref *data* (+ (* (1- y) w) (1- x))))
		  (_01 (aref *data* (+ (* (1- y) w) x)))
		  (_02 (aref *data* (+ (* (1- y) w) (1+ x))))
		  (_10 (aref *data* (+ (* y w) (1- x))))
		  (_12 (aref *data* (+ (* y w) (1+ x))))
		  (_20 (aref *data* (+ (* (1+ y) w) (1- x))))
		  (_21 (aref *data* (+ (* (1+ y) w) x)))
		  (_22 (aref *data* (+ (* (1+ y) w) (1+ x)))))
	      (let ((gx (floor (* 1/16 (+ (* _00 +3) (* _01 +10) (* _02 +3)
					  (* _20 -3) (* _21 -10) (* _22 -3)))))
		    (gy (floor (* 1/16 (+ (* _00 +3) (* _02 -3)
					  (* _10 +10) (* _12 -10)
					  (* _20 +3) (* _22 -3))))))
		(setf (aref *sobel* (+ (* y w) x)) (mod (floor (sqrt (+ (* gx gx) (* gy gy)))) #xff))))))

  (setq *data* *sobel*)
#|
;; Roberts cross
  (loop for y fixnum from 2 below (- h 2) do
       (loop for x fixnum from 2 below (- w 2) do
	    (let ((_00 (aref *data* (+ (* (- y 2) w) (- x 2))))
		  (_04 (aref *data* (+ (* (- y 2) w) (+ x 2))))
		  (_11 (aref *data* (+ (* (- y 1) w) (- x 1))))
		  (_13 (aref *data* (+ (* (- y 1) w) (+ x 1))))
		  (_31 (aref *data* (+ (* (+ y 1) w) (- x 1))))
		  (_33 (aref *data* (+ (* (+ y 1) w) (+ x 1))))
		  (_40 (aref *data* (+ (* (+ y 2) w) (- x 2))))
		  (_44 (aref *data* (+ (* (+ y 2) w) (+ x 2)))))

	      (let* ((gx (+ (* _00 +1) (* _11 +2)
			    (* _33 -2) (* _44 -1)))
		     (gy (+ (* _04 +1) (* _13 +2)
			    (* _31 -2) (* _40 -1)))
		     (g  (floor (sqrt (+ (* gx gx) (* gy gy))))))
		(when (> g #xff)
		  (setq g #xff))
		(when (< g 0)
		  (setq g 0))
		(setf (aref *sobel* (+ (* y w) x)) g)))))

  (setq *data* *sobel*)
|#
;; Calculate energy at each projection of row/column
  (list
   (aver1 (loop for y fixnum from 0 below h
	     collect (loop for x fixnum from 0 below w
			sum (aref *sobel* (+ (* y w) x))))
	  (ash h *hist-approximation*))

   (aver1 (loop for x fixnum from 0 below w
	     collect (loop for y fixnum from 0 below h
			sum (aref *sobel* (+ (* y w) x))))
	  (ash w *hist-approximation*))))

(defun find-local-extremums (list)
  (loop
     with len = (length list)
     with start = (floor (* len *margin*))
     with end = (floor (* len (- 1 *margin*)))
     with df0 = nil
     with f0 = (elt list 0)
     for i from 0
     for f  in list
     for df = (- f f0)

     when (and
	   (> i start) (< i end)
	   (> i 1)
	   (or
	    (and (> df0 0) (< df 0))
	    (and (< df0 0) (> df 0))))
     collect (cons (- i 1/2) (/ (+ f0 f) 2))
     do (setf f0 f
	      df0 df)))

(defun find-extremum (list n &key round)
  (loop with s =
       (sort (find-local-extremums list)
	     #'> :key #'cdr)
     for (x . y) in s
     unless (null :round) do (setq x (round x))
     when (= (length vals) n)
     do (return vals)
     collect x into vals
     finally (return vals)))

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
  (let ((fd (%sys-open device o-rdwr)))
    (setq *capture-fd* fd)
    (diagnose fd)					; info about device
    (device-init fd)					; setup
    (let ((buffers (v4l2:map-buffers fd 4)))		; map 4 buffers into memory
      (v4l2:stream-on fd buffers)			; start capturing
      (values fd buffers))))

(defun video-uninit (fd buffers)
  (v4l2:stream-off fd)			; stop capturing
  (v4l2:unmap-buffers buffers)		; throw away buffers from memory
  (%sys-close fd)				; close device
  (format t "that's all!~%"))

(defvar sigma)

(defun capture-thread ()
  (format t "cap thread start~%")
  (multiple-value-bind (fd buffers)
      (video-init *capture-device*)
    (setq sigma (make-array (* *got-width* *got-height*)
			  :element-type '(unsigned-byte 8))
	  *sobel* (make-array (* *got-width* *got-height*)
			      :element-type '(unsigned-byte 8)))
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
		      (let* ((r (cffi:mem-aref address :uchar (+ (* 3 i) 0)))
			     (g (cffi:mem-aref address :uchar (+ (* 3 i) 1)))
			     (b (cffi:mem-aref address :uchar (+ (* 3 i) 2)))
			     (val (floor (+ (* 0.3 r) (* 0.59 g) (* 0.11 b)))))
			(setf (aref sigma i) val)
			(when *show-rgb*
			  (setf (aref *camera-data* (+ (* 4 i) 0)) r
				(aref *camera-data* (+ (* 4 i) 1)) g
				(aref *camera-data* (+ (* 4 i) 2)) b))
			(when *show-grayscale*
			  (setf (aref *camera-data* (+ (* 4 i) 0)) val
				(aref *camera-data* (+ (* 4 i) 1)) val
				(aref *camera-data* (+ (* 4 i) 2)) val))))

		 (let* ((l (prepare sigma *got-width* *got-height*))
			(w (car l))
			(h (cadr l))
			(wm (loop for i in w maximizing i))
			(hm (loop for i in h maximizing i))
			(wa (/ (loop for i in w summing i) (length w)))
			(ha (/ (loop for i in h summing i) (length h))))

		   (when *show-sobel*
		     (loop for i fixnum from 0 below (* *got-width* *got-height*)
			for val = (aref *data* i)
			do (setf (aref *camera-data* (+ (* 4 i) 0)) val
				 (aref *camera-data* (+ (* 4 i) 1)) val
				 (aref *camera-data* (+ (* 4 i) 2)) val)))

		   (when *show-hists*
		     (loop with last = 0
			for i fixnum from 0 below *got-width*
			for val in h
			for y = (- (1- *got-height*) (floor (* (* 0.99 (/ val hm)) *got-height*)))
			do (loop for j from (if (> y last) last y) to (if (> y last) y last) do
				(setf (aref *camera-data* (* 4 (+ (* j *got-width*) i))) #xff))
			do (setq last y))

		     (loop with last = 0
			for i from 0 below *got-height*
			for val in w
			for x = (floor (* (* 0.99 (/ val wm)) *got-width*))
			do (loop for j from (if (> x last) last x) to (if (> x last) x last) do
				(setf (aref *camera-data* (+ 1 (* 4 (+ (* i *got-width*) j)))) #xff))
			do (setq last x))

		     (loop
			with y = (car (find-extremum w 1 :round t));(car (sort (find-extremum w 2 :round t) #'<))
			for x from 0 below *got-width* do
			  (setf (aref *camera-data* (+ 2 (* 4 (+ (* y *got-width*) x)))) #xff))


		     (loop
			with min = (loop for i in h minimizing i)
			with x =
			  (-
			   (loop with s = (/ (loop for i in h sum (- i min)) 2)
			      for x1 from 0
			      for i in h sum (- i min) into is
			      when (> is s) do
				(return x1))
			   (ash *got-width* (* *hist-approximation* 2)))
			for y from 0 below *got-height* do
			  (setf (aref *camera-data* (+ 2 (* 4 (+ (* y *got-width*) x)))) #xff))))))


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
