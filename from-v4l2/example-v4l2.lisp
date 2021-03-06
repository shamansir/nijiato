;; $ LD_PRELOAD=/usr/lib64/libv4l/v4l2convert.so sbcl --load example-v4l2.lisp
;; $ LD_PRELOAD=/usr/lib/libv4l/v4l2convert.so sbcl --load example-v4l2.lisp

;; Copyright 2009 Vitaly Mayatskikh <v.mayatskih@gmail.com>
;;
;; This file is a part of CL-Video4Linux2
;;
;; CL-Video4Linux2 is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; CL-Video4Linux2 is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; File is written by v.mayatskih@gmail.com

(asdf:oos 'asdf:load-op :cl-v4l2)
(asdf:oos 'asdf:load-op :cl-gtk2-gtkglext)
(asdf:oos 'asdf:load-op :bordeaux-threads)

(defpackage :test-v4l2
  (:use :common-lisp :gtk :gtkglext))

(in-package :test-v4l2)

(defvar *capture-device* "/dev/video0")
(defparameter *capture-fd* nil)

;; what we want from camera
; (defvar *want-width* 352)
; (defvar *want-height* 288)
(defvar *want-width* 640)
(defvar *want-height* 480)

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
  (format t "diagnose: started diagnostics~%")
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
    (format t "diagnose: capabilities test passed~%")

    (without-errors    
    (format t "diagnose: started slots scanning~%")    
	(loop for idx from 0 do
	     (with-slots (v4l2:index v4l2:name v4l2:type v4l2:tuner)
		 (v4l2:get-input-params fd idx)
	       (format t "diagnose: input [~D] name: ~A, type ~A~%"
		       v4l2:index
		       v4l2:name
		       (if (= v4l2:type v4l2:input-type-tuner) "tuner" "camera"))
	       (when (= v4l2:type v4l2:input-type-tuner)
		 (format t "diagnose: input [~D] connected to tuner ~D~%" v4l2:index v4l2:tuner))

	       (without-errors
           (format t "diagnose: checking slots~%")
		   (loop for idx1 from 0 do
			(with-slots (v4l2:index v4l2:name)
			    (v4l2:get-input-standard fd idx1)
			  (format t "diagnose: input [~D] std [~D] name: ~A~%"
				  idx v4l2:index v4l2:name)))
			(format t "diagnose: slots scanning complete~%")))))

    (format t "diagnose: setting input manually~%")
    (v4l2:set-input fd 0)		; some cameras don't set input by default

    (without-errors
    (format t "diagnose: checking formats~%")    
	(loop for idx from 0 do
	     (with-slots (v4l2:index v4l2:pixelformat) (v4l2:get-format fd idx)
	       (format t "diagnose: format [~D] ~S~%" v4l2:index
		       (format-string v4l2:pixelformat))))
	(format t "diagnose: formats check complete~%"))))

(defun device-init (fd)
  (format t "device-init: initializing devide using v4l2~%")
  (v4l2:set-input fd 0)
  (format t "device-init: input assigned~%")
  (without-errors
      (v4l2:set-control fd v4l2:cid-exposure 0.05))
  (format t "device-init: set ~Dx~D, format ~S~%" *want-width* *want-height*
	  (format-string v4l2:pix-fmt-rgb24))
  (v4l2:set-image-format fd *want-width* *want-height* v4l2:pix-fmt-rgb24)
  (format t "device-init: parameters are set~%")  
  (with-slots (v4l2:width v4l2:height v4l2:sizeimage v4l2:pixelformat)
      (v4l2:format-pix (v4l2:get-image-format fd))
    (setf *got-width* v4l2:width
	  *got-height* v4l2:height)
    (format t "device-init: got ~Dx~D size ~D, format ~S~%"
	    v4l2:width v4l2:height
	    v4l2:sizeimage (format-string v4l2:pixelformat))
    (setq *camera-data* (make-array (* 4 *got-height* *got-width*)
				    :element-type '(unsigned-byte 8)
				    :initial-element #xff)))
  (format t "device-init: initialization finished~%"))

(defun video-init (device)
  (format t "video-init: initializing video~%")
  (let ((fd (isys:open device isys:o-rdwr)))
    (setq *capture-fd* fd)
    (diagnose fd)					; info about device
    (device-init fd)					; setup
    (let ((buffers (v4l2:map-buffers fd 4)))		; map 4 buffers into memory
    
      (v4l2:stream-on fd buffers)			; start capturing
      (format t "video-init: starting to capture~%")
      (values fd buffers))))

(defun video-uninit (fd buffers)
  (format t "video-unit: unititilizing video~%")
  (v4l2:stream-off fd)			; stop capturing
  (v4l2:unmap-buffers buffers)		; throw away buffers from memory
  (isys:close fd)				; close device
  (format t "video-unit: that's all!~%"))

(defun capture-thread ()
  (format t "capture-thread: capturing thread start~%")
  (multiple-value-bind (fd buffers)
      (video-init *capture-device*)
    (format t "capture-thread: returned after init~%")
    (loop thereis *cap-thread-stop* do
	 (let ((frame (without-errors (v4l2:get-frame fd))))		; get one frame from driver
;     (format t "capture-thread: got frame~%")
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
;          (format t "capture-thread: transfer frame~%")
	       (with-main-loop
		   (widget-queue-draw *camera-widget*)))

;        (format t "capture-thread: return frame~%")
	     (v4l2:put-frame fd frame))))	; put frame back to driver
    (video-uninit fd buffers))
  (format t "capture-thread: capturing thread exit~%"))

(defun camera-init (widget)
  (declare (ignore widget))
  (format t "camera-init: camera initialization, binding opengl texture~%")
  (gl:clear-color 0.8 0.8 0.8 0.8)
  (gl:enable :texture-rectangle-arb :depth-test)
  (gl:depth-func :lequal)

  (gl:bind-texture :texture-rectangle-arb 0)

  (format t "camera-init: got-width: ~D; got-height: ~D~%" *got-width* *got-height*)
  (gl:tex-image-2d :texture-rectangle-arb
		   0
		   :rgb8
		   *got-width*
		   *got-height*
		   0
		   :rgba
		   :unsigned-byte
		   *camera-data*)

  (format t "camera-init: after-binding~%")
  (gl:new-list 1 :compile)

  (format t "camera-init: making quads~%")
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
  (format t "camera-init: quads made ~%")  

  (gl:clear-depth 1.0)
  (gl:flush)
  (format t "camera-init: flush~%")  
  
  (format t "camera-init: texture binding succesfull~%"))

(defun camera-draw (widget event)
  (declare (ignore widget event))
; (format t "camera-draw: drawing to the window~%")  
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
  (format t "test: staring test~%")
  (let ((cap-thread (bt:make-thread #'capture-thread :name "capturer")))
    (with-main-loop
      (format t "test: creating gtk window objects~%")
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

    (format t "test: connecting to value-changing handlers~%")
	(gobject:connect-signal bright-spin "value-changed"
				(lambda (widget)
				  (let ((value (adjustment-value (spin-button-adjustment bright-spin))))
				    (format t "~A changed value to ~F~%" widget value)
				    (unless (without-errors
						(format t "Previous value was: ~F~%"
							(v4l2:get-control *capture-fd* v4l2:cid-brightness))
					      (v4l2:set-control *capture-fd* v4l2:cid-brightness (/ value 100)))
				      (v4l2:set-control *capture-fd* v4l2:cid-exposure (/ value 100))))))

    (format t "test: connecting destorying handlers~%")
    
	(gobject:connect-signal quit-button "clicked"
				(lambda (widget)
				  (declare (ignore widget))
				  (bt:condition-notify *render-thread-stop*)))

	(gobject:connect-signal window "destroy"
				(lambda (widget)
				  (declare (ignore widget))
				  (bt:condition-notify *render-thread-stop*)))

;; Capture process needs to know which widget to ask for redraw
    (format t "test: attaching window source to camera~%")
	(setq *camera-widget* (make-instance 'gl-drawing-area
					     :on-init #'camera-init
					     :on-expose #'camera-draw))
	(box-pack-start hbox vbox :expand nil)
	(box-pack-start hbox *camera-widget* :expand t)
	(box-pack-start vbox quit-button :expand nil)
	(box-pack-start vbox bright-spin :expand nil)
	(container-add window hbox)
	(widget-show window :all t)))

    (format t "test: running main capturing thread~%")
;; Wait for window destruction
    (bt:with-lock-held (*render-thread-lock*)
      (bt:condition-wait *render-thread-stop* *render-thread-lock*))
    (setq *cap-thread-stop* t)
    (bt:join-thread cap-thread)))

(test)
