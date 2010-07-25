#|
(defun _make-color-spin (name finger color-slot-idx)
    (make-instance 'spin-button :label name
                                :value (* (elt (getf *fingers-colors* finger) color-slot-idx) 255d0)
                                :adjustment (make-instance 'adjustment
                                                           :lower 0d0
                                                           :upper 255d0
                                                           :step-increment 1d0)))
(defun _make-delta-spin (name finger color-slot-idx)
    (make-instance 'spin-button :label name
				:value (* (elt (getf *fingers-deltas* finger) color-slot-idx) 100d0)
                                :adjustment (make-instance 'adjustment
                                                           :lower 0d0
                                                           :upper 100d0
                                                           :step-increment 1d0)))

(defun _color-spin-change-handler (widget finger color-slot-idx new-value)
    (setf (elt (getf *fingers-colors* finger) color-slot-idx) (/ new-value 255))
    (format t "~a: finger ~a color slot value ~a changed to ~a value~%" widget finger color-slot-idx (/ new-value 255)))

(defun _delta-spin-change-handler (widget finger color-slot-idx new-value)
    (setf (elt (getf *fingers-deltas* finger) color-slot-idx) (/ new-value 100))
    (format t "~a: finger ~a delta slot value ~a changed to ~a value~%" widget finger color-slot-idx (/ new-value 100)))
|#

(in-package :nijiato-recognition)

(use-package :cl-v4l2-demo)

(defun before-run (win-width win-height)
    (setq *fingers-values* (make-array (* win-width win-height)
                           :element-type '(unsigned-byte 8)
                           :initial-element 0)))

(defun every-frame (frame-num) (%send-out frame-num))

(defun use-and-get-pixel (i r g b)
    (setf (aref *fingers-values* i) (get-finger-value (/ r 255) (/ g 255) (/ b 255)))

    (if (= (elt *fingers-values* i) 0)
        (let ((result (vector 0 0 0)))
             (setf (elt result 0) r
                   (elt result 1) g
                   (elt result 2) b) result)
		(color-from-finger-value (elt *fingers-values* i))))
		
(v4l2demo:run-demo v4l2demo:before-run before-run
                   v4l2demo:every-frame every-frame
                   v4l2demo:switch-pixel nil)
