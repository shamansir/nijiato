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
		
(v4l2demo:run-demo :before-run #'init-fingers-values
                   :every-frame nil
                   :switch-pixel #'visualize-value)
