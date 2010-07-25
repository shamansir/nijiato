;; $ LD_PRELOAD=/usr/lib/libv4l/v4l2convert.so [rlwrap] sbcl --load nijiato-demo-load.lisp
;; $ LD_PRELOAD=/usr/lib64/libv4l/v4l2convert.so [rlwrap] sbcl --load nijiato-demo-load.lisp

(load "v4l2-demo-with-window.lisp")
(load "package.lisp")
(load "nijiato-recognition.lisp")
(load "nijiato-demo-run.lisp")

