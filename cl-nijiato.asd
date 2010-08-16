(defpackage #:cl-nijiato-asd
  (:use :cl :asdf))

(in-package #:cl-nijiato-asd)

(defsystem cl-nijiato
  :name "cl-nijiato"
  :version "0.1"
  :author "Ulric Wilfred <shaman.sir@gmail.com>"
  :licence "GPLv3"
  :description "Detecting fingers motion using rainbow-colored paper-marks"
  :serial t
  :components ((:file "nijiato-recognition"
                     :depends-on ("package"))
               (:file "package"))
  :depends-on (:babel
               :iterate
               :trivial-features
               :trivial-garbage
               :closer-mop
               :cffi
               :alexandria
               :bordeaux-threads
               :iolib
               :cl-v4l2
               :cl-opengl
               :cl-gtk2-gtk))
