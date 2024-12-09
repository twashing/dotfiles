(use-modules (gnu)
             (gnu image))

(load "system.scm")

(define vm-image
  (image
   (format 'compressed-qcow2)
   (operating-system physical-operating-system)
   (size (* 10 (expt 2 30))))) ; ~10 GiB

vm-image
