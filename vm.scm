;; (use-modules (gnu)
;;              (gnu image))
;;
;; (load "system.scm")
;;
;; (define vm-image
;;   (image
;;    (format 'compressed-qcow2)
;;    (operating-system physical-operating-system)
;;    (size (* 10 (expt 2 30))))) ; ~10 GiB
;;
;; vm-image

(use-modules (gnu)
             (gnu image)
             (gnu system vm)  ; For qcow2-image
             (gnu system)     ; For the operating-system type
             (gnu system file-systems))  ; To properly define partitions

(load "system.scm")

;; (define (boot-partition size)
;;   (parted-file-system
;;    (label "boot")
;;    ;; (file-system (type "ext4"))
;;    (file-system
;;      (mount-point "/boot")
;;      (device "/dev/vda1")
;;      (type "ext4"))
;;    (size size)
;;    (mount-point "/boot")))
;;
;; (define (root-partition size)
;;   (parted-file-system
;;    (label "root")
;;    ;; (file-system (type "ext4"))
;;    (file-system
;;      (mount-point "/")
;;      (device "/dev/vda1")
;;      (type "ext4"))
;;    (size size)
;;    (mount-point "/")))

(define (boot-partition s)
  (partition
   (size s)
   ;; (mount-point "/boot")
   ;; (device "/dev/vda1")
   (file-system "ext4")
   (label "boot")
   (flags '(boot))))

(define (root-partition s)
  (partition
   (size s)
   ;; (mount-point "/")
   ;; (device "/dev/vda1")
   (file-system "ext4")
   (label "root")))

(define vm-image
  (image
   (operating-system physical-operating-system)
   (format 'compressed-qcow2)
   (size (* 10 (expt 2 30)))
   (partitions
    (list (boot-partition (* 1 (expt 2 30)))  ; 1 GiB boot partition
          (root-partition (* 9 (expt 2 30)))))))  ; 9 GiB root partition

vm-image
