(use-modules (gnu)
             (gnu image)
             (gnu system vm)  ; For qcow2-image
             (gnu system)     ; For the operating-system type
             (gnu system file-systems))  ; To properly define partitions

(load "system.scm")

(define (boot-partition s)
  (partition
   (size s)
   (file-system "vfat")
   (label "EFI")
   (flags '(boot))))

(define (root-partition s)
  (partition
   (size s)
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
