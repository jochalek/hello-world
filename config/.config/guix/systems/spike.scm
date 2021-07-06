;; spike-specific system config


(define-module (spike)
  #:use-module (base-system)
  #:use-module (gnu))

(operating-system
 (inherit base-operating-system)
 (host-name "spike")

  (mapped-devices
    (list (mapped-device
            (source
              (uuid "d022b282-2827-4175-9d6c-056dc6e7d940"))
            (target "guix")
            (type luks-device-mapping))))
  (file-systems
    (cons* (file-system
             (mount-point "/")
             (device "/dev/mapper/guix")
             (type "ext4")
             (dependencies mapped-devices))
           (file-system
             (mount-point "/boot/efi")
             (device (uuid "1C87-1867" 'fat32))
             (type "vfat"))
           %base-file-systems)))
