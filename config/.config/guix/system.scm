;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu) (nongnu packages linux))
(use-service-modules desktop networking ssh xorg)

(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "America/New_York")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "spike")
  (users (cons* (user-account
                  (name "justin")
                  (comment "Justin Ochalek")
                  (group "users")
                  (home-directory "/home/justin")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))
  (packages
    (append
      (list (specification->package "emacs-native-comp")
            (specification->package "emacs-exwm")
            (specification->package
              "emacs-desktop-environment")
            (specification->package "nss-certs"))
      %base-packages))
  (services
    (append
      (list (service gnome-desktop-service-type)
            (service openssh-service-type)
            (set-xorg-configuration
              (xorg-configuration
                (keyboard-layout keyboard-layout))))
      %desktop-services))
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (target "/boot/efi")
      (keyboard-layout keyboard-layout)))
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
