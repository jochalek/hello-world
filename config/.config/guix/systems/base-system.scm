;; This is an operating system configuration generated
;; by the graphical installer.

(define-module (base-system)
  #:use-module (gnu)
  #:use-module (srfi srfi-1)
  #:use-module (gnu system nss)
  #:use-module (gnu services pm)
  #:use-module (gnu services cups)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services networking)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services sound)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages certs)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (flat packages emacs))

(use-service-modules networking ssh)
(use-service-modules nix)
(use-service-modules desktop xorg)
(use-package-modules certs)
(use-package-modules shells)


(define %my-desktop-services
  (modify-services %desktop-services
                   (elogind-service-type config =>
                                         (elogind-configuration (inherit config)
                                                                (handle-lid-switch-external-power 'suspend)))
                   ;; (udev-service-type config =>
                   ;;                    (udev-configuration (inherit config)
                   ;;                                        (rules (cons %backlight-udev-rule
                   ;;                                                     (udev-configuration-rules config)))))
                   (network-manager-service-type config =>
                                                 (network-manager-configuration (inherit config)
                                                                                (vpn-plugins (list network-manager-openvpn))))))

(define-public base-operating-system
(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
 (initrd microcode-initrd)
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
                    '("wheel" "netdev" "audio" "video" "lp" "tty")))
                %base-user-accounts))

    ;; Install common system packages
    (packages (append (list
                        fd
                        ripgrep
                        git
                        stow
                        vim
                        emacs-native-comp
                        xterm
                        bluez
                        bluez-alsa
                        pulseaudio
                        tlp
                        nss-certs)     ;; for HTTPS access
                    %base-packages))

  (services
    (append
      (list (service gnome-desktop-service-type)
            (service openssh-service-type)
            (service alsa-service-type)
            (service nix-service-type)
            (bluetooth-service #:auto-enable? #t)
            (set-xorg-configuration
              (xorg-configuration
                (keyboard-layout keyboard-layout))))
      %my-desktop-services))

  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (target "/boot/efi")
      (keyboard-layout keyboard-layout)))

  ;; Define file-system to be overridden by system-specific config
  (file-systems
    (cons* (file-system
             (mount-point "/tmp")
             (device "none")
             (type "tmpfs")
             (check? #f))
           %base-file-systems))))
