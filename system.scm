(use-modules (gnu)
             (gnu image)
             (gnu system image)
             (nongnu packages linux)
             (guix profiles)
             (guix packages)
             (guix scripts package))

(use-service-modules
  cups
  desktop
  networking
  ssh
  xorg
  docker)

(load "guix/fonts.scm")

(define physical-operating-system
  (operating-system
   (kernel linux)
   (firmware (list linux-firmware))
   (locale "en_US.utf8")
   (timezone "America/New_York")
   (keyboard-layout (keyboard-layout "us"))
   (host-name "guix")
   (users (cons* (user-account
                  (name "twashing")
                  (comment "twashing")
                  (group "users")
                  (home-directory "/home/twashing")
                  (supplementary-groups
                   '("wheel" "netdev" "audio" "video")))
                 %base-user-accounts))
   (packages
    (append
     (map specification->package
          '("gcc-toolchain"
            "make"
            "git"
            "emacs" "vim"
            "tree"
            "stow"
            "zip" "unzip"
            "wget"

            "ungoogled-chromium"
            "the-silver-searcher"
            "ripgrep"
            "font-iosevka"

            ;; The `containerd-service-type` manages the installation and configuration of `containerd`
            ;; so we don't need to include it separately in the `packages` list
            ;; "containerd"
            "docker"
            "docker-cli"

            "netcat"
            "nmap"
            "openssl"
            "openvpn"
            "network-manager-openvpn"
            "ghostty"))  ; Add your standard packages here
     (list pragmata-pro)  ; Add custom packages directly
     %base-packages))
   (services
    (append
     (list (service gnome-desktop-service-type)
           (service xfce-desktop-service-type)
           (service openssh-service-type)
           (service tor-service-type)
           (set-xorg-configuration
            (xorg-configuration
             (keyboard-layout keyboard-layout)))
           (service containerd-service-type
              (containerd-configuration
                ;; You can specify additional configuration here if needed
                ))
           (service docker-service-type
                    (docker-configuration
                     (debug? #t)                     ; Enable debug logging (optional)
                     ;; (hosts '("unix:///var/run/docker.sock"))  ; Specify the socket
                     ;; Add more configuration options as needed
                     )))
     %desktop-services))
   (bootloader
    (bootloader-configuration
     (bootloader grub-efi-bootloader)
     (targets (list "/boot/efi"))
     (keyboard-layout keyboard-layout)))
   (swap-devices
    (list (swap-space
           (target
            (uuid "e5f1a4a1-f2fd-4e7e-8ec7-05abcd4d70f3")))))
   (file-systems
    (cons* (file-system
            (mount-point "/boot/efi")
            (device (uuid "E6AE-7A56" 'fat32))
            (type "vfat"))
           (file-system
            (mount-point "/")
            (device
             (uuid "e67e1014-9dfe-4b14-a113-ce9c56f963bf"
                   'ext4))
            (type "ext4"))
           %base-file-systems))))

physical-operating-system
