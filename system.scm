(use-modules (gnu)
             (nongnu packages linux)
             (guix profiles)
             (guix packages)
             (guix scripts package))

(use-service-modules
  cups
  desktop
  networking
  ssh
  xorg)

(load "guix/fonts.scm")

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
         '("nss-certs"
           "gcc-toolchain"
           "make"
           "git"
           "emacs"
           "vim"
           "tree"
           "ungoogled-chromium"
           "the-silver-searcher"
           "ripgrep"
           "font-iosevka"))  ; Add your standard packages here
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
                (keyboard-layout keyboard-layout))))
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
           %base-file-systems)))
