(use-modules (gnu))

(operating-system
  (host-name "minimal-guix")
  (timezone "Europe/Athens")
  (locale "en_US.utf8")

  (users (cons (user-account
                (name "user")
                (comment "Container User")
                (group "users")
                (supplementary-groups '("wheel" "audio" "video")))
               %base-user-accounts))

  ;; Globally-installed packages.
  (packages (map specification->package
                 (list "bash"
                       "coreutils"
                       "guix")))

  ;; No bootloader needed.
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("does-not-matter"))))

  ;; No filesystems needed
  (file-systems (list (file-system
                        (device "does-not-matter")
                        (mount-point "/")
                        (type "does-not-matter"))))

  ;; Basic services
  (services (list (service guix-service-type)
                  (service syslog-service-type)
                  (service nscd-service-type))))
