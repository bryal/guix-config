;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu))
(use-service-modules desktop networking ssh xorg)
(use-package-modules version-control file gnuzilla)

(operating-system
  (locale "en_US.utf8")
  (timezone "Europe/Stockholm")
  (keyboard-layout
    (keyboard-layout "us" "altgr-intl"))
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (target "/boot/efi")
      (keyboard-layout keyboard-layout)))
  (mapped-devices
    (list (mapped-device
            (source
              (uuid "3e8d8072-ee94-407a-9d04-f2f62e1b3743"))
            (target "cryptroot")
            (type luks-device-mapping))))
  (file-systems
    (cons* (file-system
             (mount-point "/")
             (device "/dev/mapper/cryptroot")
             (type "ext4")
             (dependencies mapped-devices))
           (file-system
             (mount-point "/boot/efi")
             (device (uuid "FF1E-537C" 'fat32))
             (type "vfat"))
           %base-file-systems))
  (host-name "spiral")
  (users (cons* (user-account
                  (name "jojo")
                  (comment "It's Ya Boii")
                  (group "users")
                  (home-directory "/home/jojo")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))
  (packages
    (cons* (specification->package "nss-certs")
           git
           file
           icecat
           %base-packages))
  (services
    (append
      (list (service gnome-desktop-service-type)
            (set-xorg-configuration
              (xorg-configuration
                (keyboard-layout keyboard-layout))))
      %desktop-services)))
