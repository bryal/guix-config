(use-modules (gnu)
	     (guix packages)
	     (guix git-download)
	     (guix build-system trivial)
	     ((guix licenses) #:prefix license:)
	     (guix download)
             (srfi srfi-1))
(use-service-modules shepherd networking ssh desktop xorg)
(use-package-modules certs
                     bash
		     screen
                     ncurses
		     linux
		     version-control
		     emacs
                     emacs-xyz
		     gnome
		     syncthing
		     sync
		     xorg
		     fonts
		     gnuzilla
		     stalonetray
                     gdb
                     file
                     python
                     python-xyz
		     xdisorg
                     freedesktop
                     pulseaudio
                     ssh)

(define (linux-nonfree-urls version)
  "Return a list of URLs for Linux-Nonfree VERSION."
  (list (string-append
         "https://www.kernel.org/pub/linux/kernel/v5.x/"
         "linux-" version ".tar.xz")))

(define-public linux-firmware-non-free
  (package
    (name "linux-firmware-non-free")
    (version "92e17d0dd2437140fab044ae62baf69b35d7d1fa")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.kernel.org/pub/scm/linux/kernel/git/firmware/linux-firmware.git")
                    (commit version)))
              (sha256
               (base32
                "1anr7fblxfcrfrrgq98kzy64yrwygc2wdgi47skdmjxhi3wbrvxz"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((source (assoc-ref %build-inputs "source"))
                         (fw-dir (string-append %output "/lib/firmware/")))
                     (mkdir-p fw-dir)
                     (copy-recursively source fw-dir)
                     #t))))

    (home-page "")
    (synopsis "Non-free firmware for Linux")
    (description "Non-free firmware for Linux")
    ;; FIXME: What license?
    (license (license:non-copyleft "http://git.kernel.org/?p=linux/kernel/git/firmware/linux-firmware.git;a=blob_plain;f=LICENCE.radeon_firmware;hb=HEAD"))))

(define-public linux-nonfree
  (let* ((version "5.2.9"))
    (package
      (inherit linux-libre)
      (name "linux-nonfree")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (linux-nonfree-urls version))
                (sha256
                 (base32
                  "1rnlnphw9rih4qhdld9ic5lnj5jh4vy5nqbj9lqwv9bc615jmw5n"))))
      (synopsis "Mainline Linux kernel, nonfree binary blobs included.")
      (description "Linux is a kernel.")
      (license license:gpl2)
      (home-page "http://kernel.org/"))))

(define intel-xorg-conf
  "
Section \"Device\"
        Identifier  \"Intel Graphics\"
        Driver      \"intel\"
        Option      \"Backlight\"  \"intel_backlight\"
EndSection")

(define touchpad-xorg-conf
  "
Section \"InputClass\"
        Identifier  \"Touchpad\"
        Driver      \"libinput\"
        MatchIsTouchpad \"on\"
        Option \"Tapping\" \"on\"
        Option \"ClickMethod\" \"clickfinger\"
EndSection")

(define touchscreen-xorg-conf
  "
Section \"InputClass\"
    Identifier         \"Touchscreen catchall\"
    MatchIsTouchscreen \"on\"
    Option \"Ignore\" \"on\"
EndSection")

;; This doesn't seem to be exported by the xorg module, so I have to define it
;; manually.
(define %default-xorg-server-arguments
  ;; Default command-line arguments for X.
  '("-nolisten" "tcp"))

(operating-system
  (host-name "spiral")
  (timezone "Europe/Stockholm")
  (locale "en_US.utf8")
  (keyboard-layout
   (keyboard-layout "us" "altgr-intl" #:options '("ctrl:nocaps")))
  (kernel linux-nonfree)
  (firmware (cons* linux-firmware-non-free
                   %base-firmware))

  ;; Use the UEFI variant of GRUB with the EFI System
  ;; Partition mounted on /boot/efi.
  (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader)
               (target "/boot/efi")
               (keyboard-layout keyboard-layout)))

  ;; Specify a mapped device for the encrypted root partition.
  ;; The UUID is that returned by 'cryptsetup luksUUID'.
  (mapped-devices
   (list (mapped-device
          (source (uuid "3e8d8072-ee94-407a-9d04-f2f62e1b3743"))
          (target "cryptroot")
          (type luks-device-mapping))))

  (file-systems (cons* (file-system (device (uuid "FF1E-537C" 'fat32))
                                    (mount-point "/boot/efi")
                                    (type "vfat"))
                       (file-system (device "/dev/mapper/cryptroot")
                                    (mount-point "/")
                                    (type "ext4")
                                    (dependencies mapped-devices))
                       %base-file-systems))

  (users (cons (user-account
                (name "jojo")
                (comment "Me")
                (group "users")
                (supplementary-groups '("wheel"
                                        "netdev"
                                        "audio"
                                        "video"))
                (home-directory "/home/jojo"))
               %base-user-accounts))

  ;; This is where we specify system-wide packages.
  (packages (cons* nss-certs		; HTTPS access
                   screen
                   git
                   emacs
                   syncthing
                   gdb
                   file
                   openssh
                   ncurses ;; Supplies terminal commands `reset` and `clear`
                   xdg-utils ;; Supplies xdg-open
                   alsa-utils ;; Supplies amixer
                   pavucontrol
                   emacs-exwm ;; Supplies desktop manager with exwm entry
                   font-dejavu
                   xinput
                   xrandr
                   xbacklight
                   setxkbmap
                   xset
                   xss-lock
                   stalonetray
                   ;; These themes together make networkmanager look all correct.
                   adwaita-icon-theme
                   hicolor-icon-theme
                   icecat
                   lm-sensors
                   python
                   %base-packages))

  (services (cons* (service slim-service-type
                            (slim-configuration
                             (default-user "jojo")
                             (xorg-configuration
                              (xorg-configuration
                               (keyboard-layout keyboard-layout)
                               (server-arguments (cons* "-ardelay" "200"
                                                        "-arinterval" "20"
                                                        %default-xorg-server-arguments))
                               (extra-config (list intel-xorg-conf
                                                   touchpad-xorg-conf
                                                   touchscreen-xorg-conf))))))
                   (service openssh-service-type
                            (openssh-configuration
                             (port-number 22)))
                   (extra-special-file "/usr/bin/sh" (file-append bash "/bin/sh"))
                   (extra-special-file "/bin/bash" (file-append bash "/bin/bash"))
                   (extra-special-file "/usr/bin/bash" (file-append bash "/bin/bash"))
                   (extra-special-file "/usr/bin/python3" (file-append python "/bin/python3"))
                   (extra-special-file "/usr/bin/python" (file-append python "/bin/python3"))
                   (remove (lambda (service)
                             (eq? (service-kind service) gdm-service-type))
                           %desktop-services)))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
