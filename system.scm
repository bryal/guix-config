;; -*- eval: (guix-devel-mode) -*-

(use-modules (gnu)
	     (guix packages)
	     (guix git-download)
	     (guix build-system trivial)
	     (guix build-system font)
	     ((guix licenses) #:prefix license:)
	     (guix download)
             (srfi srfi-1)
             (firefox))
(use-service-modules shepherd networking ssh desktop xorg sysctl)
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
                     gdb
                     file
                     python
                     python-xyz
		     xdisorg
                     freedesktop
                     pulseaudio
                     ssh
                     games)

(define intel-xorg-conf
  "
Section \"Device\"
        Identifier  \"Intel Graphics\"
        Driver      \"intel\"
        Option      \"Backlight\"  \"intel_backlight\"
        Option      \"AccelMethod\" \"sna\"
        Option      \"DRI\" \"3\"
        Option      \"TearFree\"  \"true\"
EndSection")

(define touchpad-xorg-conf
  "
Section \"InputClass\"
        Identifier  \"Touchpad\"
        Driver      \"libinput\"
        MatchIsTouchpad \"on\"
        Option \"AccelSpeed\" \"0.7\"
        Option \"ClickMethod\" \"clickfinger\"
EndSection")

;; This doesn't seem to be exported by the xorg module, so I have to define it
;; manually.
(define %default-xorg-server-arguments
  ;; Default command-line arguments for X.
  '("-nolisten" "tcp"))

(define (linux-nonfree-urls version)
  "Return a list of URLs for Linux-Nonfree VERSION."
  (list (string-append
         "https://www.kernel.org/pub/linux/kernel/v5.x/"
         "linux-" version ".tar.xz")))

(define-public linux-firmware-non-free
  (package
    (name "linux-firmware-non-free")
    (version "b2cad6a2d733d9b10d25a31874a51d96908d6e89")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.kernel.org/pub/scm/linux/kernel/git/firmware/linux-firmware.git")
                    (commit version)))
              (sha256
               (base32
                "19l5gkhygxm17biz0fs9jqm8fyjb5h9901xkpij0hxabnf7l0qzr"))))
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
  (let* ((version "5.6.9"))
    (package
      (inherit linux-libre)
      (name "linux-nonfree")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (linux-nonfree-urls version))
                (sha256
                 (base32
                  "14w09iqdy65zkbymqiybvm7z92gmf5bz0wgd5r79r1b6ccz7347a"))))
      (synopsis "Mainline Linux kernel, nonfree binary blobs included.")
      (description "Linux is a kernel.")
      (license license:gpl2)
      (home-page "http://kernel.org/"))))

(define-public font-iosevka-ss09
  (package
    (inherit font-iosevka)
    (name "font-iosevka-ss09")
    (version "3.0.0-rc.8")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/ttc-iosevka-" version ".zip"))
       (sha256 (base32 "1hzv37g6791k4rgdldn0rzj65yajvvjbr95rql5b35mcaizvwp5m"))))))

(operating-system
  (host-name "astoria")
  (timezone "Europe/Stockholm")
  (locale "en_US.utf8")
  (keyboard-layout
   (keyboard-layout "us" "altgr-intl" #:options '("ctrl:nocaps")))

  (kernel linux-nonfree)
  (firmware (cons* linux-firmware-non-free %base-firmware))

  ;; Use the UEFI variant of GRUB with the EFI System
  ;; Partition mounted on /boot/efi.
  (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader)
               (target "/boot/efi")
               (timeout 1.2)
               ;; 0 -> Current guix, 1..<n-1 -> menu-entries, n-1 -> Previous guix
               (default-entry 0)
               (keyboard-layout keyboard-layout)))

  (file-systems
   (cons* (file-system
            (mount-point "/boot/efi")
            (device "/dev/nvme0n1p1")
            (type "vfat"))
          (file-system
            (mount-point "/")
            (device "/dev/nvme0n1p2")
            (type "ext4"))
          %base-file-systems))

  (swap-devices '("/dev/nvme0n1p3"))

  (users (cons (user-account
                (name "jojo")
                (group "users")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video"))
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
                   font-iosevka-ss09
                   xinput
                   xrandr
                   xbacklight
                   setxkbmap
                   xset
                   xss-lock
                   ;; These themes together make networkmanager look all correct.
                   adwaita-icon-theme
                   hicolor-icon-theme
                   firefox
                   lm-sensors
                   python
                   %base-packages))

  (services
   (cons* (service slim-service-type
                   (slim-configuration
                    (default-user "jojo")
                    (xorg-configuration
                     (xorg-configuration
                      (keyboard-layout keyboard-layout)
                      (server-arguments (cons* "-ardelay" "210"
                                               "-arinterval" "25"
                                               %default-xorg-server-arguments))
                      (extra-config (list intel-xorg-conf
                                          touchpad-xorg-conf))))))
          (service openssh-service-type
                   (openssh-configuration
                    (port-number 22)))
          (service sysctl-service-type
                   (sysctl-configuration
                    (settings '(("fs.inotify.max_user_watches" . "256000")))))
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
