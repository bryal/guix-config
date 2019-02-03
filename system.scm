;; This is an operating system configuration template
;; for a "desktop" setup with GNOME and Xfce where the
;; root partition is encrypted with LUKS.

(use-modules (gnu)
	     (guix packages)
	     (guix git-download)
	     (guix build-system trivial)
	     ((guix licenses) #:prefix license:)
	     (guix download))
(use-service-modules shepherd networking ssh desktop xorg)
(use-package-modules certs
                     bash
		     screen
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
		     xdisorg)

(define (linux-nonfree-urls version)
  "Return a list of URLs for Linux-Nonfree VERSION."
  (list (string-append
         "https://www.kernel.org/pub/linux/kernel/v4.x/"
         "linux-" version ".tar.xz")))

(define-public linux-firmware-non-free
  (package
    (name "linux-firmware-non-free")
    (version "65b1c68c63f974d72610db38dfae49861117cae2")
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
  (let* ((version "4.18.5"))
    (package
     (inherit linux-libre)
     (name "linux-nonfree")
     (version version)
     (source (origin
              (method url-fetch)
              (uri (linux-nonfree-urls version))
              (sha256
               (base32
                "1ga7ys6s5d9dk1ly9722sbik1y6kbc3w6nw9pw86zpzdh0v0l2gv"))))
     (synopsis "Mainline Linux kernel, nonfree binary blobs included.")
     (description "Linux is a kernel.")
     (license license:gpl2)
     (home-page "http://kernel.org/"))))

(define intel-backlight
  "
Section \"Device\"
        Identifier  \"Intel Graphics\"
        Driver      \"intel\"
        Option      \"Backlight\"  \"intel_backlight\"
EndSection")

(define (fancontrol-shepherd-service config)
  "Return a <shepherd-service> for fancontrol"
  (define fancontrol-command
    `(list (string-append ,lm-sensors "/sbin/fancontrol")))
  (list (shepherd-service (provision '(fancontrol-daemon))
                          (documentation "Fancontrol daemon")
                          (start `(make-forkexec-constructor ,fancontrol-command))
                          (stop `(make-kill-destructor)))))

(define fancontrol-service-type
  (service-type (name 'fancontrol)
                (description "Run the fancontrol daemon")
                (extensions (list (service-extension shepherd-root-service-type
                                                     fancontrol-shepherd-service)))
                (default-value '())))

(define touchpad
  "
Section \"InputClass\"
        Identifier  \"Touchpad\"
        Driver      \"libinput\"
        MatchIsTouchpad \"on\"
        Option \"Tapping\" \"on\"
        Option \"ClickMethod\" \"clickfinger\"
EndSection")

(operating-system
 (host-name "sleipnir")
 (timezone "Europe/Paris")
 (locale "en_US.utf8")

 (kernel linux-nonfree)
 (firmware (cons* linux-firmware-non-free
		  %base-firmware))

 ;; Use the UEFI variant of GRUB with the EFI System
 ;; Partition mounted on /boot/efi.
 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (target "/boot/efi")))

 ;; Specify a mapped device for the encrypted root partition.
 ;; The UUID is that returned by 'cryptsetup luksUUID'.
 (mapped-devices
  (list (mapped-device
         (source (uuid "97e45a43-a827-4633-88ee-dc95bc5a9d6a"))
         (target "my-root")
         (type luks-device-mapping))))

 (file-systems (cons* (file-system (device "/dev/sda1")
				   (mount-point "/boot/efi")
				   (type "vfat"))
		      (file-system (device (file-system-label "my-root"))
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
 (packages (cons* ;; System packages
	    nss-certs		; HTTPS access
	    ;;
	    ;; Important general packages
            screen
	    git
	    emacs
	    syncthing
            gdb
            file
	    ;;
	    ;; GUI. EXWM Window manager
	    emacs-exwm
	    font-dejavu
	    xinput
	    xrandr
	    xbacklight
	    setxkbmap
            xset
	    xss-lock
	    stalonetray
	    ;; pasystray
	    network-manager-applet
	    qsyncthingtray
            adwaita-icon-theme
	    ;;
	    ;; Misc useful packages
	    icecat
            lm-sensors
            python
	    ;;
            %base-packages))

 ;; Add GNOME and/or Xfce---we can choose at the log-in
 ;; screen with F1.  Use the "desktop" services, which
 ;; include the X11 log-in service, networking with
 ;; NetworkManager, and more.
 (services (cons* (service openssh-service-type
                           (openssh-configuration
                            (port-number 22)))
                  (service fancontrol-service-type '())
                  (extra-special-file "/usr/bin/sh" (file-append bash "/bin/sh"))
                  (extra-special-file "/bin/bash" (file-append bash "/bin/bash"))
                  (extra-special-file "/usr/bin/bash" (file-append bash "/bin/bash"))
                  (extra-special-file "/usr/bin/python3" (file-append python "/bin/python3"))
                  (extra-special-file "/usr/bin/python" (file-append python "/bin/python3"))
                  (modify-services %desktop-services
                                   (slim-service-type
                                    config =>
                                    (slim-configuration
                                     (inherit config)
                                     (startx (xorg-start-command
                                              #:configuration-file
                                              (xorg-configuration-file
                                               #:extra-config
                                               (list intel-backlight
                                                     touchpad)))))))))

 ;; Allow resolution of '.local' host names with mDNS.
 (name-service-switch %mdns-host-lookup-nss))
