;; -*- eval: (guix-devel-mode) -*-

(use-modules (gnu)
	     (guix packages)
	     (guix git-download)
	     (guix build-system trivial)
	     (guix build-system font)
	     ((guix licenses) #:prefix license:)
	     (guix download)
             (srfi srfi-1))
(use-service-modules shepherd networking dns ssh desktop xorg sysctl web certbot syncthing audio file-sharing)
(use-package-modules certs bash screen ncurses linux version-control emacs
                     emacs-xyz gnome syncthing sync xorg fonts gdb file python
                     python-xyz xdisorg freedesktop pulseaudio ssh games
                     gnuzilla mail)

;; This doesn't seem to be exported by the xorg module, so I have to define it
;; manually.
(define %default-xorg-server-arguments
  ;; Default command-line arguments for X.
  '("-nolisten" "tcp"))

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

(define (simple-https-website domain path)
  (list (httpd-virtualhost
         "*:443"
         (list "
                ServerName " domain "
                DocumentRoot " path "
                <Directory " path "/>
                    Options +FollowSymLinks +Indexes
                    AllowOverride None
                </Directory>
                SSLEngine on
                SSLCertificateFile /etc/letsencrypt/live/" domain "/cert.pem
                SSLCertificateKeyFile /etc/letsencrypt/live/" domain "/privkey.pem
                SSLCertificateChainFile /etc/letsencrypt/live/" domain "/fullchain.pem\n"))))

(operating-system
  (host-name "odin")
  (timezone "Europe/Stockholm")
  (locale "en_US.utf8")
  (keyboard-layout
   (keyboard-layout "us" "altgr-intl" #:options '("ctrl:nocaps")))

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "/dev/nvme0n1")
               (timeout 1.2)
               ;; 0 -> Current guix, 1..<n-1 -> menu-entries, n-1 -> Previous guix
               (default-entry 0)
               (keyboard-layout keyboard-layout)))

  (file-systems
   (cons* (file-system
            (mount-point "/")
            (device
             (uuid "5efaa7b9-f075-4760-a698-0a3f861dad39"
                   'ext4))
            (type "ext4"))
          %base-file-systems))

  (swap-devices (list (file-system-label "SWAP")))

  (users (cons (user-account
                (name "jojo")
                (group "users")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video"))
                (home-directory "/home/jojo"))
               %base-user-accounts))

  ;; This is where we specify system-wide packages.
  (packages (cons* nss-certs            ; HTTPS access
                   screen git emacs syncthing gdb file openssh sendmail net-tools
                   ncurses              ; Supplies terminal commands `reset` and `clear`
                   xdg-utils            ; Supplies xdg-open
                   alsa-utils           ; Supplies amixer
                   pulseaudio paprefs pavucontrol font-dejavu font-iosevka-ss09
                   xinput xrandr xbacklight setxkbmap xset xss-lock
                   ;; These themes together make networkmanager look all correct.
                   adwaita-icon-theme hicolor-icon-theme
                   icecat lm-sensors python
                   %base-packages))

  (services
   (cons* (service syncthing-service-type
                   (syncthing-configuration (user "jojo")))
          (service transmission-daemon-service-type
                   (transmission-daemon-configuration
                    (rpc-authentication-required? #t)
                    (rpc-username "transmission")
                    (rpc-password "{adb10747bd8da7f0a7ec25c3fddb44bd710416ffgpufxBHc")
                    (rpc-whitelist-enabled? #t)
                    (rpc-whitelist '("::1" "127.0.0.1" "192.168.0.*"))
                    (download-queue-enabled? #t)
                    (download-queue-size 8)
                    (seed-queue-enabled? #f)
                    (speed-limit-down-enabled? #t)
                    (speed-limit-down (* 72 1024)) ; 72 Mbyte/s = 576 Mbit/s
                    (speed-limit-up-enabled? #t)
                    (speed-limit-up (* 32 1024)) ; 32 Mbyte/s = 256 Mbit/s
                    (peer-limit-global 500)
                    (peer-limit-per-torrent 100)
                    (ratio-limit-enabled? #t)
                    (ratio-limit 8.0)))
          ;; Partly exists just so that Pulseaudio is always on for user
          (service mpd-service-type (mpd-configuration (user "jojo")
                                                       (port "6600")
                                                       (address "192.168.0.41")
                                                       (music-dir "~/Transcoded")
                                                       (outputs (list (mpd-output (always-on? #t))))))
          (simple-service 'jo.zone httpd-service-type
                          (simple-https-website "jo.zone"
                                                "/home/jojo/Syncthing/jo-zone"))
          (simple-service 'carth.pink httpd-service-type
                          (simple-https-website "carth.pink"
                                                "/home/jojo/Hack/carth-website"))
          (service httpd-service-type
                   (httpd-configuration
                    (config
                     (httpd-config-file
                      (listen '("0.0.0.0:443"))
                      (modules (cons* (httpd-module (name "ssl_module")
                                                    (file "modules/mod_ssl.so"))
                                      %default-httpd-modules))))))
          (service certbot-service-type
                   (certbot-configuration
                    (email "jo@jo.zone")
                    (webroot "/srv/http")
                    (certificates
                     (list (certificate-configuration (domains '("jo.zone")))
                           (certificate-configuration (domains '("carth.pink")))))))
          (service ddclient-service-type
                   (ddclient-configuration
                    (mail "jo@jo.zone")
                    (mail-failure "jo@jo.zone")
                    ;; Additional conf in /etc/ddclient/secrets.conf. See
                    ;; ~/my-conf/SETUP.org for more info.
                    ))
          (service openssh-service-type
                   (openssh-configuration
                    (port-number 22)
                    (x11-forwarding? #t)
                    (extra-content "\
ListenAddress 0.0.0.0
ListenAddress [::]:443")))
          (service sysctl-service-type
                   (sysctl-configuration
                    (settings '(("fs.inotify.max_user_watches" . "256000")))))
          (extra-special-file "/usr/bin/sh" (file-append bash "/bin/sh"))
          (extra-special-file "/bin/bash" (file-append bash "/bin/bash"))
          (extra-special-file "/usr/bin/bash" (file-append bash "/bin/bash"))
          (extra-special-file "/usr/bin/python3" (file-append python "/bin/python3"))
          (extra-special-file "/usr/bin/python" (file-append python "/bin/python3"))
          (remove (lambda (service)
                    (or (eq? (service-kind service) gdm-service-type)
                        (eq? (service-kind service) sysctl-service-type)))
                  %desktop-services)))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
