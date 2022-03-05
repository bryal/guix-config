;; -*- eval: (guix-devel-mode) -*-

(use-modules (gnu)
	     (guix packages)
	     (guix git-download)
	     (guix build-system trivial)
	     (guix build-system font)
	     ((guix licenses) #:prefix license:)
	     (guix download)
             (srfi srfi-1))
(use-service-modules shepherd networking dns sddm ssh desktop xorg sysctl web
                     certbot syncthing audio file-sharing mcron sound)
(use-package-modules certs bash screen ncurses linux version-control emacs
                     emacs-xyz gnome syncthing sync xorg fonts gdb file python
                     python-xyz xdisorg freedesktop pulseaudio ssh games
                     gnuzilla mail rsync backup)

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
               (targets '("/dev/nvme0n1"))
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
          (file-system (mount-point "/mnt/backups")  (device (file-system-label "SLOWSTORE"))  (type "ext4"))
          (file-system (mount-point "/mnt/faststore")  (device (file-system-label "FASTSTORE"))  (type "ext4"))
          (file-system (mount-point "/mnt/smallstore") (device (file-system-label "SMALLSTORE")) (type "ext4"))
          (file-system (mount-point "/mnt/bigstore")   (device (file-system-label "BIGSTORE"))   (type "ext4"))
          %base-file-systems))

  (swap-devices (list (swap-space (target (file-system-label "SWAP")))))

  (users (cons (user-account
                (name "jojo")
                (group "users")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video" "transmission"))
                (home-directory "/home/jojo"))
               %base-user-accounts))

  ;; This is where we specify system-wide packages.
  (packages (cons* nss-certs            ; HTTPS access
                   screen git emacs syncthing gdb file openssh sendmail net-tools rsync
                   rsnapshot            ; Backup
                   ncurses              ; Supplies terminal commands `reset` and `clear`
                   xdg-utils            ; Supplies xdg-open
                   alsa-utils           ; Supplies amixer
                   emacs-exwm
                   pulseaudio paprefs pavucontrol font-dejavu font-iosevka-ss09
                   xinput xrandr xbacklight setxkbmap xset xss-lock
                   ;; These themes together make networkmanager look all correct.
                   adwaita-icon-theme hicolor-icon-theme
                   icecat lm-sensors python
                   %base-packages))

  (services
   (cons* (service sddm-service-type
                   (sddm-configuration
                    (display-server "x11")
                    (auto-login-user "jojo")
                    (auto-login-session "exwm.desktop")
                    (relogin? #t)
                    (xorg-configuration
                     (xorg-configuration
                      (keyboard-layout keyboard-layout)
                      (server-arguments (cons* "-ardelay" "210"
                                               "-arinterval" "25"
                                               %default-xorg-server-arguments))))))
          (service syncthing-service-type
                   (syncthing-configuration (user "jojo")))
          (service transmission-daemon-service-type
                   (transmission-daemon-configuration
                    (rpc-authentication-required? #t)
                    (rpc-username "transmission")
                    (rpc-password "{adb10747bd8da7f0a7ec25c3fddb44bd710416ffgpufxBHc")
                    (rpc-whitelist-enabled? #t)
                    (rpc-whitelist '("::1" "127.0.0.1" "192.168.0.*"))
                    (umask #o002)
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
          (simple-service 'synapse httpd-service-type
                          (list (httpd-virtualhost
                                 "*:8448"
                                 (list "
                                        SSLEngine on
                                        ServerName jo.zone

                                        SSLCertificateFile /etc/letsencrypt/live/jo.zone/cert.pem
                                        SSLCertificateKeyFile /etc/letsencrypt/live/jo.zone/privkey.pem
                                        SSLCertificateChainFile /etc/letsencrypt/live/jo.zone/fullchain.pem

                                        RequestHeader set \"X-Forwarded-Proto\" expr=%{REQUEST_SCHEME}
                                        AllowEncodedSlashes NoDecode
                                        ProxyPass / http://127.0.0.1:8008/ nocanon
                                        ProxyPassReverse / http://127.0.0.1:8008/\n"))))
          (simple-service 'me-mess.chat httpd-service-type
                          (list (httpd-virtualhost
                                 "*:443"
                                 (list "SSLEngine on
                                        ServerName me-mess.chat
                                        SSLCertificateFile /etc/letsencrypt/live/me-mess.chat/cert.pem
                                        SSLCertificateKeyFile /etc/letsencrypt/live/me-mess.chat/privkey.pem
                                        SSLCertificateChainFile /etc/letsencrypt/live/me-mess.chat/fullchain.pem
                                        RequestHeader set \"X-Forwarded-Proto\" expr=%{REQUEST_SCHEME}
                                        AllowEncodedSlashes NoDecode
                                        ProxyPreserveHost on
                                        ProxyPass / http://127.0.0.1:8008/ nocanon
                                        ProxyPassReverse / http://127.0.0.1:8008/\n"))))
          (simple-service 'fuckingshit.show httpd-service-type
                          (simple-https-website "fuckingshit.show"
                                                "/home/jojo/Hack/fuckingshit-show"))
          (simple-service 'jo.zone httpd-service-type
                          (simple-https-website "jo.zone"
                                                "/home/jojo/Hack/jo-zone"))
          (simple-service 'carth.pink httpd-service-type
                          (simple-https-website "carth.pink"
                                                "/home/jojo/Hack/carth-website"))
          (service httpd-service-type
                   (httpd-configuration
                    (config
                     (httpd-config-file
                      (listen '("0.0.0.0:443"
                                "0.0.0.0:8448"))
                      (modules (cons* (httpd-module (name "ssl_module") (file "modules/mod_ssl.so"))
                                      (httpd-module (name "proxy_module") (file "modules/mod_proxy.so"))
                                      (httpd-module (name "proxy_http_module") (file "modules/mod_proxy_http.so"))
                                      %default-httpd-modules))))))
          (service certbot-service-type
                   (certbot-configuration
                    (email "jo@jo.zone")
                    (webroot "/srv/http")
                    (certificates
                     (list (certificate-configuration (domains '("me-mess.chat")))
                           (certificate-configuration (domains '("jo.zone")))
                           (certificate-configuration (domains '("carth.pink")))
                           (certificate-configuration (domains '("fuckingshit.show")))))))
          (service openssh-service-type
                   (openssh-configuration
                    (port-number 22)
                    (password-authentication? #f)
                    (public-key-authentication? #t)
                    (x11-forwarding? #t)
                    (extra-content "\
ListenAddress 0.0.0.0
ListenAddress [::]")))
          (service sysctl-service-type
                   (sysctl-configuration
                    (settings '(("fs.inotify.max_user_watches" . "256000")))))
          (simple-service 'my-cron-jobs mcron-service-type
                          (list #~(job "00 *  *   *   *"  "rsnapshot -c /etc/rsnapshot.conf alpha")
                                #~(job "00 05 *   *   *"  "rsnapshot -c /etc/rsnapshot.conf beta")
                                #~(job "00 04 *   *   1"  "rsnapshot -c /etc/rsnapshot.conf gamma")
                                #~(job "00 03 1   *   *"  "rsnapshot -c /etc/rsnapshot.conf delta")
                                #~(job "00 02 *   */6 *"  "rsnapshot -c /etc/rsnapshot.conf epsilon")))
          (extra-special-file "/usr/bin/sh" (file-append bash "/bin/sh"))
          (extra-special-file "/bin/bash" (file-append bash "/bin/bash"))
          (extra-special-file "/usr/bin/bash" (file-append bash "/bin/bash"))
          (extra-special-file "/usr/bin/python3" (file-append python "/bin/python3"))
          (extra-special-file "/usr/bin/python" (file-append python "/bin/python3"))
          (remove (lambda (service)
                    (or (eq? (service-kind service) gdm-service-type)
                        (eq? (service-kind service) sysctl-service-type)
                        ))
                  %desktop-services)))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
