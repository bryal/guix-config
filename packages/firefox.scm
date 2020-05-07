;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (firefox)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (ice-9 match)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libreoffice)  ;for hunspell
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages node)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite))

(define %icecat-build-id "20200106000000") ;must be of the form YYYYMMDDhhmmss

;; Based on the IceCat package in gnuzilla.scm. If something breaks or you want
;; to upgrade, see that file for a ton of comments.
(define-public firefox
  (package
    (name "firefox")
    (version "72.0.2")
    (source
     (origin (method url-fetch)
             (uri (string-append "https://ftp.mozilla.org/pub/firefox/releases/"
                                 version "/source/"
                                 "firefox-" version ".source.tar.xz"))
             (sha256 (base32
                      "12g33fas34fzap57nag4hjm7289z8jnqgxdfazbp4lc8x95j5zbp"))))
    (build-system gnu-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("bzip2" ,bzip2)
       ("cups" ,cups)
       ("dbus-glib" ,dbus-glib)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("gtk+-2" ,gtk+-2)
       ("graphite2" ,graphite2)
       ("pango" ,pango)
       ("freetype" ,freetype)
       ("harfbuzz" ,harfbuzz)
       ("libcanberra" ,libcanberra)
       ("libgnome" ,libgnome)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libogg" ,libogg)
       ("libvorbis" ,libvorbis)
       ("libxft" ,libxft)
       ("libevent" ,libevent)
       ("libxinerama" ,libxinerama)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libxcomposite" ,libxcomposite)
       ("libxt" ,libxt)
       ("libffi" ,libffi)
       ("ffmpeg" ,ffmpeg)
       ("libvpx" ,libvpx)
       ("icu4c" ,icu4c)
       ("pixman" ,pixman)
       ("pulseaudio" ,pulseaudio)
       ("mesa" ,mesa)
       ("mit-krb5" ,mit-krb5)
       ("shared-mime-info" ,shared-mime-info)
       ("sqlite" ,sqlite)
       ("startup-notification" ,startup-notification)
       ("unzip" ,unzip)
       ("zip" ,zip)
       ("zlib" ,zlib)))
    (native-inputs
     `(("patch" ,(canonical-package patch))
       ("rust" ,rust-1.39)
       ("cargo" ,rust-1.39 "cargo")
       ("rust-cbindgen" ,rust-cbindgen)
       ("llvm" ,llvm)
       ("clang" ,clang)
       ("perl" ,perl)
       ("node" ,node)
       ("python" ,python)
       ("python-2" ,python-2)
       ("python2-pysqlite" ,python2-pysqlite)
       ("yasm" ,yasm)
       ("nasm" ,nasm)
       ("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf-2.13)
       ("which" ,which)))
    (arguments
     `(#:tests? #f
       #:out-of-source? #t
       #:validate-runpath? #f
       #:configure-flags `("--enable-default-toolkit=cairo-gtk3"
                           "--with-distribution-id=org.gnu"
                           "--with-unsigned-addon-scopes=app"
                           "--enable-startup-notification"
                           "--enable-pulseaudio"
                           "--disable-tests"
                           "--disable-updater"
                           "--disable-crashreporter"
                           "--disable-eme"
                           "--disable-gconf"
                           "--disable-debug"
                           "--disable-debug-symbols"
                           ,(string-append "--with-clang-path="
                                           (assoc-ref %build-inputs "clang")
                                           "/bin/clang")
                           ,(string-append "--with-libclang-path="
                                           (assoc-ref %build-inputs "clang")
                                           "/lib")
                           "--enable-official-branding"
                           "--with-system-zlib"
                           "--with-system-bz2"
                           "--with-system-jpeg"
                           "--with-system-icu"
                           "--enable-system-pixman"
                           "--enable-system-ffi")
       #:imported-modules ,%cargo-utils-modules ;for `generate-all-checksums'
       #:modules ((ice-9 ftw)
                  (ice-9 rdelim)
                  (ice-9 match)
                  (srfi srfi-34)
                  (srfi srfi-35)
                  ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'apply-guix-specific-patches
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             (let ((patch (string-append (assoc-ref (or native-inputs inputs)
                                                    "patch")
                                         "/bin/patch")))
               (for-each (match-lambda
                           ((label . file)
                            (when (and (string-prefix? "icecat-" label)
                                       (string-suffix? ".patch" label))
                              (format #t "applying '~a'...~%" file)
                              (invoke patch "--force" "--no-backup-if-mismatch"
                                      "-p1" "--input" file))))
                         (or native-inputs inputs)))
             #t))
         (add-after 'apply-guix-specific-patches 'remove-bundled-libraries
           (lambda _
             (for-each (lambda (file)
                         (format #t "deleting '~a'...~%" file)
                         (delete-file-recursively file))
                       '("modules/freetype2"
                         "modules/zlib"
                         "js/src/ctypes/libffi"))
             #t))
         (add-after 'remove-bundled-libraries 'link-libxul-with-libraries
           (lambda _
             (substitute* "toolkit/library/moz.build"
               (("^# This library needs to be last" all)
                (string-append "OS_LIBS += [
    'GL', 'gnome-2', 'canberra', 'Xss', 'cups', 'gssapi_krb5',
    'avcodec', 'avutil', 'pulse' ]\n\n"
                               all)))
             #t))
         (add-after 'link-libxul-with-libraries 'fix-ffmpeg-runtime-linker
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "dom/media/platforms/ffmpeg/FFmpegRuntimeLinker.cpp"
               (("libavcodec\\.so")
                (string-append (assoc-ref inputs "ffmpeg") "/lib/libavcodec.so")))
             #t))
         (replace 'bootstrap
           (lambda _
             (invoke "sh" "-c" "autoconf old-configure.in > old-configure")
             (invoke "touch" "configure")))
         (add-after 'patch-source-shebangs 'patch-cargo-checksums
           (lambda _
             (use-modules (guix build cargo-utils))
             (let ((null-hash "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
               (substitute* '("Cargo.lock" "gfx/wr/Cargo.lock")
                 (("(\"checksum .* = )\".*\"" all name)
                  (string-append name "\"" null-hash "\"")))
               (generate-all-checksums "third_party/rust"))
             #t))
         (add-before 'configure 'augment-CPLUS_INCLUDE_PATH
           (lambda* (#:key build inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs "gcc")))
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-append gcc "/include/c++" ":"
                                      gcc "/include/c++/" build)))
             #t))
         (replace 'configure
           (lambda* (#:key outputs configure-flags #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bash (which "bash"))
                    (abs-srcdir (getcwd))
                    (srcdir (string-append "../" (basename abs-srcdir)))
                    (flags `(,(string-append "--prefix=" out)
                             ,@configure-flags)))
               (setenv "SHELL" bash)
               (setenv "CONFIG_SHELL" bash)
               (setenv "AUTOCONF" (which "autoconf"))
               (setenv "CC" "gcc")
               (setenv "MOZ_BUILD_DATE" ,%icecat-build-id)
               (mkdir "../build")
               (chdir "../build")
               (format #t "build directory: ~s~%" (getcwd))
               (format #t "configure flags: ~s~%" flags)
               (apply invoke bash
                      (string-append srcdir "/configure")
                      flags))))
         (replace 'build
           (lambda args
             (let ((build (assoc-ref %standard-phases 'build)))
               (let retry ((remaining-attempts 5))
                 (if (= remaining-attempts 1)
                     (apply build args)
                     (guard (c ((invoke-error? c)
                                (format #t "~%Retrying build! (~a attempts remaining)~%~%"
                                        (- remaining-attempts 1))
                                (force-output)
                                (retry (- remaining-attempts 1))))
                       (apply build args)))))))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (gtk (assoc-ref inputs "gtk+"))
                    (gtk-share (string-append gtk "/share"))
                    (pulseaudio (assoc-ref inputs "pulseaudio"))
                    (pulseaudio-lib (string-append pulseaudio "/lib")))
               (wrap-program (car (find-files lib "^firefox$"))
                 `("XDG_DATA_DIRS" prefix (,gtk-share))
                 `("LD_LIBRARY_PATH" prefix (,pulseaudio-lib)))
               #t))))))
    (home-page "https://www.gnu.org/software/gnuzilla/")
    (synopsis "Entirely free browser derived from Mozilla Firefox")
    (description "Firefix adated for Guix, based on the IceCat package.")
    (license license:mpl2.0)
    ;; (properties
    ;;  `((ftp-directory . "/gnu/gnuzilla")
    ;;    (cpe-name . "firefox_esr")
    ;;    (cpe-version . ,(first (string-split version #\-)))))
    ))
