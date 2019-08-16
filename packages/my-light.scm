(use-modules (gnu)
             (gnu system nss)
             (guix git-download)
             (guix packages)
             (guix build download)
             (guix build-system gnu)
             ((guix licenses) #:prefix license:))
(use-package-modules autotools)

(package
 (name "my-light")
 (version "1.2")
 (source (origin
          (method url-fetch)
          (uri (string-append
                "https://github.com/haikarainen/light/archive/v"
                version ".tar.gz"))
          (sha256
           (base32
            "1gfvsw7gh5pis733l7j54vzp272pvjyzbg8a0pvapfmg0s7mip97"))
          (file-name (string-append name "-" version ".tar.gz"))))
 (build-system gnu-build-system)
 (arguments
  `(#:phases
    (modify-phases %standard-phases
                   (add-after 'patch-source-shebangs 'patch-install-sh
                              (lambda _
                                (patch-shebang "install-sh"))))))
 (native-inputs
  `(("autoconf" ,autoconf)
    ("automake" ,automake)))
 (home-page "https://haikarainen.github.io/light/")
 (synopsis "GNU/Linux application to control backlights")
 (description
  "Light is a program to send commands to screen backlight controllers
under GNU/Linux.  Features include:

@itemize
@item It does not rely on X.
@item Light can automatically figure out the best controller to use, making
full use of underlying hardware.
@item It is possible to set a minimum brightness value, as some controllers
set the screen to be pitch black at a vaĺue of 0 (or higher).
@end itemize

Light is the successor of lightscript.")
 (license license:gpl3+))
