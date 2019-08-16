(use-modules (gnu)
             (guix packages)
             (guix git-download)
             (guix build-system gnu)
             ((guix licenses) #:prefix license:))
(use-package-modules xorg fontutils perl acl)

(package (name "lemonbar-sm")
         (version "ba126f832277b00c2f8c8e0850434542b8452d33")
         (source (origin (method git-fetch)
                         (uri (git-reference (url "https://github.com/osense/bar")
                                             (commit version)))
                         (sha256 (base32 "1nb71qfh3m7y6qa2n56nylrq7p6arp7vhrjvmhs1z0lh57wn3vf5"))))
         (build-system gnu-build-system)
         (arguments
          '(#:phases (modify-phases %standard-phases
                                    ;;(delete 'unpack)
                                    (delete 'configure)
                                    (delete 'check))
            #:make-flags (list "CC=gcc"
                               (string-append "PREFIX=" (assoc-ref %outputs "out"))
                               (string-append "CFLAGS="
                                              " -Wall" " -std=c99" " -Os"
                                              " -DVERSION=\"\\\"$(VERSION)\\\"\""
                                              " -I" (assoc-ref %build-inputs "freetype") "/include/freetype2"))))
         (inputs `(("libxcb" ,libxcb)
                   ("libxft" ,libxft)
                   ("libx11" ,libx11)
                   ("freetype" ,freetype)
                   ("fontconfig" ,fontconfig)
                   ("perl" ,perl)
                   ("acl" ,acl)))
         (home-page "https://github.com/osense/bar")
         (synopsis "Lightweight status bar. Fork of lemonbar, with xft support and the ability so select monitor.")
         (description "*lemonbar* (formerly known as *bar*) is a lightweight bar entirely based on XCB. Provides full UTF-8 support, basic formatting, RandR and Xinerama support and EWMH compliance wihtout wasting your precious memory.")
         (license license:expat))
