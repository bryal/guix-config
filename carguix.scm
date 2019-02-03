(use-modules (guix packages)
             (guix gexp)
             (guix build download)
             (guix build-system cargo)
             ((guix licenses) #:prefix license:))

(define (cargo-toml-str name version authors)
  (define (lit s) (string-append "\"" s "\""))
  (string-append "[package]\n"
                 "name = " (lit name) "\n"
                 "version = " (lit version) "\n"
                 "authors = [ " (intercalate ", " (map lit authors)) " ]\n"
                 "\n"
                 "[dependencies]\n"))

(define (generate-cargo-toml name version authors)
  `(call-with-output-file "Cargo.toml"
     (lambda (port) (display ,(cargo-toml-str name version authors) port))))

(define (carguix-package #:name name
                         #:version version
                         #:authors authors
                         #:synopsis synopsis
                         #:description description
                         #:home-page home-page
                         #:license license)
  (package (name name)
           (version version)
           (source (local-file "./" "project" #:recursive? #t))
           (build-system cargo-build-system)
           (arguments
            `(#:phases (modify-phases %standard-phases
                                      (add-before 'configure 'generate-cargo-toml
                                                  (lambda _ ,generate-cargo-toml)))))
           (synopsis synopsis)
           (description description)
           (maintainers authors)
           (home-page home-page)
           (license license)))
