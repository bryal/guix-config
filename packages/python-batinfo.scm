(use-modules (gnu)
             (guix packages)
             (guix download)
             (guix build-system python)
             ((guix licenses) #:prefix license:))

(package (name "python-batinfo")
         (version "0.4.2")
         (source (origin (method url-fetch)
                         (uri (pypi-uri "batinfo" version))
                         (sha256 (base32 "1xbjylx5im6y7habkhnx6x8nhvgvpm0d0fylf4pcagimr7pjjzj9"))))
         (build-system python-build-system)
         (arguments
          `(#:phases (modify-phases %standard-phases
                                    (delete 'check))))
         (home-page "https://github.com/nicolargo/batinfo")
         (synopsis "Python battery information library")
         (description "A simple Python module to retrieve battery information on
Linux-based operating system. No ACPI or external software is
needed. Only the Linux kernel and its /sys/class/power_supply
folder.")
         (license license:lgpl3))
