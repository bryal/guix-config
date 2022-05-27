(use-modules (gnu)
             (guix packages)
             (guix git-download)
             ;; (guix build-system meson)
             (gnu packages build-tools)
             (gnu packages linux))
(use-package-modules linux)

(package
  (inherit pipewire-0.3)
  (name "pipewire")
  (version "0.3.35")
  (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/PipeWire/pipewire")
                  (commit version)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1yqvr8nn9c24wf4di92svw7hfp0j62rdq6vkrbra5q0wvqfz8qbh"))))
  (arguments
   (cons*
    #:meson
    meson-next
    (package-arguments pipewire-0.3))))
