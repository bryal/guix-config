(define-module (matrix)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages emacs-xyz))

(define-public emacs-matrix-client-latest
  (let ((commit "59c792423fd0503f1375f356bec56130e65d3b51"))
    (package
      (inherit emacs-matrix-client)
      (name "emacs-matrix-client-latest")
      (version (git-version "0.0.0" "5" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jgkamat/matrix-client-el.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1gmpw14q9zkvviqpnmvh5vbyxk42038yilxm6b2zqw7mwxfc1xra")))))))
