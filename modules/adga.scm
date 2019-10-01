(define-module agda
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages agda))

(define-public agda26
  (package
   (inherit agda)
   (version "2.6.0.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/Agda/Agda-"
           version ".tar.gz"))
     (sha256
      (base32
       "07wvawpfjhx3gw2w53v27ncv1bl0kkx08wkm6wzxldbslkcasign"))))))
