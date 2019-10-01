(define-module (stack)
  #:use-module (gnu)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system haskell)
  #:use-module (guix licenses))
(use-package-modules haskell haskell-web haskell-crypto haskell-check)

(define-public ghc-githash
  (package
    (name "ghc-githash")
    (version "0.1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/githash/githash-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0vpwzbhnr0xwc7vkg3l5qy4awgsr1fkxj58lz6m56jayaad6hn7a"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (home-page
     "https://github.com/snoyberg/githash#readme")
    (synopsis
     "Compile git revision info into Haskell projects")
    (description
     "Please see the README and documentation at <https://www.stackage.org/package/githash>")
    (license bsd-3)))

(define-public ghc-optparse-simple
  (package
    (name "ghc-optparse-simple")
    (version "0.1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/optparse-simple/optparse-simple-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1r00hkri42vyx552l8hcd1779fxiyl9w4k0pql915zsprirn8w82"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-githash" ,ghc-githash)
        ("ghc-optparse-applicative"
         ,ghc-optparse-applicative)))
    (arguments `(#:tests? #f))
    (home-page
      "https://github.com/fpco/optparse-simple#readme")
    (synopsis
      "Simple interface to optparse-applicative")
    (description
      "Please see the README at <https://www.stackage.org/package/optparse-simple>")
    (license bsd-3)))

(define-public ghc-bindings-uname
  (package
    (name "ghc-bindings-uname")
    (version "0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/bindings-uname/bindings-uname-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1lsw4dh5vgmfvrx62ns5kmngzlmjzbxkx43x5i2k5qlmzp1pa3hk"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (home-page
      "http://hackage.haskell.org/package/bindings-uname")
    (synopsis "Low-level binding to POSIX uname(3)")
    (description
      "This is a low-level binding to POSIX uname(3) function. Perhaps it shoule be part of unix package.")
    (license public-domain)))

(define-public ghc-bitarray
  (package
    (name "ghc-bitarray")
    (version "0.0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/bitarray/bitarray-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "00nqd62cbh42qqqvcl6iv1i9kbv0f0mkiygv4j70wfh5cl86yzxj"))))
    (build-system haskell-build-system)
    (arguments
      `(#:tests?
        #f
        #:cabal-revision
        ("1"
         "10fk92v9afjqk43zi621jxl0n8kci0xjj32lz3vqa9xbh67zjz45")))
    (home-page "http://code.haskell.org/~bkomuves/")
    (synopsis "Mutable and immutable bit arrays")
    (description "Mutable and immutable bit arrays.")
    (license bsd-3)))

(define-public ghc-unicode-transforms
  (package
    (name "ghc-unicode-transforms")
    (version "0.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/unicode-transforms/unicode-transforms-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1akscvyssif4hki3g6hy0jmjyr8cqly1whzvzj0km2b3qh0x09l3"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-bitarray" ,ghc-bitarray)))
    (arguments `(#:tests? #f))
    (home-page
      "http://github.com/harendra-kumar/unicode-transforms")
    (synopsis "Unicode normalization")
    (description
      "Fast Unicode 12.1.0 normalization in Haskell (NFC, NFKC, NFD, NFKD).")
    (license bsd-3)))

(define-public ghc-text-metrics
  (package
    (name "ghc-text-metrics")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/text-metrics/text-metrics-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "18mzxwkdvjp31r720ai9bnxr638qq8x3a2v408bz0d8f0rsayx1q"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-vector" ,ghc-vector)))
    (arguments
      `(#:tests?
        #f
        #:cabal-revision
        ("4"
         "017drxq9x56b345d8w5m8xdsi1zzs0z16pbdx8j35cd1lsnh3kf1")))
    (home-page
      "https://github.com/mrkkrp/text-metrics")
    (synopsis
      "Calculate various string metrics efficiently")
    (description
      "Calculate various string metrics efficiently.")
    (license bsd-3)))

(define-public ghc-store-core
  (package
    (name "ghc-store-core")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/store-core/store-core-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1489ydwmq3vd9lz193m5w277wvb9g74ssc1ncfjdry2g0y6czbjv"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-primitive" ,ghc-primitive)))
    (arguments `(#:tests? #f))
    (home-page
      "https://github.com/fpco/store#readme")
    (synopsis
      "Fast and lightweight binary serialization")
    (description "")
    (license expat)))

(define-public ghc-hspec-smallcheck
  (package
    (name "ghc-hspec-smallcheck")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/hspec-smallcheck/hspec-smallcheck-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "06c1ym793zkdwi4bxk5f4l7m1n1bg5jmnm0p68q2pa9rlhk1lc4s"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-hunit" ,ghc-hunit)
        ("ghc-call-stack" ,ghc-call-stack)
        ("ghc-hspec-core" ,ghc-hspec-core)
        ("ghc-smallcheck" ,ghc-smallcheck)))
    (arguments `(#:tests? #f))
    (home-page "http://hspec.github.io/")
    (synopsis
      "SmallCheck support for the Hspec testing framework")
    (description
      "SmallCheck support for the Hspec testing framework")
    (license expat)))

(define-public ghc-th-utilities
  (package
    (name "ghc-th-utilities")
    (version "0.2.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/th-utilities/th-utilities-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0bl4j81k7szn0lza8rnn1db6glc57dsn63ni0hwbwr3kxa3pb4x4"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-primitive" ,ghc-primitive)
        ("ghc-syb" ,ghc-syb)
        ("ghc-th-orphans" ,ghc-th-orphans)))
    (arguments `(#:tests? #f))
    (home-page
      "https://github.com/fpco/th-utilities#readme")
    (synopsis
      "Collection of useful functions for use with Template Haskell")
    (description "")
    (license expat)))

(define-public ghc-store
  (package
    (name "ghc-store")
    (version "0.5.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/store/store-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1abwlcj0z17hj5h94cbg1sgqfdsdgjhgfgd2aaspsn4zdfk5bjc5"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-async" ,ghc-async)
       ("ghc-base-orphans" ,ghc-base-orphans)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-bifunctors" ,ghc-bifunctors)
       ("ghc-contravariant" ,ghc-contravariant)
       ("ghc-cryptohash" ,ghc-cryptohash)
       ("ghc-free" ,ghc-free)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-hspec-smallcheck" ,ghc-hspec-smallcheck)
       ("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-mono-traversable" ,ghc-mono-traversable)
       ("ghc-network" ,ghc-network)
       ("ghc-primitive" ,ghc-primitive)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-safe" ,ghc-safe)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-smallcheck" ,ghc-smallcheck)
       ("ghc-store-core" ,ghc-store-core)
       ("ghc-syb" ,ghc-syb)
       ("ghc-th-lift" ,ghc-th-lift)
       ("ghc-th-lift-instances" ,ghc-th-lift-instances)
       ("ghc-th-orphans" ,ghc-th-orphans)
       ("ghc-th-reify-many" ,ghc-th-reify-many)
       ("ghc-th-utilities" ,ghc-th-utilities)
       ("ghc-unordered-containers"
        ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)
       ("ghc-void" ,ghc-void)))
    (arguments `(#:tests? #f))
    (home-page
     "https://github.com/fpco/store#readme")
    (synopsis "Fast binary serialization")
    (description "")
    (license expat)))

(define-public ghc-retry
  (package
    (name "ghc-retry")
    (version "0.7.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/retry/retry-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0v6irf01xykhv0mwr1k5i08jn77irqbz8h116j8p435d11xc5jrw"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-data-default-class"
        ,ghc-data-default-class)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-random" ,ghc-random)))
    (arguments `(#:tests? #f))
    (home-page "http://github.com/Soostone/retry")
    (synopsis
     "Retry combinators for monadic actions that may fail")
    (description
     "This package exposes combinators that can wrap arbitrary monadic actions. They run the action and potentially retry running it with some configurable delay for a configurable number of times. The purpose is to make it easier to work with IO and especially network I
O actions that often experience temporary failure and warrant retrying of the original action. For example, a database query may time out for a while, in which case we should hang back for a bit and retry the query instead of simply raising an exception.")
    (license bsd-3)))

(define-public ghc-regex-applicative-text
  (package
    (name "ghc-regex-applicative-text")
    (version "0.1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/regex-applicative-text/regex-applicative-text-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1ng2qhk4mvpzl8fx91ig7ldv09v9aqdsvn6yl9yjapc6h0ghb4xh"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-regex-applicative" ,ghc-regex-applicative)))
    (arguments
      `(#:tests?
        #f
        #:cabal-revision
        ("3"
         "1h911harqgfgkhdr22cndj2fdsl48sqhn8q0akgjngpf3p8z0bvv")))
    (home-page
      "https://github.com/phadej/regex-applicative-text#readme")
    (synopsis "regex-applicative on text")
    (description
      "Wrapped regex-applicative primitives to work with Text")
    (license bsd-3)))

(define-public ghc-project-template
  (package
    (name "ghc-project-template")
    (version "0.2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/project-template/project-template-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1p69ww4rhah2qxragl615wl4a6mk4x9w09am8knmz3s4lxpljlpb"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-base64-bytestring" ,ghc-base64-bytestring)
        ("ghc-conduit" ,ghc-conduit)
        ("ghc-conduit-extra" ,ghc-conduit-extra)
        ("ghc-resourcet" ,ghc-resourcet)))
    (arguments
      `(#:tests?
        #f
        #:cabal-revision
        ("1"
         "0lq3sqnq0nr0gbvgzp0lqdl3j3mqdmdlf8xsw0j3pjh581xj3k0a")))
    (home-page "https://github.com/fpco/haskell-ide")
    (synopsis
      "Specify Haskell project templates and generate files")
    (description
      "See initial blog post for explanation: <http://www.yesodweb.com/blog/2012/09/project-templates>")
    (license bsd-3)))

(define-public ghc-open-browser
  (package
    (name "ghc-open-browser")
    (version "0.2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/open-browser/open-browser-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0rna8ir2cfp8gk0rd2q60an51jxc08lx4gl0liw8wwqgh1ijxv8b"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (home-page
     "https://github.com/rightfold/open-browser")
    (synopsis "Open a web browser from Haskell.")
    (description
     "Open a web browser from Haskell. Currently BSD, Linux, OS X and Windows are supported.")
    (license bsd-3)))

(define-public ghc-megaparsec
  (package
    (name "ghc-megaparsec")
    (version "7.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/megaparsec/megaparsec-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0bqx1icbmk8s7wmbcdzsgnlh607c7kzg8l80cp02dxr5valjxp7j"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-parser-combinators"
        ,ghc-parser-combinators)
       ("ghc-scientific" ,ghc-scientific)))
    (arguments `(#:tests? #f))
    (home-page
     "https://github.com/mrkkrp/megaparsec")
    (synopsis "Monadic parser combinators")
    (description
     "This is an industrial-strength monadic parser combinator library. Megaparsec is a feature-rich package that strikes a nice balance between speed, flexibility, and quality of parse errors.")
    (license bsd-2)))

(define-public ghc-neat-interpolation
  (package
    (name "ghc-neat-interpolation")
    (version "0.3.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/neat-interpolation/neat-interpolation-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0lhpjckwhzlvx4cdhrwprwb85vc7hc44ybvk5nswgn7z73cp0wyy"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-megaparsec" ,ghc-megaparsec)
       ("ghc-base-prelude" ,ghc-base-prelude)))
    (arguments `(#:tests? #f))
    (home-page
     "https://github.com/nikita-volkov/neat-interpolation")
    (synopsis
     "A quasiquoter for neat and simple multiline text interpolation")
    (description
     "A quasiquoter for producing Text values with support for a simple interpolation of input values. It removes the excessive indentation from the input and accurately manages the indentation of all lines of the interpolated variables.")
    (license expat)))

(define-public ghc-mustache
  (package
    (name "ghc-mustache")
    (version "2.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/mustache/mustache-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1q3vadcvv2pxg6rpp92jq5zy784jxphdfpf6xn9y6wg9g3jn7201"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-aeson" ,ghc-aeson)
        ("ghc-either" ,ghc-either)
        ("ghc-unordered-containers"
         ,ghc-unordered-containers)
        ("ghc-vector" ,ghc-vector)
        ("ghc-scientific" ,ghc-scientific)
        ("ghc-th-lift" ,ghc-th-lift)
        ("ghc-yaml" ,ghc-yaml)
        ("ghc-cmdargs" ,ghc-cmdargs)))
    (arguments `(#:tests? #f))
    (home-page
      "https://github.com/JustusAdam/mustache")
    (synopsis "A mustache template parser library.")
    (description
      "Allows parsing and rendering template files with mustache markup. See the mustache <http://mustache.github.io/mustache.5.html language reference>. . Implements the mustache spec version 1.1.3. . /Note/: Versions including and beyond 0.4 are compatible with ghc 7.8 again.")
    (license bsd-3)))

(define-public ghc-mintty
  (package
    (name "ghc-mintty")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/mintty/mintty-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1njhz7wjmsk5pbr7gfkl95k50npkmm0iyxp3j93bbsg4rmxzg2kw"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (home-page
      "https://github.com/RyanGlScott/mintty")
    (synopsis
      "A reliable way to detect the presence of a MinTTY console on Windows")
    (description
      "MinTTY is a Windows-specific terminal emulator for the widely used Cygwin and MSYS projects, which provide Unix-like environments for Windows. MinTTY consoles behave differently from native Windows consoles (such as @cmd.exe@ or PowerShell) in many ways, and in some cases, these differences make it necessary to treat MinTTY consoles differently in code. . The @mintty@ library provides a simple way to detect if your code in running in a MinTTY console on Windows. It exports @isMinTTY@, which does the right thing 90% of the time (by checking if standard error is attached to MinTTY), and it also exports @isMinTTYHandle@ for the other 10% of the time (when you want to check is some arbitrary handle is attached to MinTTY). As you might expect, both of these functions will simply return @False@ on any non-Windows operating system.")
    (license bsd-3)))

(define-public ghc-filelock
  (package
    (name "ghc-filelock")
    (version "0.1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/filelock/filelock-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "04qimhz78jjndk686dblkx06l9jscq2q9gyr014a4pbfj4iljgi5"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (home-page
      "http://github.com/takano-akio/filelock")
    (synopsis
      "Portable interface to file locking (flock / LockFileEx)")
    (description
      "This package provides an interface to Windows and Unix file locking functionalities.")
    (license public-domain)))

(define-public ghc-cryptonite-conduit
  (package
    (name "ghc-cryptonite-conduit")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/cryptonite-conduit/cryptonite-conduit-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1bldcmda4xh52mw1wfrjljv8crhw3al7v7kv1j0vidvr7ymnjpbh"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-conduit" ,ghc-conduit)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-cryptonite" ,ghc-cryptonite)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-memory" ,ghc-memory)
       ("ghc-resourcet" ,ghc-resourcet)))
    (arguments
     `(#:tests?
       #f
       #:cabal-revision
       ("1"
        "1hh2nzfz4qpxgivfilgk4ll416lph8b2fdkzpzrmqfjglivydfmz")))
    (home-page
     "https://github.com/haskell-crypto/cryptonite-conduit")
    (synopsis "cryptonite conduit")
    (description
     "Conduit bridge for cryptonite . For now only provide a conduit version for hash and hmac, but with contribution, this could provide cipher conduits too, and probably other things.")
    (license bsd-3)))

(define-public ghc-temporary
  (package
    (name "ghc-temporary")
    (version "1.2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/temporary/temporary-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1wq0rc71mp0lw7pkpcbhglf636ni46xnlpsmx6yz8acmwmqj8xsm"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-exceptions" ,ghc-exceptions)))
    (arguments `(#:tests? #f))
    (home-page
     "https://github.com/feuerbach/temporary")
    (synopsis
     "Portable temporary file and directory support")
    (description
     "Functions for creating temporary files and directories.")
    (license bsd-3)))

(define-public ghc-path-io
  (package
    (name "ghc-path-io")
    (version "1.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/path-io/path-io-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1g9m3qliqjk1img894wsb89diym5zrq51qkkrwhz4sbm9a8hbv1a"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-dlist" ,ghc-dlist)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-path" ,ghc-path)
       ("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-temporary" ,ghc-temporary)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)))
    (arguments
     `(#:cabal-revision
       ("3" "1h9hsibbflkxpjl2fqamqiv3x3gasf51apnmklrs9l9x8r32hzcc")))
    (home-page
     "https://github.com/mrkkrp/path-io")
    (synopsis "Functions for manipulating well-typed paths")
    (description "This package provides an interface to the @code{directory}
package for users of @code{path}.  It also implements some missing stuff like
recursive scanning and copying of directories, working with temporary
files/directories, and more.")
    (license bsd-3)))

(define-public ghc-stack
  (package
    (name "ghc-stack")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/stack/stack-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "17rjc9fz1hn56jz4bnhhm50h5x71r69jizlw6dx7kfvm57hg5i0r"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-annotated-wl-pprint"
        ,ghc-annotated-wl-pprint)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-async" ,ghc-async)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-cryptonite" ,ghc-cryptonite)
       ("ghc-cryptonite-conduit"
        ,ghc-cryptonite-conduit)
       ("ghc-echo" ,ghc-echo)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-extra" ,ghc-extra)
       ("ghc-file-embed" ,ghc-file-embed)
       ("ghc-filelock" ,ghc-filelock)
       ("ghc-fsnotify" ,ghc-fsnotify)
       ("ghc-generic-deriving" ,ghc-generic-deriving)
       ("ghc-hackage-security" ,ghc-hackage-security)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-hpack" ,ghc-hpack)
       ("ghc-http-client" ,ghc-http-client)
       ("ghc-http-client-tls" ,ghc-http-client-tls)
       ("ghc-http-conduit" ,ghc-http-conduit)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-memory" ,ghc-memory)
       ("ghc-microlens" ,ghc-microlens)
       ("ghc-mintty" ,ghc-mintty)
       ("ghc-monad-logger" ,ghc-monad-logger)
       ("ghc-mono-traversable" ,ghc-mono-traversable)
       ("ghc-mustache" ,ghc-mustache)
       ("ghc-neat-interpolation"
        ,ghc-neat-interpolation)
       ("ghc-network-uri" ,ghc-network-uri)
       ("ghc-open-browser" ,ghc-open-browser)
       ("ghc-optparse-applicative"
        ,ghc-optparse-applicative)
       ("ghc-path" ,ghc-path)
       ("ghc-path-io" ,ghc-path-io)
       ("ghc-persistent" ,ghc-persistent)
       ("ghc-persistent-sqlite" ,ghc-persistent-sqlite)
       ("ghc-persistent-template"
        ,ghc-persistent-template)
       ("ghc-primitive" ,ghc-primitive)
       ("ghc-project-template" ,ghc-project-template)
       ("ghc-regex-applicative-text"
        ,ghc-regex-applicative-text)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-retry" ,ghc-retry)
       ("ghc-rio" ,ghc-rio)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-split" ,ghc-split)
       ("ghc-store" ,ghc-store)
       ("ghc-store-core" ,ghc-store-core)
       ("ghc-streaming-commons" ,ghc-streaming-commons)
       ("ghc-tar" ,ghc-tar)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-text-metrics" ,ghc-text-metrics)
       ("ghc-th-reify-many" ,ghc-th-reify-many)
       ("ghc-tls" ,ghc-tls)
       ("ghc-typed-process" ,ghc-typed-process)
       ("ghc-unicode-transforms"
        ,ghc-unicode-transforms)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-unliftio" ,ghc-unliftio)
       ("ghc-unordered-containers"
        ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)
       ("ghc-yaml" ,ghc-yaml)
       ("ghc-zip-archive" ,ghc-zip-archive)
       ("ghc-zlib" ,ghc-zlib)
       ("ghc-bindings-uname" ,ghc-bindings-uname)
       ("ghc-gitrev" ,ghc-gitrev)
       ("ghc-optparse-simple" ,ghc-optparse-simple)))
    (arguments
     `(#:tests?
       #f
       #:cabal-revision
       ("10"
        "1985lm9m6pm9mi4h4m2nrn9v2rnnfh14slcnqgyxy6k934xqvg35")))
    (home-page "http://haskellstack.org")
    (synopsis "The Haskell Tool Stack")
    (description
     "Please see the README.md for usage information, and the wiki on Github for more details.  Also, note that the API for the library is not currently stable, and may change significantly, even between minor releases. It is currently only intended for use by the executable.")
    (license bsd-3)))
