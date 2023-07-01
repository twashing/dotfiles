(define-module (guix packages rustscan)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix gexp)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)

  #:use-module ((guix licenses) #:prefix license:))

(define-public rust-text-placeholder-0.4
  (package
    (name "rust-text-placeholder")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "text_placeholder" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0q0iq60dflczvyfp3m4wn3w358yyxmcljmzdb9b5favwnb2c8qcn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/bernardoamc/text-placeholder")
    (synopsis "A flexible text template engine")
    (description "This package provides a flexible text template engine")
    (license (list license:expat license:asl2.0))))

(define-public rust-rlimit-0.8
  (package
    (name "rust-rlimit")
    (version "0.8.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rlimit" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18vsz3fdj4s8yjp96wwq7wvrlc3vzzsqki8mfpha9m5zr0g8l9zp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/Nugine/rlimit/")
    (synopsis "Resource limits")
    (description "Resource limits")
    (license license:expat)))

(define-public rust-gcd-2
  (package
    (name "rust-gcd")
    (version "2.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gcd" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "06l4fib4dh4m6gazdrzzzinhvcpcfh05r4i4gzscl03vnjhqnx8x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/frewsxcv/rust-gcd")
    (synopsis "Calculate the greatest common divisor")
    (description "Calculate the greatest common divisor")
    (license (list license:expat license:asl2.0))))

(define-public rust-colorful-0.2
  (package
    (name "rust-colorful")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "colorful" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1kj7vb4glvwcp75ambq2npf3dv1vjq4zkz12j8ypyzasaii0bbwp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/mellite/colorful")
    (synopsis "Make your terminal output colorful")
    (description "Make your terminal output colorful")
    (license license:expat)))

(define-public rust-cidr-utils-0.5
  (package
    (name "rust-cidr-utils")
    (version "0.5.10")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cidr-utils" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0750jbxvdbyyxcqnzsw438158r9drs2g077ymx9r9lv193q3dypx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-debug-helper" ,rust-debug-helper-0.3)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://magiclen.org/cidr-utils")
    (synopsis
     "This crate provides data structures and functions to deal with IPv4 CIDRs and IPv6 CIDRs.")
    (description
     "This crate provides data structures and functions to deal with IPv4 CIDRs and
IPv6 CIDRs.")
    (license license:expat)))

(define-public rust-rustscan-2
  (package
    (name "rust-rustscan")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustscan" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xdxzp4dfa8dcc6srz5apbfxkcm1a1w5d4k7wym4x0ql5kibfb4b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-ansi-term" ,rust-ansi-term-0.12)
                       ("rust-anyhow" ,rust-anyhow-1)
                       ("rust-async-std" ,rust-async-std-1)
                       ("rust-cidr-utils" ,rust-cidr-utils-0.5)
                       ("rust-colored" ,rust-colored-2)
                       ("rust-colorful" ,rust-colorful-0.2)
                       ("rust-dirs" ,rust-dirs-3)
                       ("rust-env-logger" ,rust-env-logger-0.8)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-gcd" ,rust-gcd-2)
                       ("rust-itertools" ,rust-itertools-0.9)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rand" ,rust-rand-0.7)
                       ("rust-rlimit" ,rust-rlimit-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-shell-words" ,rust-shell-words-1)
                       ("rust-structopt" ,rust-structopt-0.3)
                       ("rust-subprocess" ,rust-subprocess-0.2)
                       ("rust-text-placeholder" ,rust-text-placeholder-0.4)
                       ("rust-toml" ,rust-toml-0.5)
                       ("rust-trust-dns-resolver" ,rust-trust-dns-resolver-0.19))
       #:cargo-development-inputs (("rust-wait-timeout" ,rust-wait-timeout-0.2))))
    (native-inputs (list perl python))
    (home-page "https://github.com/rustscan/rustscan")
    (synopsis "Faster Nmap Scanning with Rust")
    (description "Faster Nmap Scanning with Rust")
    (license license:expat)))
