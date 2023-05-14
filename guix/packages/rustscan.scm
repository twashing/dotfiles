(define-module (guix packages rustscan)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix gexp)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  #:use-module (guix licenses))

(use-modules
 ;; (guix packages)
 ;; (guix download)
 ;; (gnu packages python)
 ;; (guix build-system python)
 ((guix licenses) #:prefix license:))

;; (define-public rust-windows-targets-0.48
;;   (package
;;     (name "rust-windows-targets")
;;     (version "0.48.0")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (crate-uri "windows-targets" version))
;;               (file-name (string-append name "-" version ".tar.gz"))
;;               (sha256
;;                (base32
;;                 "1mfzg94w0c8h4ya9sva7rra77f3iy1712af9b6bwg03wrpqbc7kv"))))
;;     (build-system cargo-build-system)
;;     (arguments
;;      `(#:cargo-inputs (("rust-windows-aarch64-gnullvm" ,rust-windows-aarch64-gnullvm-0.48)
;;                        ("rust-windows-aarch64-msvc" ,rust-windows-aarch64-msvc-0.48)
;;                        ("rust-windows-i686-gnu" ,rust-windows-i686-gnu-0.48)
;;                        ("rust-windows-i686-msvc" ,rust-windows-i686-msvc-0.48)
;;                        ("rust-windows-x86-64-gnu" ,rust-windows-x86-64-gnu-0.48)
;;                        ("rust-windows-x86-64-gnullvm" ,rust-windows-x86-64-gnullvm-0.48)
;;                        ("rust-windows-x86-64-msvc" ,rust-windows-x86-64-msvc-0.48))))
;;     (home-page "https://github.com/microsoft/windows-rs")
;;     (synopsis "Import libs for Windows")
;;     (description "Import libs for Windows")
;;     (license (list license:expat license:asl2.0))))
;;
;; (define-public rust-windows-sys-0.48
;;   (package
;;     (name "rust-windows-sys")
;;     (version "0.48.0")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (crate-uri "windows-sys" version))
;;               (file-name (string-append name "-" version ".tar.gz"))
;;               (sha256
;;                (base32
;;                 "1aan23v5gs7gya1lc46hqn9mdh8yph3fhxmhxlw36pn6pqc28zb7"))))
;;     (build-system cargo-build-system)
;;     (arguments
;;      `(#:cargo-inputs (("rust-windows-targets" ,rust-windows-targets-0.48))))
;;     (home-page "https://github.com/microsoft/windows-rs")
;;     (synopsis "Rust for Windows")
;;     (description "Rust for Windows")
;;     (license (list license:expat license:asl2.0))))
;;
;; (define-public rust-dirs-sys-0.4
;;   (package
;;     (name "rust-dirs-sys")
;;     (version "0.4.0")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (crate-uri "dirs-sys" version))
;;               (file-name (string-append name "-" version ".tar.gz"))
;;               (sha256
;;                (base32
;;                 "0jsmmfls4p79yby563xrlpckf5qxkq7yam7zqms0vxw8vc046h84"))))
;;     (build-system cargo-build-system)
;;     (arguments
;;      `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
;;                        ("rust-redox-users" ,rust-redox-users-0.4)
;;                        ("rust-windows-sys" ,rust-windows-sys-0.48))))
;;     (home-page "https://github.com/dirs-dev/dirs-sys-rs")
;;     (synopsis
;;      "System-level helper functions for the dirs and directories crates.")
;;     (description
;;      "System-level helper functions for the dirs and directories crates.")
;;     (license (list license:expat license:asl2.0))))
;;
;; (define-public rust-dirs-5
;;   (package
;;     (name "rust-dirs")
;;     (version "5.0.0")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (crate-uri "dirs" version))
;;               (file-name (string-append name "-" version ".tar.gz"))
;;               (sha256
;;                (base32
;;                 "1kbfyhyw7ycf2g26rp1fi5x84dbcilyfpqy2iajy6lrkrnd05kny"))))
;;     (build-system cargo-build-system)
;;     (arguments
;;      `(#:cargo-inputs (("rust-dirs-sys" ,rust-dirs-sys-0.4))))
;;     (home-page "https://github.com/soc/dirs-rs")
;;     (synopsis
;;      "A tiny low-level library that provides platform-specific standard locations of directories for config, cache and other data on Linux, Windows, macOS and Redox by leveraging the mechanisms defined by the XDG base/user directory specifications on Linux, the Known Folder API on Windows, and the Standard Directory guidelines on macOS.")
;;     (description
;;      "This package provides a tiny low-level library that provides platform-specific
;; standard locations of directories for config, cache and other data on Linux,
;; Windows, macOS and Redox by leveraging the mechanisms defined by the XDG
;; base/user directory specifications on Linux, the Known Folder API on Windows,
;; and the Standard Directory guidelines on macOS.")
;;     (license (list license:expat license:asl2.0))))
;;
;; (define-public rust-colorful-0.2
;;   (package
;;     (name "rust-colorful")
;;     (version "0.2.2")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (crate-uri "colorful" version))
;;               (file-name (string-append name "-" version ".tar.gz"))
;;               (sha256
;;                (base32
;;                 "1kj7vb4glvwcp75ambq2npf3dv1vjq4zkz12j8ypyzasaii0bbwp"))))
;;     (build-system cargo-build-system)
;;     (home-page "https://github.com/mellite/colorful")
;;     (synopsis "Make your terminal output colorful")
;;     (description "Make your terminal output colorful")
;;     (license license:expat)))
;;
;; (define-public rust-colored-2
;;   (package
;;     (name "rust-colored")
;;     (version "2.0.0")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (crate-uri "colored" version))
;;               (file-name (string-append name "-" version ".tar.gz"))
;;               (sha256
;;                (base32
;;                 "1gbcijscmznzy42rn213yp9ima7210zakgaqibgg1n441dsnyqdk"))))
;;     (build-system cargo-build-system)
;;     (arguments
;;      `(#:cargo-inputs (("rust-atty" ,rust-atty-0.2)
;;                        ("rust-lazy-static" ,rust-lazy-static-1)
;;                        ("rust-winapi" ,rust-winapi-0.3))
;;        #:cargo-development-inputs (("rust-ansi-term" ,rust-ansi-term-0.12)
;;                                    ("rust-rspec" ,rust-rspec-1))))
;;     (home-page "https://github.com/mackwic/colored")
;;     (synopsis "The most simple way to add colors in your terminal")
;;     (description "The most simple way to add colors in your terminal")
;;     (license license:mpl2.0)))
;;
;; (define-public rust-cidr-utils-0.5
;;   (package
;;    (name "rust-cidr-utils")
;;    (version "0.5.10")
;;    (source (origin
;;             (method url-fetch)
;;             (uri (crate-uri "cidr-utils" version))
;;             (file-name (string-append name "-" version ".tar.gz"))
;;             (sha256
;;              (base32
;;               "0750jbxvdbyyxcqnzsw438158r9drs2g077ymx9r9lv193q3dypx"))))
;;    (build-system cargo-build-system)
;;    (arguments
;;     `(#:cargo-inputs (("rust-debug-helper" ,rust-debug-helper-0.3)
;;                       ("rust-num-bigint" ,rust-num-bigint-0.4)
;;                       ("rust-num-traits" ,rust-num-traits-0.2)
;;                       ("rust-once-cell" ,rust-once-cell-1)
;;                       ("rust-regex" ,rust-regex-1)
;;                       ("rust-serde" ,rust-serde-1))))
;;    (home-page "https://magiclen.org/cidr-utils")
;;    (synopsis "CIDR library for IPv4 and IPv6")
;;    (description
;;     "This package provides a crate for dealing with Classless Inter-Domain
;; Routing in IPv4 and IPv6.")
;;    (license license:expat)))
;;
;; (define-public rustscan
;;   (package
;;    (name "rustscan")
;;    (version "2.1.1")
;;    (source (origin
;;             (method git-fetch)
;;             (uri (git-reference
;;                   (url "https://github.com/RustScan/RustScan.git")
;;                   (commit version)))
;;             (file-name (git-file-name name version))
;;             (sha256
;;              (base32 "1hs065rrk40ksbxh413fbgnr760adgw9dlg5qqv6xb04ra0n0hjf"))))
;;    (build-system cargo-build-system)
;;    (arguments
;;     `(#:cargo-inputs
;;       (("rust-ansi-term" ,rust-ansi-term-0.12)
;;        ("rust-anyhow" ,rust-anyhow-1)
;;        ("rust-cidr-utils" ,rust-cidr-utils-0.5)
;;        ("rust-colored" ,rust-colored-2)
;;        ("rust-colorful" ,rust-colorful-0.2)
;;        ("rust-dirs" ,rust-dirs-5))))
;;    (home-page "https://github.com/RustScan/RustScan")
;;    (synopsis "A fast port scanner written in Rust")
;;    (description "RustScan is a fast port scanner that utilizes the Rust programming language to scan for open ports on IP addresses.")
;;    (license gpl3+)))

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
    (home-page "https://github.com/rustscan/rustscan")
    (synopsis "Faster Nmap Scanning with Rust")
    (description "Faster Nmap Scanning with Rust")
    (license license:expat)))
