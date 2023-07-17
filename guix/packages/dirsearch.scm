(define-module (guix packages dirsearch)
  ;; #:use-module ((guix build pyproject-build-system) #:prefix python:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web))

(define-public python-requests-ntlm
  (package
    (name "python-requests-ntlm")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "requests_ntlm" version))
              (sha256
               (base32
                "1a0np7lk8ma1plv1s4aw5q9h2z3aljprkl9qsfypqcaf0zsqbhik"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-cryptography python-pyspnego
                             python-requests))
    (home-page "https://github.com/requests/requests-ntlm")
    (synopsis
     "This package allows for HTTP NTLM authentication using the requests library.")
    (description
     "This package allows for HTTP NTLM authentication using the requests library.")
    (license #f)))

(define-public python-dirsearch
  (package
    (name "python-dirsearch")
    (version "0.4.3.post1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "dirsearch" version))
              (sha256
               (base32
                "1nax6g282480qf0ccmrjilpa0w8gci1njg8jychq3jjhj8qc0vzw"))))
    (build-system pyproject-build-system)

    ;; (arguments `(#:tests? #false))
    ;; (arguments `(#:tests? #f))
    ;; (arguments (list #:tests? #false)) ;the tests are not run automatically

    ;; (arguments
    ;;  `(#:tests? #false
    ;;    #:phases
    ;;    (modify-phases python:%standard-phases
    ;;                   (delete 'check))))

    ;; (arguments
    ;;  `(#:tests? #false
    ;;    #:phases
    ;;    (modify-phases %standard-phases
    ;;                   (delete 'check))))

    ;; (arguments
    ;;  '(#:phases
    ;;    (modify-phases %standard-phases
    ;;                   (delete 'check))))

    ;; (arguments
    ;;  `(#:tests? #false
    ;;    #:phases
    ;;    (modify-phases python:%standard-phases
    ;;                   (replace 'check
    ;;                            (lambda* (#:key inputs outputs #:allow-other-keys)
    ;;                                     (invoke "echo noop"))))))

    (arguments
     (list
      #:modules '((guix build pyproject-build-system)

                  ;; (guix build utils)
                  ;; (ice-9 match)
                  )
      ;; #:test-flags
      ;; ;; "from sklearn.datasets import load_boston" fails because it has been
      ;; ;; removed from scikit-learn since version 1.2.
      ;; '(list "--ignore=tests/conftest.py"
      ;;   "--ignore=tests/test_tools.py"
      ;;   "--ignore=tests/tree/test_regressor.py"
      ;;   "--ignore=tests/ensemble/test_regressor.py")
      #:phases
      #~(modify-phases python:%standard-phases
          (delete 'check)
          (delete 'check)
          (delete 'check)
          )))

    (propagated-inputs (list python-beautifulsoup4
                             python-certifi
                             python-cffi
                             python-chardet
                             python-charset-normalizer
                             python-colorama
                             python-cryptography
                             python-defusedxml
                             python-idna
                             python-jinja2
                             python-markupsafe
                             python-ntlm-auth
                             python-pyopenssl
                             python-pyparsing
                             python-pysocks
                             python-requests
                             python-requests-ntlm
                             python-urllib3))
    (home-page "https://github.com/maurosoria/dirsearch")
    (synopsis "Advanced web path scanner")
    (description "Advanced web path scanner")
    (license #f)))
