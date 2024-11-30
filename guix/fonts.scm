(use-modules (guix packages)
             (guix build-system copy)
             (gnu packages fonts))

(define pragmata-pro
  (package
   (name "pragmata-pro")
   (version "1.0")

   ;; NOTE install .ttf locally
   (source (local-file "fonts/PragmataPro.ttf"))
   (build-system copy-build-system)
   (arguments
    '(#:install-plan '(("PragmataPro.ttf" "share/fonts/truetype/"))))
   (home-page "")
   (synopsis "A custom font, wrapping the Pragmata Pro font file")
   (description "Pragmata Pro font package")
   (license #f)))

(packages->manifest
 (list pragmata-pro
       font-iosevka))
