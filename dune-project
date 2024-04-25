(lang dune 3.15)
(name onionctrl)
(documentation "https://verbosemode.github.io/onionctrl/")
(source (github verbosemode/onionctrl))
(license ISC)
(authors "Jochen Bartl <jochen@verbosemo.de>")
(maintainers "Jochen Bartl <jochen@verbosemo.de>")
(generate_opam_files true)
(package
 (name onionctrl)
 (synopsis "A Tor controller implementation")
 (description "A Tor controller implementation")
 (depends
  angstrom
  astring
  bos
  hex
  logs
  miou
  ppx_deriving
  (ocaml (>= 5.0.0))
  dune
  (alcotest :with-test)
  (odoc :with-doc)))
