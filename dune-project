(lang dune 3.6)

(name aoc)

(generate_opam_files true)

(source
 (github AVR-GCC/OcamlAoC))

(authors "Bar Yehudai")

(license LICENSE)

(package
 (name aoc)
 (synopsis "Advent of coding 2022 excercises")
 (depends ocaml dune))
