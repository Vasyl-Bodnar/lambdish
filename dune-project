(lang dune 3.8)

(name lambdish)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Vasyl Bodnar")

(maintainers "Vasyl-Bodnar")

(license MIT)

; (documentation https://url/to/documentation)

(package
 (name lambdish)
 (synopsis "Lambda Calculus Interpreter")
 (description "Lambda Calculus Interpreter")
 (depends ocaml dune)
 (tags
  (interpreter language)))
