# The first OCaml example of README.md in the section with the heading
# "## <section>", or of all README.md if section is empty. The warning
# attribute lets an example have values that it does not use. The program
# fails if the section has no OCaml example.
BEGIN { on = (section == "") }
section != "" && /^## / { if (on) exit ; on = ($0 == "## " section) ; next }
on && /^```ocaml/ { inside = 1; found = 1; print "[@@@warning \"-32\"]"; next }
inside && /^```/ { exit }
inside
END { if (!found) exit 1 }
