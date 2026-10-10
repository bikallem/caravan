# Compares the violations of a make conformance run with the expected
# violations (plan 6.6):
#
#   awk -f compare.awk expected.tsv violations.tsv
#
# The two files are tab-separated. The first 6 columns are the key: kind,
# rule, url, method, set, verdict. In expected.tsv, "*" matches each value,
# and a line that starts with "#" is a comment.
#
# A violation that matches no expected row is "unexpected". An expected row
# that matches no violation is "stale". An "Unclear" violation only prints.
# The exit status is 1 if there is an unexpected or a stale row.

BEGIN { FS = "\t"; keys = 6 }

FILENAME == ARGV[1] {
  if ($0 ~ /^#/ || $0 ~ /^[ \t]*$/) next
  n++
  for (i = 1; i <= keys; i++) want[n, i] = $i
  line[n] = $0
  next
}

{
  matched = 0
  for (e = 1; e <= n; e++) {
    ok = 1
    for (i = 1; i <= keys; i++)
      if (want[e, i] != "*" && want[e, i] != $i) { ok = 0; break }
    if (ok) { used[e] = 1; matched = 1 }
  }
  key = $1
  for (i = 2; i <= keys; i++) key = key "\t" $i
  if ($6 ~ /^Unclear/) print "unclear:\t" key
  else if (!matched) { print "unexpected:\t" key; bad++ }
}

END {
  for (e = 1; e <= n; e++)
    if (!used[e]) { print "stale:\t" line[e]; bad++ }
  exit (bad > 0)
}
