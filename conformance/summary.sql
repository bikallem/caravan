-- The number of verdicts for each rule of a make conformance run, also the
-- verdicts "Follows specification". The plan (6.7) compares them with part A.
-- A probe counts one time for its two http2 variants. Inapplicable probe
-- verdicts are not in the database.
COPY (
  SELECT kind, rule, verdict, count(*)
    FROM (SELECT 'direct' AS kind, name AS rule, violation AS verdict FROM directtest
          UNION ALL
          SELECT 'probe', p.name, p.violation
            FROM probetest p JOIN reqresp r ON r.id = p.req_id
           WHERE r.probe_id LIKE '%, 1)'
          UNION ALL
          SELECT 'retro', name, violation FROM retrotest) v
   GROUP BY 1, 2, 3
   ORDER BY 1, 2, 3
) TO STDOUT;
