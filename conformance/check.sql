-- The checks of a make conformance run (plan 6.4 and 6.5). Run it with
-- psql -X -q -v ON_ERROR_STOP=1 on the result database of the run.
--
-- The upstream runner catches each failure and exits 0. Thus the validity
-- checks come first. Each failed check raises an error with its message, and
-- psql stops. If all checks pass, the script writes the violations to stdout
-- in the COPY text format. The columns are the key of a violation (kind, rule,
-- url, method, set, verdict) and then extra. COPY escapes the tabs and the
-- newlines of extra.

-- The URLs: / gives 200, thus there is no redirect URL.
DO $$
DECLARE
  n int;
BEGIN
  SELECT count(*) INTO n FROM url;
  IF n <> 2
     OR NOT EXISTS (SELECT 1 FROM url WHERE description = 'Base URL' AND is_base)
     OR NOT EXISTS (SELECT 1 FROM url WHERE description = 'non-existing' AND NOT is_base)
  THEN
    RAISE EXCEPTION 'invalid run: url has % rows, expected 2 (Base URL, non-existing)', n;
  END IF;
END $$;

-- The GET requests before and after the tests of each URL.
DO $$
DECLARE
  n int;
  ok int;
BEGIN
  SELECT count(*), count(*) FILTER (WHERE susp = 'No' AND b_error = '' AND a_error = '')
    INTO n, ok FROM monitoring;
  IF n <> 2 OR ok <> 2 THEN
    RAISE EXCEPTION 'invalid run: monitoring has % rows, % without an error or a change of status, expected 2 and 2', n, ok;
  END IF;
END $$;

-- The probes: 180 for each URL, each one time, and no failed probe.
DO $$
DECLARE
  n int;
  failed int;
  bad_urls int;
BEGIN
  SELECT count(*) INTO n FROM reqresp WHERE req_type = 'proxy-probe';
  SELECT count(*) INTO failed FROM reqresp
    WHERE req_type IN ('proxy-probe-failed', 'proxy-probe-error');
  SELECT count(*) INTO bad_urls FROM url u
    WHERE (SELECT count(DISTINCT r.probe_id) FROM reqresp r
           WHERE r.url_id = u.id AND r.req_type = 'proxy-probe') <> 180;
  IF n <> 360 OR failed <> 0 OR bad_urls <> 0 THEN
    RAISE EXCEPTION 'invalid run: % probe rows (expected 360), % failed probes (expected 0), % URLs without 180 different probes', n, failed, bad_urls;
  END IF;
END $$;

-- The direct tests: 9 rules for each URL and 2 rules for the base URL only,
-- each with a verdict. A test that raised leaves a row with no verdict and a
-- row with its error: the message names them.
DO $$
DECLARE
  n int;
  pairs int;
  base int;
  empty int;
  errors text;
BEGIN
  SELECT string_agg(name || ': ' || test_error, '; ') INTO errors
    FROM directtest WHERE test_error <> '';
  SELECT count(*), count(DISTINCT (d.url_id, d.name)),
         count(*) FILTER (WHERE u.is_base), count(*) FILTER (WHERE d.violation = '')
    INTO n, pairs, base, empty
    FROM directtest d JOIN url u ON u.id = d.url_id;
  IF n <> 20 OR pairs <> 20 OR base <> 11 OR empty <> 0 THEN
    RAISE EXCEPTION 'invalid run: directtest has % rows (expected 20), % rules and URLs (expected 20), % rows for the base URL (expected 11), % rows without a verdict (expected 0); test errors: %', n, pairs, base, empty, coalesce(errors, 'none');
  END IF;
END $$;

-- The retro tests: 6 rules for each URL.
DO $$
DECLARE
  n int;
  pairs int;
BEGIN
  SELECT count(*), count(DISTINCT (url_id, name)) INTO n, pairs FROM retrotest;
  IF n <> 12 OR pairs <> 12 THEN
    RAISE EXCEPTION 'invalid run: retrotest has % rows and % rules and URLs, expected 12 and 12', n, pairs;
  END IF;
END $$;

-- The probe id has the form "(m, h, x)". The two values of the http2 index x
-- send the same HTTP/1.1 request, thus they must give the same verdicts.
CREATE TEMPORARY VIEW probe AS
SELECT p.*, u.description AS url_description,
       split_part(btrim(r.probe_id, '()'), ', ', 1)::int AS m,
       split_part(btrim(r.probe_id, '()'), ', ', 2)::int AS h,
       split_part(btrim(r.probe_id, '()'), ', ', 3)::int AS x
  FROM probetest p
  JOIN reqresp r ON r.id = p.req_id
  JOIN url u ON u.id = p.url_id;

DO $$
DECLARE
  n int;
  example text;
BEGIN
  SELECT count(*), min(format('%s %s (%s, %s) %s', name, url_description, m, h, violation))
    INTO n, example
    FROM (SELECT name, url_description, m, h, violation
            FROM probe
           GROUP BY name, url_description, m, h, violation
          HAVING count(*) FILTER (WHERE x = 1) <> count(*) FILTER (WHERE x = 2)) d;
  IF n <> 0 THEN
    RAISE EXCEPTION 'invalid run: % verdicts differ between the two http2 variants of a probe, for example %', n, example;
  END IF;
END $$;

-- The violations: each verdict that is not "Follows specification" and not
-- "Inapplicable (...)", and each test error. A probe row stands for the two
-- http2 variants of the probe.
COPY (
  WITH v AS (
    SELECT 'direct' AS kind, d.name AS rule, u.description AS url,
           '-' AS method, '-' AS header_set, d.violation, d.test_error, d.extra
      FROM directtest d JOIN url u ON u.id = d.url_id
    UNION ALL
    SELECT 'probe', name, url_description,
           (ARRAY['GET', 'HEAD', 'POST', 'OPTIONS', 'DELETE', 'PUT', 'TRACE',
                  'PATCH', 'ABC'])[m],
           h::text, violation, test_error, extra
      FROM probe
    UNION ALL
    SELECT 'retro', t.name, u.description, '-', '-', t.violation, t.test_error, t.extra
      FROM retrotest t JOIN url u ON u.id = t.url_id
  )
  SELECT kind, rule, url, method, header_set,
         CASE WHEN test_error <> '' THEN 'Test failed' ELSE violation END AS verdict,
         string_agg(DISTINCT concat_ws(' ', nullif(extra, ''),
                                       'test_error: ' || nullif(test_error, '')),
                    ' | ') AS extra
    FROM v
   WHERE test_error <> ''
      OR (violation <> 'Follows specification' AND violation NOT LIKE 'Inapplicable%')
   GROUP BY 1, 2, 3, 4, 5, 6
   ORDER BY kind, rule, url, method, length(header_set), header_set, verdict
) TO STDOUT;
