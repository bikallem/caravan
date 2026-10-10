"""A stub of the tranco package for make conformance.

helpers.util.create_sites_urls of cispa/http-conformance calls Tranco before it
examines the mode. The real package downloads a list from the network. The
local mode does not use the list. This stub gives the rank -1 for each domain
and uses no network. It is first on PYTHONPATH, thus it replaces the real
package. The upstream files do not change.
"""


class _List:
    def rank(self, domain):
        return -1


class Tranco:
    def __init__(self, **kw):
        pass

    def list(self, date=None, list_id=None):
        return _List()
