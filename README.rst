capp-futures
===============================================================================

Couchapp for futures baked by
`couchapp.py <https://github.com/couchapp/couchapp>`_.

Usage
----------------------------------------------------------------------


Settlement endpoint::

    curl -H 'Accept: text/csv' 'http://localhost:5984/market/_design/futures/_list/settlement/tx?startkey=\["2017-01-01"\]'
