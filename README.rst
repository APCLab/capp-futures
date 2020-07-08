capp-futures
===============================================================================

Couchapp for futures baked by
`couchapp.py <https://github.com/couchapp/couchapp>`_.

Push to CouchDB
----------------------------------------------------------------------

#. Create ``.couchapprc`` in dir ``futures``::

    {
        "env" : {
            "default" : {
                "db" : "http://username:password@localhost:5984/market"
            }
        }
    }

#. ``cp futures/.couchapprc options/``

#. ``make``


Usage
----------------------------------------------------------------------


Settlement endpoint::

    curl 'http://localhost:5984/market/_design/futures/_list/settlement/tx_settlement?&startkey=\["2020-01-01"\]'

Daily K Bar endpoint::

    curl 'http://localhost:5984/market/_design/futures/_list/kbar/tx_kbar?tf=18000&keys=\[\["2017-03-01","TX","201703"\],\["2017-03-02","TX","201703"\]\]'

    curl 'http://localhost:5984/market/_design/options/_list/kbar/txo_kbar?tf=18000reduce=false&startkey=\["2017-01-01"\]'

*n* sec K Bar endpoint, specified via ``tf`` argument which stands for *timeframe*::

    curl 'http://localhost:5984/market/_design/futures/_list/kbar/tx_kbar?&startkey=\["2017-01-01"\]&tf=2'

Vol profile::

    curl 'http://localhost:5984/market/_design/futures/_list/vol/tx_vol?tf=60&startkey=\["2020-01-01"\]'

Another special ``tf`` keywords are supported:
- ``daily``
- ``raw``, which will return the price/volume ticks.

JSON K Bar is currently only well formatted for single day query::

    curl  -H "Accept: application/json" 'http://localhost:5984/market/_design/futures/_list/kbar/tx?keys=\[\["2017-03-01","TX","201703"\]\]&tf=10'


Dev Notes
----------------------------------------------------------------------

- By Using sample view function in single ddoc, but with different reduce function,
  the view index won't be rebuilt.

- https://docs.couchdb.org/en/stable/ddocs/views/intro.html#one-vs-multiple-design-documents

- https://stackoverflow.com/questions/28572449
