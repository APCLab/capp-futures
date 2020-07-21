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

    curl 'http://localhost:5984/market/_design/futures/_list/settlement/settlement?startkey=\["TX","2020-01-01"\]&endkey=\["TX\ufff0"\]'

*n* sec K Bar endpoint, specified via ``tf`` argument which stands for *timeframe*::

    curl 'http://localhost:5984/market/_design/futures/_list/kbar/kbar?startkey=\["TX"\]&endkey=\["TX\ufff0"\]&tf=2'

    curl 'http://localhost:5984/market/_design/futures/_list/kbar/kbar?startkey=\["TX","2020-03-01"\]&endkey=\["TX","2020-12-12"\]&tf=3600'

Vol Bar::

    curl 'http://localhost:5984/market/_design/futures/_list/volbar/vol/volbar?startkey=\["TX","2020-01-01"\]&endkey=\["TX\ufff0"\]&tf=3600'

Tick Bar::

    curl 'http://localhost:5984/market/_design/futures/_list/tickbar/tick/tickbar?startkey=\["TX","2020-01-01"\]&endkey=\["TX\ufff0"\]&tf=3600'


Dev Notes
----------------------------------------------------------------------

- By Using sample view function in single ddoc, but with different reduce function,
  the view index won't be rebuilt.

- https://docs.couchdb.org/en/stable/ddocs/views/intro.html#one-vs-multiple-design-documents

- https://stackoverflow.com/questions/28572449

- Why we need ``\ufff0`` in ``endkey``?
  https://docs.couchdb.org/en/stable/ddocs/views/collation.html
