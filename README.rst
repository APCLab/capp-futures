capp-futures
===============================================================================

Couchapp for futures baked by
`couchapp.py <https://github.com/couchapp/couchapp>`_.

Usage
----------------------------------------------------------------------


Settlement endpoint::

    curl -H 'Accept: text/csv' 'http://localhost:5984/market/_design/futures/_list/settlement/tx?startkey=\["2017-01-01"\]'

Daily K Bar endpoint::

    curl 'http://localhost:5984/market/_design/futures/_list/kbar/tx?keys=\[\["2017-03-01","TX","201703"\],\["2017-03-02","TX","201703"\]\]'

    curl 'http://localhost:5984/market/_design/options/_list/kbar/txo?startkey=\["2017-01-01"\]'

*n* sec K Bar endpoint, specificed via ``tf`` argument which stands for *timeframe*::

    curl 'http://localhost:5984/market/_design/options/_list/kbar/txo?startkey=\["2017-01-01"\]&tf=2'

Json K Bar is currenty only well formated for single day query::

    curl  -H "Accept: application/json" 'http://localhost:5984/market/_design/futures/_list/kbar/tx?keys=\[\["2017-03-01","TX","201703"\]\]&tf=10'
