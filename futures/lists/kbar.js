/**
 * List function - use `start()` and `send()` to output headers and content.
 * @link http://docs.couchdb.org/en/latest/couchapp/ddocs.html#listfun
 *
 * @param {object} head - View Head Information.
 *   http://docs.couchdb.org/en/latest/json-structure.html#view-head-info-object
 * @param {object} req - Request Object.
 *   http://docs.couchdb.org/en/latest/json-structure.html#request-object
 **/
function(head, req) {
  // currently only daily k bar

  /*
   * param format: (csv|json)
  * */
  var get_ohlc = function (row) {
    var r = row.value[0];
    var o = r.price;
    var h = r.price;
    var l = r.price;
    var c = r.price;
    var vol = r.volume;

    for (var i in row.value) {
      var r = row.value[i];

      h = (r.price > h ? r.price : h);
      l = (r.price < l ? r.price : l);
      c = r.price;
      vol += r.volume;
    }

    return [o, h, l, c, vol];
  };

  var gen = function(format) {
    return function() {
      if (format === 'csv')
        send('Date,\tOpen,\tHigh,\tLow,\tClose,\tVolume\n')

      while (row = getRow()) {
        var bar = get_ohlc(row);

        if (format === 'csv') {
          send(row.key[0] + ',\t' +
               bar[0] + ',\t' + bar[1] + ',\t' + bar[2] + ',\t' +
               bar[3] + ',\t' + bar[4] + '\n');
        }
      }
    }
  };

  provides('csv', gen('csv'));
}
