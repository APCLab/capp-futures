/**
 * List function - use `start()` and `send()` to output headers and content.
 * @link http://docs.couchdb.org/en/latest/couchapp/ddocs.html#listfun
 *
 * @param {object} head - View Head Information.
 *   http://docs.couchdb.org/en/latest/json-structure.html#view-head-info-object
 * @param {object} req - Request Object.
 *   http://docs.couchdb.org/en/latest/json-structure.html#request-object
 *
 * query param:
 *   @param {string/int} tf - timeframe in seconds or `daily`
 **/
function(head, req) {
  /*
   * param format: (csv|json)
  * */
  var moment = require('lib/moment.min');
  var duration = moment.duration;
  var timeframe = req.query.tf || 'daily';


  var get_ohlc = function (row, timeframe) {
    if (timeframe === 'daily') {
      // init value
      var r = row.value[0];
      var o = r.price;
      var h = r.price;
      var l = r.price;
      var c = r.price;
      var vol = 0;

      for (var i in row.value) {
        var r = row.value[i];
        var p = r.price;

        h = (p > h ? p : h);
        l = (p < l ? p : l);
        c = p;
        vol += r.volume;
      }

      return [[o, h, l, c, vol]];
    }

    // intra-day kbar
    var rows = row.value;
    var r_idx = 0; // rows index
    var r = rows[r_idx]; // single row

    var o = 0;
    var h = 0;
    var l = 99999;
    var c = 0;
    var vol = 0;
    var bars = [];

    var start_time = 31500; // 08:45:00 in seconds
    var end_time = 49500;  // 13:45:00 in seconds

    timeframe = parseInt(timeframe);

    for (var cur_time=start_time; cur_time<end_time; cur_time+=timeframe) {
      var tf_end_time = cur_time + timeframe;

      if (r_idx == 0) {
        o = 0;
        h = 0;
        l = 99999;
        c = 0;
        vol = 0;
      }
      else {
        /*
         * Consider there is no any tick in this timeframe,
         * we should inherit value from the last tick.
         */
        r = rows[r_idx - 1];
        o = h = l = c = r.price;
        vol = 0;
      }

      var _init = false;
      for (; r_idx<rows.length; ++r_idx) {
        r = rows[r_idx];
        if (duration(r.time).asSeconds() >= tf_end_time)
          break;  // enter next timeframe

        p = r.price;

        if (!_init) {
          _init = true;
          o = h = l = c = p;
          vol = r.volume;
          continue;
        }

        h = (p > h ? p : h);
        l = (p < l ? p : l);
        c = p;
        vol += r.volume;
      }
      /* Rolling out the kbar
       *
       * Although there is no rows to process,
       * we still need to fill some kbar to the following timeframe.
       */
      bars.push([
        moment.utc(cur_time * 1000).format('HH:mm:ss'),
        o, h, l, c, vol
      ])
    }
    /*
     * Note that there are some tick data reveal after 13:45:00.
     * In this case, we will merge the left ticks into the latest kbar.
     */
    for(; r_idx<rows.length; ++r_idx) {
      var bar = bars[bars.length - 1];

      r = rows[r_idx];
      var p = r.price;

      bar[1] = (bar[1] == 0 ? p : bar[1]);
      bar[2] = (p > h ? p : h);
      bar[3] = (p < l ? p : l);
      bar[4] = p;
      bar[5] += r.volume;
    }

    return bars;
  };

  var gen = function(format) {
    return function() {
      if (format === 'csv') {
        if (timeframe === 'daily')
          send('Symbol,Contract,Date,Open,High,Low,Close,Volume\n');
        else
          send('Symbol,Contract,Date,Time,Open,High,Low,Close,Volume\n');
      }
      else if (format === 'json') {
        send('[');
      }

      while (row = getRow()) {  // a row is intra-day, single target data
        var bars = get_ohlc(row, timeframe);

        for (var i in bars) {
          var bar = bars[i];

          if (format === 'csv') {
            var msg = '';

            // Symbol and Contract
            for (var j=1; j<row.key.length; j++)
              msg += (row.key[j] + ',');
            msg += row.key[0];  // Date

            // [Time,] OHLC, Volume
            msg += (',' + bar);

            msg += '\n';
            send(msg);
          }
          else if (format === 'json') {
            send(JSON.stringify(bar))

            if (i != bars.length - 1)
              send(',\n');
          }
        }
      }

      if (format === 'json') {
        send(']\n');
      }
    };
  };

  provides('csv', gen('csv'));
  provides('json', gen('json'));
}
