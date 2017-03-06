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

  /*
   * param format: (csv|json)
  * */
  var gen = function(format) {
    return function() {
      if (format === 'json')
        send('[');
      else if (format === 'csv')
        send('Date,Symbol,Contract\n')

      var near_contract = "";
      var last_date = "";
      while (row = getRow()) {
        var k = row.key;
        var date = k[0];
        var contract = k[2];

        if (date > last_date) {
          near_contract = contract;
          last_date = date;
        }
        else if (date === last_date) {
          if (near_contract > contract) {
            continue;
          }
        }
        else {
          send("\"error\"");
        }

        if (format === 'json') {
          send(toJSON([date, k[1], near_contract]));
          send(',\n');
        }
        else if (format === 'csv')
          send(date + ',' + k[1] + ',' + near_contract + '\n')
      }

      if (format === 'json')
        send(']');
    };
  };

  provides('json', gen('json'));
  provides('csv', gen('csv'));
}
