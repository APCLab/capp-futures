/**
 * List function - use `start()` and `send()` to output headers and content.
 * @link http://docs.couchdb.org/en/latest/couchapp/ddocs.html#listfun
 *
 * @param {object} head - View Head Information.
 *   http://docs.couchdb.org/en/latest/json-structure.html#view-head-info-object
 * @param {object} req - Request Object.
 *   http://docs.couchdb.org/en/latest/json-structure.html#request-object
 **/
"use strict";

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
      var init = false;
      var row;
      while (row = getRow()) {
        var k = row.key;
        var date = k[0];
        var contract = k[2];

        if (!init) {
          init = true;
          near_contract = contract;
          last_date = date;
        }

        if (date > last_date) {
          /* date change:
           *   - render: note that we will miss the end of data.
           *   - init vars
           */
          if (format === 'json') {
            send(toJSON([last_date, k[1], near_contract]));
            send(',\n');
          }
          else if (format === 'csv')
            send(last_date + ',' + k[1] + ',' + near_contract+ '\n');

          near_contract = contract;
          last_date = date;
          continue;
        }
        else if (date === last_date) { /* still meet the same date */
          if (contract < near_contract)
            near_contract = contract;
        }
        else
          send("\"error\"");
      }

      if (format === 'json')
        send(']');
    };
  };

  provides('json', gen('json'));
  provides('csv', gen('csv'));
}
