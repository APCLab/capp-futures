function (doc) {
  if (doc.symbol === 'TXO')
    emit([doc.date, doc.symbol, doc.contract, doc.right, doc.strike_price], doc.records);
}
