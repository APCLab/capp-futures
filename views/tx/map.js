function (doc) {
  if (doc.symbol === 'TX')
    emit([doc.date, doc.symbol, doc.contract], doc.records);
}
