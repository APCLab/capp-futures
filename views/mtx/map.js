function (doc) {
  if (doc.symbol === 'MTX')
    emit([doc.date, doc.symbol, doc.contract], doc.records);
}
