function (doc) {
  if (doc.symbol === 'TE')
    emit([doc.date, doc.symbol, doc.contract], doc.records);
}
