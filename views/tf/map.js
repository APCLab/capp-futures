function (doc) {
  if (doc.symbol === 'TF')
    emit([doc.date, doc.symbol, doc.contract], doc.records);
}
