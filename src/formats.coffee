formats = [
  { name: 'context', extension: 'ctx' }
  { name: 'docbook', extension: 'db', mime: 'application/docbook+xml' }
  { name: 'epub', mime: 'application/epub+zip', downloadResources: true, binary: true } # unofficial mime-type
  { name: 'html', mime: 'text/html' }
  { name: 'json', mime: 'application/json' }
  { name: 'latex' } # actual mime: text/x-latex
  { name: 'man', extension: '1' }
  { name: 'markdown' }
  { name: 'mediawiki' }
  { name: 'odt', mime: 'application/vnd.oasis.opendocument.text', downloadResources: true, binary: true }
  { name: 'org' }
  { name: 'pdf' , downloadResources: true, convertTo: 'latex' }
  { name: 'rst' }
  { name: 'rtf', mime: 'application/rtf', downloadResources: true }
  { name: 's5', mime: 'text/html' }
  { name: 'shower', mime: 'text/html' }
  { name: 'texinfo' }
  { name: 'textile' }
]

# write defaults
formats.forEach (format) ->
  format.mime              ||= 'text/plain'
  format.extension         ||= format.name
  format.convertTo         ||= format.name
  format.downloadResources ||= false
  format.binary            ||= false

indexBy = (arr, property) ->
  index = {}
  arr.forEach (obj) -> index[obj[property]] = obj
  index

exports.byName      = indexBy(formats, 'name')
exports.byExtension = indexBy(formats, 'extension')
