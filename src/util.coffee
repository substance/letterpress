http = require 'http'
urlM = require 'url'
fs   = require 'fs'
path = require 'path'
crypto = require 'crypto'
child_process = require 'child_process'

async = require 'async'
Data = require 'data'

PandocRenderer = require './pandoc_renderer'


# Paths
rootDir      = path.join(__dirname, '..')
schemaFile   = "#{rootDir}/data/schema.json"
templatesDir = "#{rootDir}/templates"
tmpDir       = "#{rootDir}/tmp"

fs.mkdirSync(tmpDir, 0755) unless path.existsSync(tmpDir)

# Fixtures
schema = JSON.parse(fs.readFileSync(schemaFile, 'utf-8'))


# Fetch document
# ==============

exports.fetchDocument = (url, callback) ->
  http.get urlToHttpOptions(url), (res) ->
    readableStreamToString res, (err, jsonStr) ->
      if err
        callback(err, null)
      else
        callback(null, jsonToDocument(JSON.parse(jsonStr)))

urlToHttpOptions = (url) ->
  fragments = urlM.parse(url)
  return {
    host: fragments.hostname
    port: fragments.port
    path: fragments.pathname + (fragments.search or '')
  }

readableStreamToString = (stream, callback) ->
  result = ''
  stream.setEncoding('utf8')
  stream.on 'data', (d) -> result += d
  stream.on 'end', -> callback(null, result)
  stream.on 'error', (e) -> callback(e, null)

jsonToDocument = (rawDoc) ->
  graph = new Data.Graph(schema)
  graph.merge(rawDoc.graph)
  doc = graph.get(rawDoc.id)


# Convert document
# ================

exports.convert = (format, doc, callback) ->
  try
    pandocJson = JSON.stringify(PandocRenderer.render(doc))
  catch exc
    # use `process.nextTick` to minimize code paths
    process.nextTick(-> callback exc, null); return
  
  cmd = "#{rootDir}/convert #{format} #{templatesDir}"
  convertProcess = child_process.exec cmd, (err, stdout, stderr) ->
    if err
      callback(new Error(stderr), null)
    else
      callback(null, stdout)
  convertProcess.stdin.end(pandocJson, 'utf-8')


# Download resources
# ==================

exports.downloadResources = (doc, callback) ->
  resourceUrls = extractResourceUrls(doc)
  async.map resourceUrls, fetchResource, (err, localPaths) ->
    return callback(err) if err
    urlsMap = keysAndValuesToMap(resourceUrls, localPaths)
    replaceResourceUrls(doc, urlsMap)
    callback(null)

walkDocument = (doc, fn) ->
  recurse = (node) ->
    fn(node)
    if children = node.all('children')
      children.each recurse
  recurse(doc)

extractResourceUrls = (doc) ->
  urls = []
  walkDocument doc, (node) ->
    if node.type._id is '/type/image'
      urls.push node.get('url')
  urls

replaceResourceUrls = (doc, urlsMap) ->
  walkDocument doc, (node) ->
    if node.type._id is '/type/image'
      url = node.get 'url'
      if urlsMap.hasOwnProperty url
        node.set url:urlsMap[url]
      else
        # TODO: what to do?
        throw new Error "URL #{url} wasn't found in urlMap"

writeStreamToFile = (readStream, path) ->
  writeStream = fs.createWriteStream(path)
  readStream.pipe(writeStream)

sha1 = (str) ->
  hash = crypto.createHash('sha1')
  hash.update(str)
  hash.digest('hex')

fetchResource = (url, callback) ->
  http.get urlToHttpOptions(url), (res) ->
    if res.statusCode != 200
      return callback(new Error "Server didn't respond with 200 (OK)", null)
    
    # TODO: handle jpeg, gif, ...
    tmpFile = "#{tmpDir}/#{sha1 url}.png"
    writeStreamToFile(res, tmpFile)
    console.log("Downloading '#{url}' to '#{tmpFile}'")
    res.on 'end', -> callback(null, tmpFile)
  .on 'error', (err) -> callback(err, null)

keysAndValuesToMap = (keys, values) ->
  map = {}
  keys.forEach (key, i) ->
    map[key] = values[i]
  map


# Generate PDF
# ============

exports.generatePdf = (latex, url, callback) ->
  hash = sha1 url
  
  latexFile = "#{tmpDir}/#{hash}.tex"
  fs.writeFile latexFile, latex, (err) ->
    return callback(err, null) if err
    pdfCmd = "pdflatex -halt-on-error -output-directory #{tmpDir} #{latexFile}"
    pdfFile = "#{tmpDir}/#{hash}.pdf"
    child_process.exec pdfCmd, (err, stdout, stderr) ->
      return callback(new Error(stderr), null) if err
      console.log("Generated '#{pdfFile}' from '#{latexFile}' using pdflatex.")
      fs.readFile pdfFile, callback
