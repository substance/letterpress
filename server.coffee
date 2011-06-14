express = require('express')
app = express.createServer()
http = require('http')
fs = require('fs')
_ = require('underscore')
Data = require('data')
async = require('async')
LatexRenderer = require('./src/renderers').LatexRenderer
{spawn, exec} = require 'child_process'

# Express.js Configuration
# -----------

app.configure ->
  app.use(express.bodyParser())
  app.use(express.methodOverride())
  app.use(express.cookieParser())
  app.use(app.router)
  app.use(express.static(__dirname+"/public", { maxAge: 41 }))

# Fixtures
schema = JSON.parse(fs.readFileSync(__dirname+ '/data/schema.json', 'utf-8'))
raw_doc = JSON.parse(fs.readFileSync(__dirname+ '/data/document.json', 'utf-8'))


# Util
# -----------

Util = {}

Util.fetchDocument = (url, callback) ->
  fragments = require('url').parse(url)
  options = { host: fragments.host, path: fragments.pathname }
  options.path += fragments.search if fragments.search

  http.get options, (cres) ->
    cres.setEncoding('utf8')
    json = ""
    cres.on 'data', (d) ->
      json += d
      
    cres.on 'end', ->
      callback(null, JSON.parse(json))
  .on 'error', (e) ->
    callback(e)

Util.convert = (url, options, callback) ->
  Util.fetchDocument url, (err, raw_doc) ->
    graph = new Data.Graph(schema)
    graph.merge(raw_doc.graph)
    doc = graph.get(raw_doc.id)
    
    new LatexRenderer(doc).render (latex) ->
      callback(null, latex)


# Routes
# -----------


# Index
app.get '/', (req, res) ->
  html = fs.readFileSync(__dirname+ '/templates/app.html', 'utf-8')
  res.send(html)

# Convert to LaTeX
app.get '/latex', (req, res) ->
  res.charset = 'utf8'
  res.header('Content-Type', 'text/plain')
  Util.convert req.query.url, {format: 'Latex'}, (err, latex) ->
    res.send(latex)

# On the fly PDF generation
app.get '/pdf', (req, res) ->
  pdfCmd = "pdflatex -output-directory tmp tmp/document.latex"

  res.writeHead(200, { 'Content-Type': 'application/pdf'})
  Util.convert req.query.url, {format: 'Latex'}, (err, latex) ->
    
    fs.writeFile 'tmp/document.latex', latex, (err) ->
      throw err if err
      exec pdfCmd, (err, stdout, stderr) ->
        console.log(stdout);
        fs.readFile 'tmp/document.pdf', (err, data) ->
          res.write(data, 'binary');
          res.end()
        throw err if err
      

# Start the fun
console.log('Letterpress is listening at http://localhost:4004')
app.listen(4004)