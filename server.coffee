express = require('express')
app = express.createServer()
http = require('http')
fs = require('fs')
_ = require('underscore')
Data = require('data')
async = require('async')
LatexRenderer = require('./src/renderers').LatexRenderer


# Fixtures
schema = JSON.parse(fs.readFileSync(__dirname+ '/data/schema.json', 'utf-8'))
raw_doc = JSON.parse(fs.readFileSync(__dirname+ '/data/document.json', 'utf-8'))

# Init graph with schema+raw_doc
graph = new Data.Graph(schema)
graph.merge(raw_doc.graph)

# Fetch the doc
doc = graph.get('/document/substance/2569faba6cc0583fa8a7b037d8a721b3')

console.log(LatexRenderer)

app = express.createServer()

app.get '/', (req, res) ->
  res.send(new LatexRenderer(doc).render());
  # res.send('Hello World')

console.log('Substance Press is listening at http://localhost:4004');
app.listen(4004);