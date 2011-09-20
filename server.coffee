express = require 'express'
util    = require './src/util'
formats = require './src/formats'

ShowerRenderer = require('./src/shower_renderer').ShowerRenderer


# Express.js Configuration
# ========================

app = express.createServer()

app.configure ->
  app.use(app.router)
  app.use(express.static("#{__dirname}/public", { maxAge: 41 }))


# Helpers
# =======

# Looks almost like Haskell's partial application :-)

sendHttpError = (res) -> (httpCode) -> (error) ->
  res.statusCode = httpCode
  res.end "Error: #{error.message or error}"

handleError = (errorCallback, successCallback) -> (err, args...) ->
  if err
    errorCallback err
  else
    successCallback args...


# Handlers
# ========

handleConversion = (res, url, format) ->
  res.charset = 'utf8'
  sendError = sendHttpError res
  
  unless format
    # bad request
    return sendError(400)(new Error("Unknown target format."))
  
  console.log("Got request to convert '#{url}' to #{format.name}")
  
  util.makeDocDir url, handleError sendError(500), (docDir) ->
    util.fetchDocument url, handleError sendError(404), (doc) ->
      continuation = ->
        resultStream = util.convert format, doc, docDir, handleError sendError(500), (resultStream) ->
          resultStream.on 'error', sendError(500)
          console.log("Converting '#{url}' to #{format.name}.")
          if format.name is 'pdf'
            util.generatePdf resultStream, docDir, handleError sendError(500), (pdfStream) ->
              res.header('Content-Type', 'application/pdf')
              pdfStream.pipe(res)
          else
            res.header('Content-Type', format.mime)
            resultStream.pipe(res)
            
      if format.downloadResources
        util.downloadResources doc, docDir, handleError sendError(500), ->
          console.log("Downloaded resources for document '#{url}'")
          continuation()
      else
        continuation()


convertShower = (res, url, callback) ->
  sendError = sendHttpError res
  util.fetchDocument url, handleError sendError(404), (doc) ->
    new ShowerRenderer(doc).render (html, resources) ->
      callback(null, html)


# Routes
# ======

app.get "/shower", (req, res) ->
  convertShower res, "http://substance.io/documents/michael/data-js-slides", (err, html) ->
    res.send(html)


app.get /^\/[a-zA-Z0-9_]+\.([a-z0-9]+)/, (req, res) ->
  extension = req.params[0]
  {url} = req.query
  handleConversion(res, url, formats.byExtension[extension])

# Fallback for those who have JavaScript disabled
app.get '/render', (req, res) ->
  {url,format} = req.query
  handleConversion(res, url, formats.byExtension[format])


# Start the fun
# =============

# Catch errors that may crash the server
process.on 'uncaughtException', (err) ->
  console.error("Caught exception: #{err}")

app.listen(4004)
console.log("Letterpress is listening at http://localhost:4004")
