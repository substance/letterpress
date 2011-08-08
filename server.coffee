express = require 'express'
util    = require './src/util'


# Express.js Configuration
# ========================

app = express.createServer()

app.configure ->
  app.use(app.router)
  app.use(express.static("#{__dirname}/public", { maxAge: 41 }))


# Helpers
# =======

# Looks almost like Haskell's partial application :-)

sendHttpError = (res) -> (httpCode, errorToSend) -> (actualError) ->
  error = errorToSend or actualError
  res.statusCode = httpCode
  res.end "Error: #{error.message or error}"

handleError = (errorCallback, successCallback) -> (err, args...) ->
  if err
    errorCallback err
  else
    successCallback args...


# Routes
# ======

formats =
  latex:    { mime: 'text/plain' } # actually text/x-latex
  markdown: { mime: 'text/plain' }
  html:     { mime: 'text/html' }
  json:     { mime: 'application/json' }

app.get '/render', (req, res) ->
  {format, url} = req.query
  res.charset = 'utf8'
  sendError = sendHttpError res
  
  unless formats[format]
    # bad request
    return sendError(400)(new Error("Unknown target format."))
  util.fetchDocument url, handleError sendError(404), (doc) ->
    util.convert format, doc, handleError sendError(500), (result) ->
      console.log("Converted '#{url}' to #{format}.")
      res.header('Content-Type', formats[format].mime)
      res.end(result)

# On the fly PDF generation
app.get '/pdf', (req, res) ->
  {url} = req.query
  sendError = sendHttpError res
  util.fetchDocument url, handleError sendError(404), (doc) ->
    util.downloadResources doc, handleError sendError(500), ->
      console.log("Downloaded resources for document '#{url}'")
      util.convert 'latex', doc, handleError sendError(500), (latex) ->
        console.log("Converted #{url} to latex for PDF generation.")
        pdfError = new Error """
          An error occurred during PDF generation. Be aware PDF
          export is highly experimental. Problems occur when
          special characters are used for example. Please help
          improving all this by reporting your particular problem to
          <a href=\"mailto:info@substance.io\">info@substance.io</a>.
          """
        util.generatePdf latex, url, handleError sendError(500, pdfError), (pdf) ->
          res.header('Content-Type', 'application/pdf')
          res.end(pdf)


# Start the fun
# =============

# Catch errors that may crash the server
process.on 'uncaughtException', (err) ->
  console.log("Caught exception: #{err}")

app.listen(4004)
console.log('Letterpress is listening at http://localhost:4004')
