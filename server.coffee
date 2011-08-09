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


# Handlers
# ========

formats =
  latex:     { mime: 'text/plain' } # actually text/x-latex
  html:      { mime: 'text/html' }
  json:      { mime: 'application/json' }
  context:   { mime: 'text/plain' }
  texinfo:   { mime: 'text/plain' }
  markdown:  { mime: 'text/plain' }
  textile:   { mime: 'text/plain' }
  mediawiki: { mime: 'text/plain' }
  rst:       { mime: 'text/plain' }
  man:       { mime: 'text/plain' }
  docbook:   { mime: 'application/docbook+xml' }
  org:       { mime: 'text/plain' }

handleTextFormat = (res, url, format) ->
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
handlePdf = (res, url) ->
  sendError = sendHttpError res
  util.fetchDocument url, handleError sendError(404), (doc) ->
    util.downloadResources doc, handleError sendError(500), ->
      console.log("Downloaded resources for document '#{url}'")
      util.convert 'latex', doc, handleError sendError(500), (latex) ->
        console.log("Converted '#{url}' to latex for PDF generation.")
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


# Routes
# ======

shortNameFromUrl = (url) ->
  last = (arr) -> arr[arr.length - 1]
  last(url.replace(/\/$/, '').split('/')).replace(/[^A-Za-z0-9_]/g, '_')

app.get '/render', (req, res) ->
  {url,format} = req.query
  res.redirect "/#{shortNameFromUrl(url)}.#{format}?url=#{encodeURIComponent(url)}"

app.get /^\/[a-zA-Z0-9_]+\.([a-z]+)/, (req, res) ->
  format = req.params[0]
  {url}  = req.query
  if format is 'pdf'
    handlePdf(res, url)
  else
    handleTextFormat(res, url, format)


# Start the fun
# =============

# Catch errors that may crash the server
process.on 'uncaughtException', (err) ->
  console.log("Caught exception: #{err}")

app.listen(4004)
console.log('Letterpress is listening at http://localhost:4004')
