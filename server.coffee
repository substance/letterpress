express = require 'express'
util    = require './src/util'
formats = require './src/formats'


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
  util.fetchDocument url, handleError sendError(404), (doc) ->
    continuation = ->
      util.convert format, doc, url, handleError sendError(500), (result) ->
        console.log("Converted '#{url}' to #{format.name}.")
        if format.name is 'pdf'
          util.generatePdf result, url, handleError sendError(500), (pdf) ->
            res.header('Content-Type', 'application/pdf')
            res.end(pdf)
        else
          res.header('Content-Type', format.mime)
          res.end(result)
    if format.downloadResources
      util.downloadResources doc, handleError sendError(500), ->
        console.log("Downloaded resources for document '#{url}'")
        continuation()
    else
      continuation()


# Routes
# ======

shortNameFromUrl = (url) ->
  last = (arr) -> arr[arr.length - 1]
  last(url.replace(/\/$/, '').split('/')).replace(/[^A-Za-z0-9_]/g, '_')

app.get '/render', (req, res) ->
  {url,format} = req.query
  format = formats.byName[format]
  res.redirect "/#{shortNameFromUrl(url)}.#{format.extension}?url=#{encodeURIComponent(url)}"

app.get /^\/[a-zA-Z0-9_]+\.([a-z0-9]+)/, (req, res) ->
  extension = req.params[0]
  {url} = req.query
  handleConversion(res, url, formats.byExtension[extension])


# Start the fun
# =============

# Catch errors that may crash the server
process.on 'uncaughtException', (err) ->
  console.log("Caught exception: #{err}")

app.listen(4004)
console.log("Letterpress is listening at http://localhost:4004")
