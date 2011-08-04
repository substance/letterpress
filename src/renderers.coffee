sys = require('sys')
p = (x) -> sys.puts(sys.inspect(x))
sax = require('sax')
$ = null
fs = require('fs')
_ = require('underscore')
async = require('async')
Data = require('data')


unescape = (text) ->
  String(text||'').replace(/&amp;/g, '\\&').replace(/&lt;/g, '<').replace(/&gt;/g, '>')
                  .replace(/&quot;/g, '"').replace(/&apos;/g, "'").replace(/&nbsp;/g, " ")

# Sanitize
sanitize = (html) ->
  unescape(html)
    .replace(/\\/g, '$\\backslash$') # escape \
    .replace(/\"/g, '``') # escape "
    .replace(/_/g, '\\_') # escape _
    .replace(/&/g, '\\&') # escape &
    .replace(/#/g, '\\#') # escape #
    .replace(/~/g, '\\verb') # escape ~
    .replace(/\$/g, '\\$') # escape $
    .replace(/%/g, '\\%') # escape %
    .replace(/\^/g, '\\^') # escape ^
    .replace(/\{/g, '\\{') # escape {
    .replace(/\}/g, '\\}') # escape }

exports.LatexRenderer = (doc) ->
  
  template = fs.readFileSync(__dirname+ '/../templates/latex/lncs.tex', 'utf-8')
  content = ""
  resources = []

  renderHTML = (html, callback) ->
    parser = sax.parser(false, {
      # trim: true
    });
    
    res = "";
    
    parser.onerror = (e) ->
      # an error happened.
      
    parser.ontext = (t) ->
      res += sanitize(t)
      
    parser.onopentag = (node) ->      
      switch (node.name)
        when 'A'
          res += " \\href{#{node.attributes.href}}{"
        when 'P', 'BODY'
          res += "\n\n"
        when 'I', 'EM'
          res += " \\textit{"
        when 'B', 'STRONG'
          res += " \\textbf{"
        when 'CODE'
          res += " \\texttt{"
        when 'UL'
          res += "\\begin{itemize}"
        when 'OL'
          res += "\\begin{enumerate}"
        when 'LI'
          res += "\n\\item "
        when 'BR'
          res += "\\\\\n"
        else
          # skip for now
    
    parser.onclosetag = (node) ->
      switch(node)
        when 'A', 'I', 'EM', 'B', 'STRONG', 'CODE'
          res += "}"
        when 'UL'
          res += "\\end{itemize}\n"
        when 'OL'
          res += "\\end{enumerate}\n"
        else
          # skip for now
    parser.onend = ->
      # parser stream is done, and ready to have more stuff written to it.
    
    
    parser.write("<p>"+html+"</p>").close()
    callback(res)
  
  renderers = {
    "/type/document": (node, parent, level, callback) ->
      children = node.all('children')
      
      res = template
      content = ""
      
      childres = {}
      # Render childs, asynchronously and in parallel
      async.forEach children.keys(), (childKey, callback) ->
        child = children.get(childKey)
        renderers[child.type._id] child, null, level+1, (latex) ->
          childres[childKey] = latex
          callback()
      , ->
        # Merge results
        _.each children.keys(), (child) ->
          content += childres[child]
        
        # res += "\n\\end{document}\n"
        
        # Render template
        res = res.replace("####title####", (node.get('title') || "").trim())
                 .replace("####author####", node.get('creator').get('name'))
                 .replace("####content####", content)
                 .replace("####abstract####", (node.get('lead') || "").trim())
                 
        callback(res)
    
    "/type/story": (node, parent, level, callback) ->
      renderers["/type/document"](node, parent, level, callback)

    "/type/conversation": (node, parent, level, callback) ->
      renderers["/type/document"](node, parent, level, callback)
    
    "/type/article": (node, parent, level, callback) ->
      renderers["/type/document"](node, parent, level, callback)

    "/type/manual": (node, parent, level, callback) ->
      renderers["/type/document"](node, parent, level, callback)

    "/type/qaa": (node, parent, level, callback) ->
      renderers["/type/document"](node, parent, level, callback)
    
    "/type/section": (node, parent, level, callback) ->
      children = node.all('children')
      result = ""
      
      switch(level)
        when 1
          cmd = "section"
        when 2
          cmd = "subsection"
        when 3
          cmd = "subsubsection"
      
      result += "\n\n\\"+cmd+"{#{node.get('name').trim()}}\n"
      
      childres = {}
      # Render childs, asynchronously and in parallel
      async.forEach(children.keys(), (childKey, callback) ->
        child = children.get(childKey)
        renderers[child.type._id](child, node, level+1, (latex) ->
          childres[childKey] = latex
          callback()
        )
        
      , ->
        # Merge results
        _.each children.keys(), (child) ->
          result += childres[child]
        callback(result)
      )

    "/type/text": (node, parent, level, callback) ->
      renderHTML node.get('content').trim(), (latex) ->
        callback(latex)
        
    "/type/code": (node, parent, level, callback) ->
      result = "\\begin{verbatim}\n"
      result += unescape(node.get('content'))+"\n"
      result += "\\end{verbatim}\n"
      callback(result)
      
    "/type/image": (node, parent, level, callback) ->
      resources.push(node.get('original_url'));
      
      # result = 
      callback """
               \n\\begin{figure}[htb]
               \\begin{center}
               \\leavevmode
               \\includegraphics[width=1.0\\textwidth]{./tmp/#{doc.html_id}/resources/#{resources.length-1}.png}
               \\end{center}
               \\caption{#{sanitize(node.get('caption'))}}
               \\label{fig:awesome_image}
               \\end{figure}
               """
      
    "/type/resource": (node, parent, level, callback) ->
      # Quotes are skipped
      callback("") # Is there some Latex markup for quotations?  
    
    "/type/quote": (node, parent, level, callback) ->
      # Quotes are skipped
      callback("") # Is there some Latex markup for quotations?
    
    "/type/question": (node, parent, level, callback) ->
      # Questions are skipped
      callback("")
      
    "/type/answer": (node, parent, level, callback) ->
      # Answers are skipped
      callback("")

    "/type/visualization": (node, parent, level, callback) ->
      # Not yet supported
      callback("")
  }
  
  # Export Interface
  {
    render: (callback) ->
      # Traverse the document
      renderers[doc.type._id](doc, null, 0, (html) ->
        callback(html, resources);
      )
  }


# PandocRenderer
# ==============
#
# Converts a substance document to Pandoc's JSON representation.
# Pandoc is a document converter writter in Haskell that can convert between a
# plethora of formats, including LaTeX, HTML and Markdown.
# Pandoc can also serialize (and deserialize) it's internal representation of
# documents from/to JSON.
#
# * [Pandoc's internal representation](https://github.com/jgm/pandoc-types/blob/master/Text/Pandoc/Definition.hs)
# * [The rules for deriving a JSON representation of a Haskell data structure](http://hackage.haskell.org/packages/archive/json/0.4.3/doc/html/src/Text-JSON-Generic.html#toJSON)

class exports.PandocRenderer

  constructor: (@doc) ->
  
  render: ->
    # Traverse the document
    @renderPart(@doc, 0)

  renderPart: (node, args...) ->
    @[node.type._id](node, args...)

  inlineHtml: (html) -> { RawInline: ['html',html.trim()] }

  blockHtml: (html) -> { RawBlock: ['html',html.trim()] }

  nullAttr: -> ['', [], []]

  renderChildren: (node, level) ->
    renderedChildren = _.map node.all('children').values(), (child) =>
      @renderPart child, level+1
    flatten = (list) -> _.reduce list, ((a, b) -> a.concat b), []
    flatten(renderedChildren)

  "/type/document": (node, level) ->
    renderedChildren = @renderChildren(node, level)
    
    # render abstract as a paragraph
    renderedChildren.unshift({ Para: [@inlineHtml lead] }) if lead = node.get('lead')
    
    date = node.get('published_on')
    date = Date.parse(date) if date
    
    formatDate = (d) ->
      #"#{d.getMonth()+1}/#{d.getDate()}/#{d.getFullYear()}"
      # unfortunately, Date.prototype is broken on my machine
      ""
    
    [
      {
        docTitle: [@inlineHtml(node.get('title') or "")]
        docAuthors: [[@inlineHtml(node.get('creator').get('name'))]]
        docDate: if date then [@inlineHtml formatDate(date)] else []
      }
    ,
      renderedChildren
    ]
  
  "/type/story":        (node, level) -> @["/type/document"](node, level)
  "/type/conversation": (node, level) -> @["/type/document"](node, level)
  "/type/article":      (node, level) -> @["/type/document"](node, level)
  "/type/manual":       (node, level) -> @["/type/document"](node, level)
  "/type/qaa":          (node, level) -> @["/type/document"](node, level)
  
  "/type/section": (node, level) ->
    level = Math.min(level, 6)
    
    renderedChildren = @renderChildren(node, level)
    renderedHeader = { Header: [level, [@inlineHtml node.get('name')]] }
    renderedChildren.unshift(renderedHeader)
    
    renderedChildren

  "/type/text": (node, level) ->
    [@blockHtml node.get('content').trim()]
      
  "/type/code": (node, level) ->
    [{ CodeBlock: [@nullAttr, node.get('content') ] }]
    
  "/type/image": (node, level) ->
    # In Pandoc, images are inline elements => wrap them with a paragraph
    # caption
    [{ Para: [{ Image: [[@inlineHtml(node.get('caption'))], [node.get('url'), []]] }] }]
    
  "/type/resource": (node, level) ->
    [@blockHtml "<p>Resources are not yet implemented.</p>"]
  
  "/type/quote": (node, level) ->
    [
      { BlockQuote: [@blockHtml(node.get('content'))] }
      { Para: ['EmDash', 'Space', @inlineHtml(node.get('author'))] }
    ]
  
  "/type/question": (node, level) ->
    [@blockHtml "<p>Questions are not yet implemented.</p>"]
    
  "/type/answer": (node, level) ->
    [@blockHtml "<p>Answers are not yet implemented.</p>"]

  "/type/visualization": (node, level) ->
    [@blockHtml "<p>Visualizations are not supported.</p>"]
