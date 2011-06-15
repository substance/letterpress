sys = require('sys')
p = (x) -> sys.puts(sys.inspect(x))
sax = require('sax')
$ = null
fs = require('fs')
_ = require('underscore')
async = require('async')
Data = require('data')
Encoder = require('./encoder').Encoder

template = fs.readFileSync(__dirname+ '/../templates/latex/lncs.tex', 'utf-8')


# Unescape HTML entities
unescape = (html) ->
  return String(html||'').replace(/&amp;/g, '&').replace(/&lt;/g, '<').replace(/&gt;/g, '>')
                                    .replace(/&quot;/g, '"').replace(/&apos;/g, "'").replace(/&nbsp;/g, " ");

exports.LatexRenderer = (doc) ->
  
  # Why?
  content = ""

  renderHTML = (html, callback) ->
    parser = sax.parser(false, {
      trim: true
    });
    
    res = "";
    
    parser.onerror = (e) ->
      # an error happened.
      
    parser.ontext = (t) ->
      # res += Encoder.htmlDecode(t)
      # res += t
      res += unescape(t)
      
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
        when 'BR'
          res += "\\\\\n"
        else
          # skip for now
    
    parser.onclosetag = (node) ->
      switch(node)
        when 'A', 'I', 'EM', 'B', 'STRONG'
          res += "}"
        else
          # skip for now
    parser.onend = ->
      # parser stream is done, and ready to have more stuff written to it.
    
    parser.write(html).close()
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
                 .replace("####abstract####", node.get('lead'))
                 
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
      
      result += "\n\n\\section{#{node.get('name').trim()}}\n"
      
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
      result += node.get('content')+"\n"
      result += "\\end{verbatim}\n"
      callback(result)
      
    "/type/image": (node, parent, level, callback) ->
      # Images are skipped
      callback("")
      
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
        callback(html)
      )
  }