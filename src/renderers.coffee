sys = require('sys')
p = (x) -> sys.puts(sys.inspect(x))
jsdom = require('jsdom')
$ = null
fs = require('fs')
_ = require('underscore')
async = require('async')
Data = require('data')

exports.LatexRenderer = (doc) ->
  html_renderer = (html, callback) ->
    # Render HTML Node
    render = (n) ->
      $n = $(n)

      unless n.nodeName?
        if n.html?
          return " " + n.html().trim() + " "
        if n.toString?
          return " " + n.toString().trim() + " "
        return ''

      switch n.nodeName
        when 'A'
          "\\href{#{$n.attr 'href'}}{#{(render $n).trim()}}"
        # Not considering line breaks due to some issues
        # when 'BR'
        #   "\\linebreak\n"
        when 'P', 'BODY'
          $n.children().replaceWith ->
            render @
          $n.text() + "\n"
        when 'I'
          "\\textit{#{(render $n).trim()}}"
        when 'B'
          "\\textbf{#{(render $n).trim()}}"
        else
          "" # skip for now
          #p "couldn't match #{n.nodeName}, inlining html:"
          #p $n.html()
    
    result = 'aaa'
    jsdom.env html,
      ['http://code.jquery.com/jquery-1.5.min.js'],
      (err, w) ->
        $ = w.$
        callback(render(w.$("body")[0]))

  renderers = {
    "/type/document": (node, parent, level, callback) ->
      children = node.all('children')
      
      res = "\\documentclass[12pt,leqno]{memoir}\n"
      res += "\\usepackage{hyperref}\n"
      res += "\\begin{document}\n"
      res += "\\title{#{node.get('title').trim()}}"
      
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
          res += childres[child]
        
        res += "\n\\end{document}\n"
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
      res = ""
      
      res += "\\section{#{node.get('name')}}\n"
      
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
          res += childres[child]
        callback(res)
      )

    "/type/text": (node, parent, level, callback) ->
      html_renderer node.get('content').trim(), (latex) ->
        callback(latex)
        
    "/type/code": (node, parent, level, callback) ->
      res = "\\begin{verbatim}\n"
      res += node.get('content')+"\n"
      res += "\\end{verbatim}\n"
      callback(res)
      
    "/type/image": (node, parent, level, callback) ->
      # Images are skipped
      callback("")
      
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