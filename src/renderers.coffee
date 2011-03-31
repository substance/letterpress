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
        when 'BR'
          "\\linebreak\n"
        when 'P', 'BODY'
          $n.children().replaceWith ->
            render @
          $n.text() + "\n"
        when 'I'
          "\\textit{#{(render $n).trim()}}"
        when 'B'
          "\\textbf{#{(render $n).trim()}}"
        else
          p "couldn't match #{n.nodeName}, inlining html:"
          p $n.html()
    
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
      
      res += "\\section{#{node.get('name')}}"
      
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
  }
  
  # Export Interface
  {
    render: (callback) ->
      # Traverse the document
      renderers[doc.type._id](doc, null, 0, (html) ->
        callback(html)
      )
  }