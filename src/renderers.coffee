sys = require('sys')
p = (x) -> sys.puts(sys.inspect(x))
jsdom = require('jsdom')
$ = null
fs = require('fs')
Data = require('data')

exports.LatexRenderer = (doc) ->
  html_renderer = (html) ->
    
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
          "\\href{#{$n.attr 'href'}}{#{render $n.html()}}"
        when 'BR'
          "\\linebreak\n"
        when 'P', 'BODY'
          $n.children().replaceWith ->
            render @
          $n.text() + "\n"
        when 'I'
          "\\textit{#{render $n}}"
        when 'B'
          "\\textbf{#{render $n}}"
        else
          p "couldn't match #{n.nodeName}, inlining html:"
          p $n.html()
    
    result = 'aaa'
    jsdom.env html,
      ['http://code.jquery.com/jquery-1.5.min.js'],
      (err, w) ->
        $ = w.$
        result = render(w.$("body")[0])
    result

  renderers = {
    "/type/document": (node, parent, level) ->
      children = node.all('children')
      
      res = "\\documentclass[12pt,leqno]{memoir}\n"
      res += "\\usepackage{hyperref}\n"
      res += "\\begin{document}\n"
      res += "\\title{#{node.get('title').trim()}}"
      
      if (children)
        children.each (child, key, index) ->
          res += renderers[child.type._id](child, node, level+1)
      
      res += "\n\\end{document}\n"
      return res
    
    "/type/story": (node, parent, level) ->
      renderers["/type/document"](node)

    "/type/conversation": (node, parent, level) ->
      renderers["/type/document"](node)
      
    "/type/article": (node, parent, level) ->
      renderers["/type/document"](node)

    "/type/manual": (node, parent, level) ->
      renderers["/type/document"](node)

    "/type/qaa": (node, parent, level) ->
      renderers["/type/document"](node)
    
    "/type/section": (node, parent, level) ->
      res = ""
      children = node.all('children')
      
      res += "\\section{\n"
      if (children)
        node.all('children').each (child, key, index) ->
          res += renderers[child.type._id](child, node, level+1)
      res += "}\n\n"
      return res

    "/type/text": (node, parent, level) ->
      # html_renderer(node.get('content').trim())
      node.get('content').trim()
  }
  
  # Export Interface
  {
    render: ->
      # Traverse the document
      renderers[doc.type._id](doc, null, 0).trim();
  }