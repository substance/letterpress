sys = require('sys')
p = (x) -> sys.puts(sys.inspect(x))
sax = require('sax')
$ = null
fs = require('fs')
_ = require('underscore')
async = require('async')
Data = require('data')

# Sanitize
sanitize = (html) ->
  return String(html||'').replace(/&amp;/g, '&').replace(/&lt;/g, '<').replace(/&gt;/g, '>')
                         .replace(/&quot;/g, '"').replace(/&apos;/g, "'").replace(/&nbsp;/g, " ")
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
        when 'A', 'I', 'EM', 'B', 'STRONG'
          res += "}"
        when 'UL'
          res += "\\end{itemize}\n"
        when 'OL'
          res += "\\end{enumerate}\n"
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
      result += node.get('content')+"\n"
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