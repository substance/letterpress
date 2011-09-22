fs = require('fs')


idGen = (prefix) ->
  i = 0
  return -> prefix + (i++)

exports.ShowerRenderer = (doc) ->
  
  getId = idGen('slide')
  
  content = ""
  renderers = {
    "/type/document": (node, parent, level) ->
      # TODO: not every presentation is made by Michael Aufreiter ;-)
      content = """
      <header class="caption">
        <h1>Data.js</h1>
        <p>Michael Aufreiter, Substance.io</p>
      </header>
      """
      
      node.all('children').each (child) ->
        content += renderers[child.type._id](child, node)
      
      template = fs.readFileSync(__dirname+ '/../public/shower/template.html', 'utf-8')
      return template.replace('{{{{content}}}}', content)
    
    "/type/article": (node, parent, level, callback) ->
      renderers["/type/document"](node, parent, level, callback)
    
    "/type/section": (node, parent, level, callback) ->
      # Is there an image?
      image = node.all('children').select((n) -> n.type._id == "/type/image").first()

      content = """
      <div class="slide#{if image then ' bg' else ''}" id="#{getId()}"><div>
        <section>
      """
      
      # Convention: Hide header if image caption equals section name
      if !image || image.get('caption') != node.get('name')
        content += """
        <header>
          <h2>#{node.get('name')}</h2>
        </header>
        """
      
      node.all('children').each (child) ->
        content += renderers[child.type._id](child, node)

      content += "</section></div></div>"
      return content


    "/type/text": (node, parent, level, callback) ->
      return node.get('content').trim();
        
    "/type/code": (node, parent, level, callback) ->
      content = "<pre>"
      node.get('content').split("\n").forEach (line) ->
        content += "<code>#{line}</code>"
      content += "</pre>"
      
    "/type/image": (node, parent, level, callback) ->
      return "<img src=\"#{node.get('original_url')}\" alt=\"\">"
      
    "/type/resource": (node, parent, level, callback) ->
      return ""
    
    "/type/quote": (node, parent, level, callback) ->

      return """
      <blockquote cite="#{node.get('author')}">
        <p>#{node.get('content')}</p>
      </blockquote>
      """
  }
  
  # Export Interface
  {
    render: (callback) -> 
      callback(renderers[doc.type._id](doc, null, 0), [])
  }
