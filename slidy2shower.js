function idGen (prefix) {
  var i = 0;
  return function () {
    return prefix + (i++);
  };
}
var getId = idGen('slide');

$('.layout').remove();
$('#header').wrap('<header class="caption" />');
$(document.body).addClass('list');
$('.slide').each(function () {
  // Scripts (e.g. for spam-secure email links) shouldn't be executed twice
  $(this).find('script').remove();
  $(this).html('<div><section>'+$(this).html()+'</section></div>');
});
$('.slide h1, .slide h2, .slide h3').wrap($('<header />'));
$('h1,h2,h3').each(function () {
  var id = $(this).attr('id');
  if (id) {
    $(this)
      .removeAttr('id')
      .closest('.slide').attr('id', id);
  }
});
$('.slide').each(function () {
  if (!$(this).attr('id')) {
    $(this).attr('id', getId());
  }
});

// Fix contents
$('.slide li > p').each(function () {
  $(this.firstChild).unwrap();
});
$('pre code').each(function () {
  var code = $(this).html();
  code = code.split('\n')
             .map(function (s) {
               if (s === '') s = '&nbsp;';
               return '<code>'+s+'</code>'
             })
             .join('\n');
  $(this).closest('pre').html(code);
});
$('img').each(function () {
  var slide   = $(this).closest('.slide')
  ,   title   = slide.find('h1,h2,h3').first()
  ,   caption = $(this).next();
  
  slide.addClass('bg');
  if (title.text() === caption.text()) {
    caption.remove();
  }
});

$('<div class="progress"><div></div></div>').appendTo(document.body);
