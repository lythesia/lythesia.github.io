(function($){
  // Blink
  var timer = setInterval(function() {
    var title = $(document).attr('title'),
       $title = $('#header a:first');
    var new_title = title.indexOf("_") < 0 ? ">> _" : ">>  ";
    document.title = new_title;
    $title.html(new_title);
  }, 500);
})(jQuery);
