$(document).ready(function(){
    // id of the plot
    $("#specplot_front").mousemove(function(e){ 
      var hover     = $("#hover_info");
      var winwidth  = $( window ).width();
      var winheight = $( window ).height();
      
      var body = document.body,
          html = document.documentElement;

      var height = Math.max( body.scrollHeight, 
                             body.offsetHeight,
                             html.clientHeight, 
                             html.scrollHeight, 
                             html.offsetHeight );
                             
      hover.attr("style", "");
      //stop hover info going off edge of screen
      if(e.pageX + hover.width() <= $( window ).width()) {
        hover.css({"left": (e.pageX + 5) + "px"});
      } 
      else {
        hover.css({"right": (winwidth 
                             - hover.width()/4
                             - e.pageX - 5) + "px"});
      }
      if(e.pageY + hover.height() <= $(this).height()) {
        hover.css({"top": (e.pageY + 5) + "px"});
      } 
      else {
        hover.css({"bottom": (height 
                              + $(this).offset().top
                              - hover.height()
                              - e.pageY - 5) + "px"});
      }
      hover.show();
    });     
  });