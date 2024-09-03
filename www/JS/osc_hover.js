$(document).ready(function(){
    // id of the plot
    $("#oscplot").mousemove(function(e){ 

      // ID of uiOutput
      $("#hover_info_osc").show();         
      $("#hover_info_osc").css({             
        top: (e.pageY - 15) + "px",             
        left: (e.pageX + 5) + "px"
      });     
    });     
  });