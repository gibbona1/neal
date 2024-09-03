$(document).on("shiny:connected", function() {
  
  function logToShiny(message) {
    Shiny.setInputValue("js_log", message, {priority: "event"});
  }
  
  // Custom message handler to update the ticker position
  Shiny.addCustomMessageHandler("updateTicker", function(message) {
    var plotDims = message.plotDimensions;
    var total_height = message.total_height;
    var total_width = message.total_width;
    if (plotDims) {
      var audioElement = $("#my_audio_player")[0];
      var duration = audioElement.duration;
      var currentTime = audioElement.currentTime;
      if (duration > 0) {
        var leftOffset = total_width * plotDims.left + (total_width * plotDims.width * currentTime / duration);
        $("#ticker").css({top: total_height * plotDims.top, left: leftOffset + "px", height: total_height * plotDims.height});
      }
    }
  });
});
