$(document).on("shiny:connected", function() {
  
  function logToShiny(message) {
    Shiny.setInputValue("js_log", message, {priority: "event"});
  }
  
  function getPlotDimensions() {
    var specplotDiv = $("#specplot");
  
    if (specplotDiv.length > 0) {
      // Get the width and height of the specplot element
      var total_width = specplotDiv.width();
      var total_height = specplotDiv.height();
  
      // Print the dimensions
      // logToShiny("Width: " + total_width + ", Height: " + total_height);
      return {total_width: total_width, total_height: total_height};
    }
    return null;
  }
  
  $(window).resize(function() {
    getPlotDimensions();
  });
  
  // Custom message handler to update the ticker position
  Shiny.addCustomMessageHandler("updateTicker", function(message) {
    logToShiny("here");
    var plotDims = message.plotDimensions;
    var dims = getPlotDimensions();
    logToShiny(JSON.stringify(dims));
    if (dims && plotDims) {
      var total_height = dims.total_height;
      var total_width  = dims.total_height;
      var audioElement = $("#my_audio_player")[0];
      // Function to update ticker position based on current time
      function updateTicker() {
        var duration = audioElement.duration;
        var currentTime = audioElement.currentTime;
        logToShiny(JSON.stringify({currentTime: currentTime, duration: duration}));

        if (duration > 0) {
          var leftOffset = Math.floor(total_width * (plotDims.left + (plotDims.width * currentTime / duration)));
          var attrs = {top: Math.floor(total_height * plotDims.top), 
                            left: Math.floor(leftOffset), 
                            height: Math.floor(total_height * plotDims.height)
          };
          logToShiny(JSON.stringify(attrs));
          $("#ticker").css(attrs);
        }
      }

      // Update ticker position every 100 ms
      setInterval(updateTicker, 1000);
    }
  });
});
