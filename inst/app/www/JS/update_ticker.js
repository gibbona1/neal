$(document).on("shiny:connected", function() {

  function getPlotDimensions() {
    var specplotDiv = $("#specplot");

    if (specplotDiv.length > 0) {
      return {
        total_width:  specplotDiv.width(),
        total_height: specplotDiv.height()
      };
    }
    return null;
  }

  var latestPlotDims = null;
  var tickerHandle    = null;

  function updateTickerPosition() {
    var ticker = $("#ticker");
    if (ticker.length === 0) {
      // Ticker removed from DOM (checkbox turned off) - stop polling
      if (tickerHandle) {
        clearInterval(tickerHandle);
        tickerHandle = null;
      }
      return;
    }

    if (!latestPlotDims) return;

    var dims = getPlotDimensions();
    var audioElement = document.getElementById("my_audio_player");
    if (!dims || !audioElement) return;

    var duration = audioElement.duration;
    if (!duration || isNaN(duration)) return;

    var frac = audioElement.currentTime / duration;

    var leftOffset = dims.total_width  * (latestPlotDims.left + latestPlotDims.width * frac);
    var topOffset  = dims.total_height * latestPlotDims.top;
    var height     = dims.total_height * latestPlotDims.height;

    ticker.css({
      left:   Math.round(leftOffset) + "px",
      top:    Math.round(topOffset)  + "px",
      height: Math.round(height)     + "px"
    });
  }

  // Custom message handler to receive updated ticker geometry from the server
  Shiny.addCustomMessageHandler("updateTicker", function(message) {
    latestPlotDims = message.plotDimensions;
    updateTickerPosition();
    if (!tickerHandle)
      tickerHandle = setInterval(updateTickerPosition, 50);
  });

  $(document).on("timeupdate play pause seeked", "#my_audio_player", updateTickerPosition);
});
