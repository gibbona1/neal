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

  var latestPlotDims  = null;
  var latestViewRange = null; // [start, end] seconds, x-axis range the panel currently shows
  var latestPlayStart = 0;    // seconds, absolute start time of the audio currently loaded
  var tickerHandle     = null;

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

    if (!latestPlotDims || !latestViewRange) return;

    var dims = getPlotDimensions();
    var audioElement = document.getElementById("my_audio_player");
    if (!dims || !audioElement) return;

    var duration = audioElement.duration;
    if (!duration || isNaN(duration)) return;

    var viewStart = latestViewRange[0];
    var viewSpan  = latestViewRange[1] - latestViewRange[0];
    if (!viewSpan) return;

    // Absolute time the audio is currently at, in the same coordinate space
    // as the plot's x-axis, then expressed as a fraction of the panel's
    // currently visible x-range (which may be a live selection box smaller
    // than the panel, or the panel's own full width when zoomed/unzoomed).
    var currentAbsTime = latestPlayStart + audioElement.currentTime;
    var frac = (currentAbsTime - viewStart) / viewSpan;
    frac = Math.min(Math.max(frac, 0), 1);

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
    latestPlotDims  = message.plotDimensions;
    latestViewRange = message.viewRange;
    latestPlayStart = message.playStart || 0;
    updateTickerPosition();
    if (!tickerHandle)
      tickerHandle = setInterval(updateTickerPosition, 50);
  });

  $(document).on("timeupdate play pause seeked", "#my_audio_player", updateTickerPosition);
});
