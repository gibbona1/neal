var id = setInterval(audio_pos, 100);
function audio_pos() {
    var audio = document.getElementById("my_audio_player");
    var curtime = audio.currentTime;
    console.log(audio);
    Shiny.onInputChange("get_time", curtime);
};