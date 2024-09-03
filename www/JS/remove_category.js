document.getElementById("label_ui").addEventListener("keyup", 
    function(event) {
        event.preventDefault();
        if (event.keyCode === 8) {
        document.getElementById("remCategory").click();
        }
    });