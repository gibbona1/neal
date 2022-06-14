document.getElementById("otherCategory").addEventListener("keyup", 
    function(event) {
        event.preventDefault();
        if (event.keyCode === 13) {
        document.getElementById("addCategory").click();
        }
    });