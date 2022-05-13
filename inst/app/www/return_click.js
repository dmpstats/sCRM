/*Simulate click to add winfarm action on return key*/
$(document).keyup(function(event) {
    if ($("#wf-label").is(":focus") && (event.keyCode == 13)) {
        $("#btn-add-wf").click();
    }
});
