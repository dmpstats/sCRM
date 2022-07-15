/*Close dropdown menu with windfarm addition widgets*/
$( document ).ready(function() {
  Shiny.addCustomMessageHandler('close_drpdwn-add-wf', function(arg) {
     $('html').click();
  })
});



$( document ).ready(function() {
  var section = document.getElementsByClassName('content');
  section[0].setAttribute('id', 'waiter-content');
});