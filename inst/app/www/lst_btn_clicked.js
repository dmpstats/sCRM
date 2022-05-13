$( document ).ready(function() {

  $(document).on('click', 'button', function () {
    //Shiny.onInputChange('last_btn',this.id);
    Shiny.onInputChange('js-btn-clicked',this.id + '|' + (new Date()).getTime());
    });
 
});



