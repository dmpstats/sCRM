$( document ).ready(function() {
  
//  $(document).on('click', 'button,a', function(e) {
//    if(e.target.id.length == 0) { return } // No ID, not ours.
//    if(e.target.nodeName == 'A' &&           
//    typeof e.target.href != 'undefined' && // If it's a link  
//    e.target.href.length > 0) {              // with an href
//       return; }                      // don't mess with it.
//       Shiny.onInputChange('last_btn', e.target.id + '-' + (new Date()).getTime());
//    });
    
  $(document).on('click', 'button', function () {
    //Shiny.onInputChange('last_btn',this.id);
    Shiny.onInputChange('js-btn-clicked',this.id + '_' + (new Date()).getTime());
    });
 
});



//'.needed'