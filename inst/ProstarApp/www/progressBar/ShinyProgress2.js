Shiny.addCustomMessageHandler("updateprogress",
                              function(data) {
                                  
                                  $el = $("#"+data.id);
                                  
                                
                                  if(data.hasOwnProperty('color')) {
                                      $el.removeClass("progress-bar-standard progress-bar-info progress-bar-success progress-bar-danger progress-bar-warning");
                                      $el.addClass("progress-bar-"+data.color);
                                  };
                                  if(data.hasOwnProperty('striped')) {
                                      $el.toggleClass('progress-bar-striped', data.striped);
                                  };
                                  if(data.hasOwnProperty('active')) {
                                      $el.toggleClass('active', data.active);
                                  };
                                  if(data.hasOwnProperty('vertical')) {
                                      $el.toggleClass('vertical', data.vertical);
                                  };
                                  
                                   if(data.hasOwnProperty('value')) {
                                      $el.css('width', data.value+'%').attr('aria-valuenow', data.value);
                                  };
                                  
                                  document.getElementById("text_value").innerHTML = data.text_value;
                              }
);
