##' Prostar : a GUI for DAPAR
##' 
##' @title Prostar
##' @return A new window in the default internet browser
##' @author Samuel Wieczorek
##' @examples
##' if(interactive()) {Prostar()}
Prostar <-
  function(){
          G <- .GlobalEnv
         
    options(shiny.maxRequestSize=1024^3)
    a=shiny::runApp(system.file("ProstarApp",package="Prostar"),
                    launch.browser = TRUE)
    return(invisible(a))
  }

