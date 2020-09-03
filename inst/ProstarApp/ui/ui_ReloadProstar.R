tabPanel(title="ReloadProstar",
         value="ReloadTab",
         p("Due to some instability of cache memory when successively opening several datasets in a Prostar session, data management has been simplified. 
         To work on another dataset than the current one, reloading Prostar first is now necessary (with the button above).  It will restart Prostar
         with a fresh R session where import menus are enabled 'Dataset manager' menu."),
         actionButton("ReloadProstar", "Reload Prostar",class = actionBtnClass)
)
