tabPanel("Open MSnset file",
         value = "openMSnsetTab",
         tagList(
           uiOutput("openMSnsetScreen"),
           actionButton("loadMSnset", "Load MSnset",class = actionBtnClass),
           #uiOutput("updateDesign")
           p("Once you have clicked on the loading button above, the dataset will be loaded min Prostar. 
              You will be automatically redirected to the home page of Prostar to start processing your dataset.
              At this time, all the menus that allow to import a dataset ('Open MSnset', 'demo Data' and 'Convert data')
              will be disabled to avoid the successive loading of multiple datasets.
              If you wish to work on another dataset, then go to the 'Reload Prostar' item of the first menu and click on the button.
              By doing this, Prostar will restart with a fresh R session and the import menus will be enabled again.
              ")
        )
                     
)