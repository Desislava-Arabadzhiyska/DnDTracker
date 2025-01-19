slider_def <- function(s_id, h_name, min_val, max_val, start_val){
  sliderInput(s_id, h_name,
              min = min_val,
              max = max_val,
              value = start_val)
}

## fb_tab ----
fb_tab <- tabItem(
  tabName = "fb_tab",
  h2("Combat"),
  p("Here you can follow the turns in your combat"),
  
  
  # box(
  #   title = "At the start of the battle", 
  #   solidHeader = TRUE,
  #   width = 12,
  #   #height = 150,
  #   flowLayout(
  #   verbatimTextOutput("summaryDset"))),
  
  box(
    title = "Current Player", 
    solidHeader = TRUE,
    width = 6,
    #height = 20,
    flowLayout(
      verbatimTextOutput("Trn"))),
  box(
    title = "Other Players", 
    solidHeader = TRUE,
    width = 6,#5
    #height = 20,
    flowLayout(
      dataTableOutput('OthersTable'))),
  box(id = "Hits", title = "How many were hit?", width = 12,
      collapsible = T, collapsed = F,
      flowLayout(
        slider_def("hits", "", 1, 5, 1),
        uiOutput("HWboxes"),
        uiOutput("HHboxes")
      )
  ),
  box(id = "TurnBox", title = "Finish Turn?", width = 12, 
      collapsible = T, collapsed = F, 
      flowLayout(actionButton("turn", "Finish Turn"))),
  box(id = "DownloadBox", title = "Want to Save your current state?", width = 12, 
      collapsible = T, collapsed = T, 
      flowLayout(downloadButton("downloadData", "Download Current State")))
  
  #div(style = "position: absolute; bottom:0; margin: 40px auto;", ),
  #div(style = "position: absolute; bottom:0;", downloadButton("downloadData", "Download Current State"))
  )
