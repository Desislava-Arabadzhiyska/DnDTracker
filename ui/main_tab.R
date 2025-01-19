#Function defs
slider_def <- function(s_id, h_name, min_val, max_val, start_val){
  sliderInput(s_id, h_name,
              min = min_val,
              max = max_val,
              value = start_val)
}
  

## main_tab ----
main_tab <- tabItem(
  tabName = "main_tab",
  
  selectInput("CSVorMan", "Do you wish to upload a CSV or manully enter the character's info?", 
              c("","Upload CSV", "Manually enter details")),
  
  hidden(fileInput("file1", "Choose CSV File",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv"))),
  hidden(fluidRow( class = "rowhide",box(id = "Q1", title = "How many people are in your party?", width = 12,
      collapsible = T, collapsed = F,
      flowLayout(
        slider_def("s1", "Party members", 1, 10, 1), 
        uiOutput("Nboxes"),
        uiOutput("ACboxes", width = 3),
        uiOutput("Hboxes", width = 3),
        uiOutput("Iboxes", width = 3)
      )
  ))),
  hidden(fluidRow( class = "rowhide2",box(id = "Q2", title = "Hom many enemy types do you have?", width = 12,
      collapsible = T, collapsed = F,
      flowLayout(
        slider_def("s2","Enemy types", 1, 5, 1), 
        uiOutput("Tboxes"),
        uiOutput("sliders"),
        uiOutput("EACboxes", width = 3),
        uiOutput("EHboxes", width = 3),
        uiOutput("EIboxes", width = 3)
      )
  ))),
  actionButton("view_feedback", "Submit Quiz")
)
