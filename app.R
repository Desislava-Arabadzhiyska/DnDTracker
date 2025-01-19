#Libraries
library(shiny)
library(shinyjs)
library(shinydashboard)
library(tidyverse)

#point to additional tabs
source("ui/main_tab.R")
source("ui/fb_tab.R")
#source("ui/info_tab.R")


# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  skin = "black",
  # header, # if sourced above
  # sidebar, # if sourced above
  dashboardHeader(title = "DnD Combat tracker"),
  dashboardSidebar(
    # https://fontawesome.com/icons?d=gallery&m=free
    sidebarMenu(
      id = "tabs",
      menuItem("DnD Combat tracker", tabName = "main_tab",
               icon = icon("home")),
      menuItem("Follow combat", tabName = "fb_tab",
               icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), # links to www/custom.css
      tags$script(src = "custom.js") # links to www/custom.js
    ),
    tabItems(
      main_tab,
      fb_tab
    )
  )
)

# Define server logic behind app
server <- function(input, output, session) {
  
  observeEvent(input$CSVorMan, {
    if (input$CSVorMan == "Upload CSV") {
      shinyjs::show("file1")
    } else {
      shinyjs::hide("file1")
    }
  })
  
  observeEvent(input$CSVorMan, {
    if (input$CSVorMan == "Manually enter details") {
      shinyjs::show(selector = ".rowhide")
      shinyjs::show(selector = ".rowhide2")
    } else {
      shinyjs::hide(selector = ".rowhide")
      shinyjs::hide(selector = ".rowhide2")
    }
  })
  
  #####################################################
  ##################################Party Member section -----------------------------------------------------
  #####################################################
  
  # variable number of boxes for party member names 
  output$Nboxes <- renderUI({
    pvars <- input$s1
    ns <- seq(pvars)
    lapply(seq(pvars), function(i) {
      textInput(inputId = paste0("PM", as.character(ns[i])), 
                paste0("PM ", c(as.character(ns[i])), " name"), 
                placeholder = "Enter name..."
      )
    })
  })
  # variable number of boxes for party member health
  output$Hboxes <- renderUI({
    pvars <- input$s1
    ns <- seq(pvars)
    lapply(seq(pvars), function(i) {
      textInput(inputId = paste0("PMH", as.character(ns[i])), 
                paste0(input[[paste0("PM", as.character(ns[i]))]], "'s health"), 
                placeholder = "Enter health...e.g. 11"
      )
    })
  })
  # variable number of boxes for party member AC
  output$ACboxes <- renderUI({
    pvars <- input$s1
    ns <- seq(pvars)
    lapply(seq(pvars), function(i) {
      textInput(inputId = paste0("PMAC", as.character(ns[i])), 
                paste0(input[[paste0("PM", as.character(ns[i]))]], "'s AC"), 
                placeholder = "Enter AC...e.g. 16"
      )
    })
  })
  # variable number of boxes for party member initiative
  output$Iboxes <- renderUI({
    pvars <- input$s1
    ns <- seq(pvars)
    lapply(seq(pvars), function(i) {
      textInput(inputId = paste0("PMI", as.character(ns[i])), 
                paste0(input[[paste0("PM", as.character(ns[i]))]],  "'s initiative"), 
                placeholder = "Enter initiative...e.g. 10"
      )
    })
  })
  
  #######################################################
  ##################################Enemy section --------------------------------------------------------------
  ######################################################
  #Create a variable number of text boxes for the enemies
  output$Tboxes <- renderUI({
    pvars <- input$s2
    ns <- seq(pvars)
    lapply(seq(pvars), function(i) {
      textInput(inputId = paste0("Enemy", as.character(ns[i])), 
                paste0("Enemy ", c(as.character(ns[i]))), 
                placeholder = "Enter enemy type..."
      )
    })
  })
  #Create a variable number of sliders for the enemies
  output$sliders <- renderUI({
    pvars <- input$s2
    ns <- seq(pvars)
    lapply(seq(pvars), function(i) {
      sliderInput(inputId = paste0("NEnemyType", ns[i]),
                  label = paste0("Number of ", input[[paste0("Enemy", as.character(ns[i]))]]),
                  min = 1, max = 20, value = 3)
    })
  })
  # variable number of boxes for Enemy health
  output$EHboxes <- renderUI({
    pvars <- input$s2
    ns <- seq(pvars)
    lapply(seq(pvars), function(i) {
      textInput(inputId = paste0("EH", as.character(ns[i])), 
                paste0(input[[paste0("Enemy", as.character(ns[i]))]], "'s health"), 
                placeholder = "Enter health...e.g. 11"
      )
    })
  })
  # variable number of boxes for Enemy AC
  output$EACboxes <- renderUI({
    pvars <- input$s2
    ns <- seq(pvars)
    lapply(seq(pvars), function(i) {
      textInput(inputId = paste0("EAC", as.character(ns[i])), 
                paste0(input[[paste0("Enemy", as.character(ns[i]))]], "'s AC"), 
                placeholder = "Enter AC...e.g. 16"
      )
    })
  })
  # variable number of boxes for Enemy initiative
  output$EIboxes <- renderUI({
    pvars <- input$s2
    ns <- seq(pvars)
    lapply(seq(pvars), function(i) {
      textInput(inputId = paste0("EI", as.character(ns[i])), 
                paste0(input[[paste0("Enemy", as.character(ns[i]))]],  "'s initiative modifier"), 
                placeholder = "Enter initiative...e.g. 10"
      )
    })
  })
  
  
  #####################################################################
  ################################################## finish quiz ----
  ####################################################################
  observeEvent(input$view_feedback, {
    runjs("closeBox('Q4');")
    #show("save_data")
    updateTabItems(session, "tabs", selected = "fb_tab")
  })
  
  #################################
  ###### Calculate order
  ##################################
  Rtabl <- reactiveValues(
    Names = c(),
    Health = c(),
    AC = c(),
    Initiative = c(), 
    Type = c()
  )
  observeEvent(input$view_feedback,{
    if (input$CSVorMan == "Upload CSV") {
      ############################################Get CSV Content
      
      req(input$file1)
      
      df <- read.csv(input$file1$datapath,
                     header = TRUE,
                     sep = ",")
        
      
      df <- df%>%
        mutate("Initiative" = if_else(Type == "Enemy", as.double(Initiative) + sample(1:20, length(df$Names)), as.double(Initiative)))%>%
        arrange(desc(Initiative))%>%
        mutate("Order" = seq.int(length(df$Names)))
  
  Rtabl$Names <- df$Names%>%unlist()
  Rtabl$Health <- df$Health%>%unlist()
  Rtabl$AC <- df$AC%>%unlist()
  Rtabl$Initiative <- df$Initiative%>%unlist()
  Rtabl$Type <- df$Type%>%unlist()      
  
    } else{
      p_names <- c()
      p_health <- c()
      p_ac <- c()
      p_initiative <- c()
      for (i in 1:input$s1) {
        p_names[i] <- input[[paste0("PM", as.character(i))]]
        p_health[i] <- input[[paste0("PMH", as.character(i))]]
        p_ac[i] <- input[[paste0("PMAC", as.character(i))]]
        p_initiative[i] <- input[[paste0("PMI", as.character(i))]]
      }
      e_names <- c()
      e_n <- c()
      e_health <- c()
      e_ac <- c()
      e_initiative <- c()
      for (i in 1:input$s2) {
        e_type<- input[[paste0("Enemy", as.character(i))]]
        e_n[i] <- input[[paste0("NEnemyType", as.character(i))]]
        e_names <- c(e_names, paste0(e_type, as.character(1:e_n[i])))
        e_health <- c(e_health, rep(input[[paste0("EH", as.character(i))]], e_n[i]))
        e_ac <- c(e_ac, rep(input[[paste0("EAC", as.character(i))]], e_n[i]))
        e_initiative <- c(e_initiative, as.double(input[[paste0("EI", as.character(i))]])+sample(1:20, e_n[i]))
      }
      e_initiative[e_initiative<1] <- 1
      
      my_table<- (tibble("Names" = c(p_names, e_names), 
                         "Health" = c(p_health, e_health), 
                         "AC" =  c(p_ac, e_ac),
                         "Initiative" =  as.double(c(p_initiative, e_initiative)),
                         "Type" = c(rep("Player", length(p_names)), rep("Enemy", length(e_names)))
      )%>%
        arrange(desc(Initiative))%>%
        mutate("Order" = seq.int(length(c(p_names, e_names))))
      # %>% top_n(2)
      )
      Rtabl$Names <- my_table[, 1]%>%unlist()
      Rtabl$Health <- my_table[, 2]%>%unlist()
      Rtabl$AC <- my_table[, 3]%>%unlist()
      Rtabl$Initiative <- my_table[, 4]%>%unlist()
      Rtabl$Type <- my_table[, 5]%>%unlist()
    }
  })



#######################################################################
#####Display who's turn it is ------------------------------------------------------------------------------------- 
#######################################################################
values <- reactiveValues(j = 0)
observe({
  input$turn
  isolate({
    values$j <- values$j + 1
  })
})


output$Trn <- renderPrint({
  a <- Rtabl$Names
  b <- Rtabl$Health
  c <- Rtabl$AC
  d <- Rtabl$Type
  if (values$j <= length(a)){
    if (as.character(d[values$j]) == "Enemy"){
      cat(paste0(as.character(a[values$j]), "'s turn"))
    } else {
    cat(paste0(as.character(a[values$j]), "'s turn", "\n", "Health: ", as.character(b[values$j]), "\n", "AC: ", as.character(c[values$j])))
    }
    #print(as.character(length(a)))
    #print(as.character(a))
  } else { print("Round over")
    values$j <-1
  }
})

#####################################################################
##################################Display others ------------------------------------------------------------------------------------- 
####################################################################
output$OthersTable <- renderDataTable({
  a <- tibble("Names" = Rtabl$Names, 
              "Health" = Rtabl$Health, 
              "AC" =  Rtabl$AC,
              "Initiative" =  Rtabl$Initiative,
              "Type" = Rtabl$Type)%>% 
    mutate("Health" = if_else(Type == "Enemy", "Not Disclosed", as.character(Health)), 
           "AC" = if_else(Type == "Enemy", "Not Disclosed", as.character(AC)))
  
  a <- a[-values$j, ]
  if (values$j <= length(a[[1]])+1){
    #a[,1:2]
    a[,1:3]
    
  } else { print("Round over")}
})

# ###################################################
# # variable number of boxes for damage recipients
# ###################################################
output$HWboxes <- renderUI({
  #CurrentType <- as.character(Rtabl$Type[values$j])
  #OtherTypes <- CurrentType != Rtabl$Type
  pvars <- input$hits
  ns <- seq(pvars)
  #a <- sort(as.character(Rtabl$Names[OtherTypes]))
  a <- sort(as.character(Rtabl$Names))
  lapply(seq(pvars), function(i) {
    selectInput(
      inputId=paste0("HITSWHOM", as.character(ns[i])),
      paste0("Character ", as.character(ns[i]), " hit"),
      a,
      selected = NULL,
      multiple = FALSE,
      selectize = TRUE,
      width = NULL,
      size = NULL
    )
  })
})
# ####################################
# # variable number of boxes for damage
# ###################################
output$HHboxes <- renderUI({
  pvars <- input$hits
  ns <- seq(pvars)
  lapply(seq(pvars), function(i) {
    textInput(inputId = paste0("HITSHOW", as.character(ns[i])),
              paste0(input[[paste0("HITSWHOM", as.character(ns[i]))]],  " was hit for:"),
              placeholder = "Enter damage received...e.g. 5"
    )
  })
})
# 
# #############################################################
# #get a summary of the hit updates
# ##############################################################

hitTable <- reactiveValues(a = (tibble("Names" = "a",
                                       "Health_update" = 0)))
observeEvent(input$turn,{
  h_names <- c()
  h_health <- c()
  #if (values$j >1) {
  for (i in 1:input$hits) {
    if(!is.na(i)){
      h_names[i] <- input[[paste0("HITSWHOM", as.character(i))]]
    } else {h_names[i] <- ""}
    h_health[i] <- input[[paste0("HITSHOW", as.character(i))]]
  }
  
  h_table<- (tibble("Names" = h_names,
                    "Health_update" = h_health)
             #)} else {
             # h_table<- (tibble("Names" = "a",
             #    "Health_update" = 0)
             
  )     #}
  #retunrn(h_table)
  isolate(
    hitTable$a <- h_table
  )
})

observeEvent(input$turn, {
  tmp <- tibble("Names" = as.character(Rtabl$Names), 
                "Health" = as.double(Rtabl$Health))
  if (values$j >1) {
    tmp2 <- hitTable$a#hitTable()
    #tmp <- tmp2
    tmp <- tmp %>% left_join(tmp2, by = "Names")%>%
      mutate(Health_update = if_else(is.na(Health_update), 0, as.double(Health_update)),
             Health = as.double(Health) - as.double(Health_update))
    #%>%select(-Health_update)
  }
  Rtabl$Names <- tmp$Names
  Rtabl$Health <- tmp$Health
  #Remove any characters that have died or have fainted
  CharsToRemove <- Rtabl$Health >0
  Rtabl$Names <- Rtabl$Names[CharsToRemove]
  Rtabl$Health <- Rtabl$Health[CharsToRemove]
  Rtabl$Initiative <- Rtabl$Initiative[CharsToRemove]
  Rtabl$Type <- Rtabl$Type[CharsToRemove]
  Rtabl$AC <- Rtabl$AC[CharsToRemove]
  
})

# Downloadable csv of selected dataset ----
output$downloadData <- downloadHandler(
  filename = function() {
    paste("CurrentState.csv", sep = "")
  },
  content = function(file) {
    write.csv(tibble("Names" = as.character(Rtabl$Names), 
                     "Health" = as.double(Rtabl$Health)), file, row.names = FALSE)
  }
)

}

# Run the application 
shinyApp(ui = ui, server = server)
