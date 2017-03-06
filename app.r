#Libraries: ggplot for visualization, dplyr for data cleaning, and shiny for interaction and publishing

library("ggplot2")
library("shiny")
library("dplyr")

#Creates dataframe containing all of the speed dating data, uncleaned
data <- read.csv("data/Speed Dating Data.csv")

#UI code starts here, will be used by everybody so expect merge conflicts and keep it clean------------
dating.ui <- fluidPage(
#master UI code starts here------------------  

sidebarLayout(
  
  #We'll use this as the control panel for user control
  sidebarPanel(
    
  ),
  
  #We'll use this with a tabsetpanel in order to show graphs, reports, figures, etc
  mainPanel(
    
  )
)

#master UI code ends here--------------------
)
#UI code ends here-------------------------------------------------------------------------------------


#Server code starts here, will be used by everybody so expect merge conflicts and keep it clean--------
dating.server <- function(input, output) {
#master server code starts here------------------  

#master server code ends here--------------------  
}
#Server code ends here---------------------------------------------------------------------------------

shinyApp(ui = dating.ui, server = dating.server)