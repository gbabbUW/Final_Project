#Libraries: ggplot for visualization, dplyr for data cleaning, and shiny for interaction and publishing

library("ggplot2")
library("shiny")
library("dplyr")

#Creates dataframe containing all of the speed dating data, uncleaned
data <- read.csv("data/Speed Dating Data.csv")

#Dataframes separated by race---------------------------------------------------------------------------------

#Data for Black/African Americans
black <- filter(data, race == 1)
black.matches <- filter(black, match == 1)
black.nomatch <- filter(black, match == 0)

#Data for European/Caucasian Americans
caucasian <- filter(data, race == 2)
caucasian.matches <- filter(caucasian, match == 1)
caucasian.nomatch <- filter(caucasian, match == 0)

#Data for Latino/Hispanic Americans
latino <- filter(data, race == 3)
latino.matches <- filter(latino, match == 1)
latino.nomatch <- filter(latino, match == 0)

#Data for Asian/Pacific-Islander/Asian-Americans
asian <- filter(data, race == 4)
asian.matches <- filter(asian, match == 1)
asian.nomatch <- filter(asian, match == 0)

#Data for Other
other <- filter(data, race == 6)
other.matches <- filter(other, match == 1)
other.nomatch <- filter(other, match == 0)

#Quantitative data of matches by race-------------------------------------------
#Black race---------------------------------------------------------------------
#number of matches per race based on a black individual
black.match.num <- nrow(black.matches)
black.black.matches <- nrow(filter(black.matches, race_o == 1))
black.caucasian.matches <- nrow(filter(black.matches, race_o == 2))
black.latino.matches <- nrow(filter(black.matches, race_o == 3))
black.asian.matches <- nrow(filter(black.matches, race_o == 4))
black.other.matches <- nrow(filter(black.matches, race_o == 6))

#number of non-matches per race based on a black individual
black.nomatch.num <- nrow(black.nomatch)
black.black.nomatches <- nrow(filter(black.nomatch, race_o == 1))
black.caucasian.nomatches <- nrow(filter(black.nomatch, race_o == 2))
black.latino.nomatches <- nrow(filter(black.nomatch, race_o == 3))
black.asian.nomatches <- nrow(filter(black.nomatch, race_o == 4))
black.other.nomatches <- nrow(filter(black.nomatch, race_o == 6))

#total number of dates per race per a black individual
black.black.total <- black.black.matches + black.black.nomatches
black.caucasion.total <- black.caucasion.matches + black.caucasion.nomatches
black.latino.total <- black.latino.matches + black.latino.nomatches
black.asian.total <- black.asian.matches + black.asian.nomatches
black.other.total <- black.other.matches + black.other.nomatches

#Caucasian race-------------------------------------------------------------------
#number of matches per race based on a caucasian individual
caucasian.match.num <- nrow(caucasian.matches)
caucasian.black.matches <- nrow(filter(caucasian.matches, race_o == 1))
caucasian.caucasian.matches <- nrow(filter(caucasian.matches, race_o == 2))
caucasian.latino.matches <- nrow(filter(caucasian.matches, race_o == 3))
caucasian.asian.matches <- nrow(filter(caucasian.matches, race_o == 4))
caucasian.other.matches <- nrow(filter(caucasian.matches, race_o == 6))

#number of non-matches per race based on a caucasian individual
caucasian.nomatch.num <- nrow(caucasian.nomatch)
caucasian.black.nomatches <- nrow(filter(caucasian.nomatch, race_o == 1))
caucasian.caucasian.nomatches <- nrow(filter(caucasian.nomatch, race_o == 2))
caucasian.latino.nomatches <- nrow(filter(caucasian.nomatch, race_o == 3))
caucasian.asian.nomatches <- nrow(filter(caucasian.nomatch, race_o == 4))
caucasian.other.nomatches <- nrow(filter(caucasian.nomatch, race_o == 6))

#total number of dates per race per a caucasian individual
caucasian.black.total <-caucasian.black.matches + caucasian.black.nomatches
caucasian.caucasion.total <- caucasian.caucasion.matches + caucasian.caucasion.nomatches
caucasian.latino.total <- caucasian.latino.matches + caucasian.latino.nomatches
caucasian.asian.total <- caucasian.asian.matches + caucasian.asian.nomatches
caucasian.other.total <- caucasian.other.matches + caucasian.other.nomatches

#Latino race-------------------------------------------------------------------
#number of matches per race based on a latino individual
latino.match.num <- nrow(latino.matches)
latino.black.matches <- nrow(filter(latino.matches, race_o == 1))
latino.caucasian.matches <- nrow(filter(latino.matches, race_o == 2))
latino.latino.matches <- nrow(filter(latino.matches, race_o == 3))
latino.asian.matches <- nrow(filter(latino.matches, race_o == 4))
latino.other.matches <- nrow(filter(latino.matches, race_o == 6))

#number of non-matches per race based on a latino individual
latino.nomatch.num <- nrow(latino.nomatch)
latino.black.nomatches <- nrow(filter(latino.nomatch, race_o == 1))
latino.caucasian.nomatches <- nrow(filter(latino.nomatch, race_o == 2))
latino.latino.nomatches <- nrow(filter(latino.nomatch, race_o == 3))
latino.asian.nomatches <- nrow(filter(latino.nomatch, race_o == 4))
latino.other.nomatches <- nrow(filter(latino.nomatch, race_o == 6))

#total number of dates per race per a latino individual
latino.black.total <-latino.black.matches + latino.black.nomatches
latino.caucasion.total <- latino.caucasion.matches + latino.caucasion.nomatches
latino.latino.total <- latino.latino.matches + latino.latino.nomatches
latino.asian.total <- latino.asian.matches + latino.asian.nomatches
latino.other.total <- latino.other.matches + latino.other.nomatches

#Asian race--------------------------------------------------------------------
#number of matches per race based on an asian/pacific islander individual
asian.match.num <- nrow(asian.matches)
asian.black.matches <- nrow(filter(asian.matches, race_o == 1))
asian.caucasian.matches <- nrow(filter(asian.matches, race_o == 2))
asian.latino.matches <- nrow(filter(asian.matches, race_o == 3))
asian.asian.matches <- nrow(filter(asian.matches, race_o == 4))
asian.other.matches <- nrow(filter(asian.matches, race_o == 6))

#number of no match dates based on an asian individual
asian.nomatch.num <- nrow(asian.nomatch)
asian.black.nomatches <- nrow(filter(asian.nomatch, race_o == 1))
asian.caucasian.nomatches <- nrow(filter(asian.nomatch, race_o == 2))
asian.latino.nomatches <- nrow(filter(asian.nomatch, race_o == 3))
asian.asian.nomatches <- nrow(filter(asian.nomatch, race_o == 4))
asian.other.nomatches <- nrow(filter(asian.nomatch, race_o == 6))

#Total number of dates per race of an asian individual
asian.black.total <-asian.black.matches + asian.black.nomatches
asian.caucasion.total <- asian.caucasion.matches + asian.caucasion.nomatches
asian.latino.total <- asian.latino.matches + asian.latino.nomatches
asian.asian.total <- asian.asian.matches + asian.asian.nomatches
asian.other.total <- asian.other.matches + asian.other.nomatches

#other race-------------------------------------------------------------------------------
#number of matches per race based on a other individual
other.match.num <- nrow(other.matches)
other.black.matches <- nrow(filter(other.matches, race_o == 1))
other.caucasian.matches <- nrow(filter(other.matches, race_o == 2))
other.latino.matches <- nrow(filter(other.matches, race_o == 3))
other.asian.matches <- nrow(filter(other.matches, race_o == 4))
other.other.matches <- nrow(filter(other.matches, race_o == 6))

#Number of no match dates per race based on an other individual
other.nomatch.num <- nrow(other.nomatch)
other.black.nomatches <- nrow(filter(other.nomatch, race_o == 1))
other.caucasian.nomatches <- nrow(filter(other.nomatch, race_o == 2))
other.latino.nomatches <- nrow(filter(other.nomatch, race_o == 3))
other.asian.nomatches <- nrow(filter(other.nomatch, race_o == 4))
other.other.nomatches <- nrow(filter(other.nomatch, race_o == 6))

#Total number of dates per race of an other individual
other.black.total <-other.black.matches + other.black.nomatches
other.caucasion.total <- other.caucasion.matches + other.caucasion.nomatches
other.latino.total <- other.latino.matches + other.latino.nomatches
other.asian.total <- other.asian.matches + other.asian.nomatches
other.other.total <- other.other.matches + other.other.nomatches

#Sorting the above quantitative values into a dataframe for final referrencing
race <- c("Black","Caucasian","Latino","Asian","Other")

black.match.num <- c(black.black.matches/ black.black.total,
                     black.caucasian.matches/black.caucasian.total,
                     black.latino.matches/black.latino.total,
                     black.asian.matches/black.asian.total,
                     black.other.matches/black.other.total)

caucasian.match.num <- c(caucasian.black.matches/ caucasian.black.total,
                         caucasian.caucasian.matches/caucasian.caucasian.total,
                         caucasian.latino.matches/caucasian.latino.total,
                         caucasian.asian.matches/caucasian.asian.total,
                         caucasian.other.matches/caucasian.other.total)

latino.match.num <- c(latino.black.matches/ latino.black.total,
                      latino.caucasian.matches/latino.caucasian.total,
                      latino.latino.matches/latino.latino.total,
                      latino.asian.matches/latino.asian.total,
                      latino.other.matches/latino.other.total)

asian.match.num <- c(asian.black.matches/ asian.black.total,
                     asian.caucasian.matches/asian.caucasian.total,
                     asian.latino.matches/asian.latino.total,
                     asian.asian.matches/asian.asian.total,
                     asian.other.matches/asian.other.total)

other.match.num <- c(other.black.matches/ other.black.total,
                     other.caucasian.matches/other.caucasian.total,
                     other.latino.matches/other.latino.total,
                     other.asian.matches/other.asian.total,
                     other.other.matches/other.other.total)

black.nomatches.num <- c(black.black.nomatches,caucasian.black.nomatches,latino.black.nomatches,asian.black.nomatches,other.black.nomatches)
caucasian.nomatches.num <- c(black.caucasian.nomatches,caucasian.caucasian.nomatches,latino.caucasian.nomatches,asian.caucasian.nomatches,other.caucasian.nomatches)
latino.nomatches.num <- c(black.latino.nomatches,caucasian.latino.nomatches,latino.latino.nomatches,asian.latino.nomatches,other.latino.nomatches)
asian.nomatches.num <- c(black.asian.nomatches,caucasian.asian.nomatches,latino.asian.nomatches,asian.asian.nomatches,other.asian.nomatches)
other.nomatches.num <- c(black.other.nomatches,caucasian.other.nomatches,latino.other.nomatches,asian.other.nomatches,other.other.nomatches)


sorted.data <- data.frame(race, black.match.num, caucasian.match.num, latino.match.num, asian.match.num, other.match.num)

View(sorted.data)
#UI code starts here, will be used by everybody so expect merge conflicts and keep it clean------------
dating.ui <- fluidPage(
#master UI code starts here------------------  

sidebarLayout(
  
  #We'll use this as the control panel for user control
  sidebarPanel(
    
    selectInput('selected.race', label = "Race", choices = c("Black", "Caucasian", "Latino", "Asian", "Other"))
    
  ),
  
  #We'll use this with a tabsetpanel in order to show graphs, reports, figures, etc
  mainPanel(
    plotOutput(plot)
  )
)

#master UI code ends here--------------------
)
#UI code ends here-------------------------------------------------------------------------------------


#Server code starts here, will be used by everybody so expect merge conflicts and keep it clean--------
dating.server <- function(input, output) {
#master server code starts here------------------  

  filtered.race <- reactive({
    filtered.race.data <- sorted.data %>%
      filter(race == input$selected.race)
    
    return(filtered.race.data)
  })
  
  output$plot <- renderPlot({
   raceplot <- ggplot(data = filtered.race, x = black.match.num)
   
   return(raceplot)
  })
  
#master server code ends here--------------------  
}
#Server code ends here---------------------------------------------------------------------------------

shinyApp(ui = dating.ui, server = dating.server)