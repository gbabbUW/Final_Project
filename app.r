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
black.caucasian.total <- black.caucasian.matches + black.caucasian.nomatches
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
caucasian.caucasian.total <- caucasian.caucasian.matches + caucasian.caucasian.nomatches
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
latino.caucasian.total <- latino.caucasian.matches + latino.caucasian.nomatches
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
asian.caucasian.total <- asian.caucasian.matches + asian.caucasian.nomatches
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
other.caucasian.total <- other.caucasian.matches + other.caucasian.nomatches
other.latino.total <- other.latino.matches + other.latino.nomatches
other.asian.total <- other.asian.matches + other.asian.nomatches
other.other.total <- other.other.matches + other.other.nomatches

#Sorting the above quantitative values into a dataframe for final referrencing
race <- c("Black","Caucasian","Latino","Asian","Other")

#creates a vector containing the percentage of positive matches out of the total matches per race for black individuals
black.match.num <- c(round(black.black.matches/ black.black.total * 100, digits =2),
                     round(black.caucasian.matches/black.caucasian.total* 100, digits =  2),
                     round(black.latino.matches/black.latino.total * 100, digits = 2),
                     round(black.asian.matches/black.asian.total,digits = 2),
                     round(black.other.matches/black.other.total,digits = 2))

#creates a vector containing the percentage of positive matches out of the total matches per race for caucasian individuals
caucasian.match.num <- c(round(caucasian.black.matches/ caucasian.black.total * 100, digits =2),
                         round(caucasian.caucasian.matches/caucasian.caucasian.total* 100, digits =  2),
                         round(caucasian.latino.matches/caucasian.latino.total * 100, digits = 2),
                         round(caucasian.asian.matches/caucasian.asian.total,digits = 2),
                         round(caucasian.other.matches/caucasian.other.total,digits = 2))

#creates a vector containing the percentage of positive matches out of the total matches per race for latino individuals
latino.match.num <- c(round(latino.black.matches/ latino.black.total * 100, digits =2),
                      round(latino.caucasian.matches/latino.caucasian.total* 100, digits =  2),
                      round(latino.latino.matches/latino.latino.total * 100, digits = 2),
                      round(latino.asian.matches/latino.asian.total,digits = 2),
                      round(latino.other.matches/latino.other.total,digits = 2))

#creates a vector containing the percentage of positive matches out of the total matches per race for asian individuals
asian.match.num <- c(round(asian.black.matches/ asian.black.total * 100, digits =2),
                     round(asian.caucasian.matches/asian.caucasian.total* 100, digits =  2),
                     round(asian.latino.matches/asian.latino.total * 100, digits = 2),
                     round(asian.asian.matches/asian.asian.total,digits = 2),
                     round(asian.other.matches/asian.other.total,digits = 2))

#creates a vector containing the percentage of positive matches out of the total matches per race for other individuals
other.match.num <- c(round((other.black.matches/ other.black.total)* 100, digits = 2),
                    round((other.caucasian.matches/other.caucasian.total)* 100,digits = 2),
                    round((other.latino.matches/other.latino.total)* 100,digits = 2),
                    round((other.asian.matches/other.asian.total)* 100,digits = 2),
                    round((other.other.matches/other.other.total)* 100,digits = 2 ))

#creates data frames using the vectors of positive matches per race for each respective race, used for later reactive filtering
black.data <- data.frame(race,black.match.num)
caucasian.data <- data.frame(race,caucasian.match.num)
latino.data <- data.frame(race,latino.match.num)
asian.data <- data.frame(race,asian.match.num)
other.data <- data.frame(race,other.match.num)

#cleans up the column names for easier reactive variables
colnames(black.data) <- c("race","match.percentage")
colnames(caucasian.data) <- c("race","match.percentage")
colnames(latino.data) <- c("race","match.percentage")
colnames(asian.data) <- c("race","match.percentage")
colnames(other.data) <- c("race","match.percentage")

#UI code starts here, will be used by everybody so expect merge conflicts and keep it clean------------
dating.ui <- fluidPage(
#master UI code starts here------------------  

sidebarLayout(

  #We'll use this as the control panel for user control
  sidebarPanel(
    
    #Creates a drop down input in which you can select different races to see who they matched with per race
    selectInput('selected.race', label = "Race", choices = c("Black", "Caucasian", "Latino", "Asian", "Other"))

  ),

  #We'll use this with a tabsetpanel in order to show graphs, reports, figures, etc
  mainPanel(
    plotOutput('plot')
  )
)

#master UI code ends here--------------------
)
#UI code ends here-------------------------------------------------------------------------------------


#Server code starts here, will be used by everybody so expect merge conflicts and keep it clean--------
dating.server <- function(input, output) {
#master server code starts here------------------  

  #chooses a dataframe of information containing matches per race for a specific race based on user input
  filtered <- reactive({
    if(input$selected.race == "Black"){
      filtered.race.data <- black.data
    }
    
    else if(input$selected.race == "Caucasian"){
      filtered.race.data <- caucasian.data
    }
    
    else if(input$selected.race == "Latino"){
      filtered.race.data <- latino.data
    }
    
    else if(input$selected.race == "Asian"){
      filtered.race.data <- asian.data
    }
    
    else if(input$selected.race == "Other"){
      filtered.race.data <- other.data
    }
   
    return(filtered.race.data)
  })

  #renders a plot of percentage of matches per race based on the chosen race of the user
  output$plot <- renderPlot({
   raceplot <- ggplot(data = filtered(), mapping = aes(x = race, y = match.percentage, fill = race)) +
     geom_bar(stat="identity") +
     labs (x = "Race") +
     labs (y = "Percent of Positive Matches")

   return(raceplot)
  })
  
#master server code ends here--------------------  
  
}

#Server code ends here---------------------------------------------------------------------------------

shinyApp(ui = dating.ui, server = dating.server)