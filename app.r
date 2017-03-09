#Libraries: ggplot for visualization, dplyr for data cleaning, and shiny for interaction and publishing

library("ggplot2")
library("shiny")
library("dplyr")

#Creates dataframe containing all of the speed dating data, uncleaned
data <- read.csv("data/Speed Dating Data.csv")

#Data Section. All the data was cleaned and sorted here, so read comments carefully for understanding!--------
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

#End of Data Section-----------------------------------------------------------------------------------

#UI code starts here-------------------------

dating.ui <- fluidPage(
#master UI code starts here------------------  

sidebarLayout(

  #Sidebar panel for user control of data--------------------------------
  sidebarPanel(

    
    #Creates a drop down input in which you can select different races to see who they matched with per race
    selectInput('selected.race', label = h3("Select Race"), choices = c("Black", "Caucasian", "Latino", "Asian", "Other")),

    selectInput("field.input", label=h3("Select Field"), 
                c("Law"=1, "Math"=2, "Social Science, Psychologist"=3, 
                  "Medical Science, Pharmaceuticals, and Bio Tech"=4, "Engineering"=5,
                  "English/Creative Writing/ Journalism"=6, "History/Religion/Philosophy"=7,
                  "Business/Econ/Finance"=8, "Education, Academia"=9, 
                  "Biological Sciences/Chemistry/Physics"=10, "Social Work"=11,
                  "Undergrad/undecided"=12, "Political Science/International Affairs"=13,
                  "Film"=14, "Fine Arts/Arts Administration"=15, "Languages"=16,
                  "Architecture"=17, "Other"=18))
                                


  ),

  #End of SidebarPanel code----------------------------------------------
  
  #Tabsetpanel separation for each individuals different analysises
  mainPanel(

    tabsetPanel(
      
      tabPanel("Race Trends",
               plotOutput('plot.race', brush = "plot_brush"),
               verbatimTextOutput("info.race"),
               h3('Percentage of Positive Matches by Race'),
               p('This bar graph shows how members of the race you have selected matched members of other races during speed dating; it is important to note that while a category for Native Americans was included in the raw data, no individuals who identified as Native American took part in this study. The bars represent the percent of individuals in each race that your selected race considered a "match" divided by the total amount of dates your selected race had with each race. Do note that the bar graph scales for each selected race, so keep an eye on the y axis!')),
      
      tabPanel("Fields/Careers",
               plotOutput('plot.rate', brush = "plot_brush"),
               verbatimTextOutput("info.rate"),
               p('This bar graph shows how each individual was rated on a scale of 1 to 10 by their partner,
                 these rates are very normally distributed throughout work fields.'),
               plotOutput("plot.purpose", brush = "plot_brush"),
               verbatimTextOutput("info.purpose"),
               p('This bar graph shows how each individual rated the purpose of why they were 
                 speed dating in the first place, 1: Seemed like a fun night out, 2: To meet new people,
                 3: To get a date, 4: Looking for a serious relationship, 5: To say I did it, and 6: Other'),
               plotOutput("plot.out", brush = "plot_brush"),
               verbatimTextOutput("info.out"),
               p('This bar graph shows how each individual rated how much they went out 
                 1: Several times a week, 2: Twice a week, 3: Once a week, 4: Twice a month,
                 5: Once a month, 6: Several times a year, 7: Almost never'),
               h3("Analsis of Data"),
               p("Generally most of the data when filtered by field does not largely show any specific correlation or patterns 
                  when exploring a field's effects on how the individual's partner rated them, how the individual rated their reason 
                  for coming to speed dating, and how the individual rated how much they went out. Ratings are largely normally 
                  distributed, with a rating of 5 or 6 being the norm, while reasoning for speed dating is largely skewed right, meaning
                  most are speed dating because it seemed like a fun night out. How many times people go out is largely twice a week or several 
                  times a week as well. When exploring the data there were however some correlations made between field and how individuals responded and 
                  were rated."),
                
                p("When observing individuals in Medical Science/Pharmaceuticals/Bio Tech, the data for how their partner rated them is 
                  largely skewed left, meaning their ratings are generally very high. The data on their reasoning for speed dating shows that a large 
                  majority are there to meet new people, and this may be because the majority say that they go out only twice a month. Working in the medical 
                  field can be very time consuming, so this statistic is very reasonable."), 
                 
                p("When observing individuals in the History/Religion/Philosophy fields, they received very high ratings overall, however, 
                  in contrast to the Medical Professionals, these individuals were largely looking for a serious relationship, and went out 
                  generally twice a week. Philosophy largely explores humans, including their actions and relationships, so the idea that many 
                  of these individuals are looking for love is very plausible."),
                 
                 p("When observing Undergraduate Students, they also seemed to receive high ratings over all, were mostly doing it for a fun night out 
                  and went out around twice a week, something that is very common for individuals attending university."),
                 
                  p("Most individuals in the field of English/Creative Writing/ Journalism were speed dating to say that they did it which was not a 
                  common choice for any of the other fields, this may be because the experience could be substance for their writing.  In contrast, most 
                  Social Science/Psychologists were doing it to meet new people and because it seemed fun, which also correlates to their subsequent lifestyles. 
                  Film, language and fine arts also seemed to follow the Social Sciences, showing a coloration between the arts, and speed dating to meet new people.), 
                 
                 p(Although a large majority of the data followed a similar pattern, there are endless inferences that can be made, and it does seem as though the field 
                  you are in can affect why and how you speed date.")
             
               
    )
   )
  #End of TabsetPanel Code--------------------------------------------------------------------------------------
  )
 )
)

#UI code ends here-------------------------------------------------------------------------------------



#Server code starts here, will be used by everybody so expect merge conflicts and keep it clean--------
dating.server <- function(input, output) {
  
#master server code starts here------------------  
  
  #chooses a dataframe of information containing matches per race for a specific race based on user input
  filtered.race <- reactive({
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
  output$plot.race <- renderPlot({
    raceplot <- ggplot(data = filtered.race(), mapping = aes(x = race, y = match.percentage, fill = race)) +
      geom_bar(stat="identity") +
      labs (x = "Race") +
      labs (y = "Percent of Positive Matches")
    
    return(raceplot)
  })
  
  # Output for brushing a bar
  output$info.race <- renderText({
    if(is.null(input$plot_brush)){
      xy <- ("Click and drag across a bar for it's value!")
    }else{
      paste0("Count: ", round(input$plot_brush$ymax, 0))
    }
  })
  
  # Filters the data by field and partner rating
  filtered.rating <- reactive({
    data.info <- data %>% 
      filter(field_cd == input$field.input)
    
    count.attracted <- function(rating) {
      
      count <- select(data.info, attr_o) %>% 
        filter(attr_o == rating) %>% 
        nrow()
      return (count)
    }
    rates <- c(1,2,3,4,5,6,7,8,9,10)
    counts <- c(count.attracted(1),count.attracted(2),count.attracted(3),count.attracted(4),count.attracted(5),
                count.attracted(6),count.attracted(7),count.attracted(8),count.attracted(9),count.attracted(10))
    data.counts.attracted <- data.frame(rates, counts)
    
    
    return(data.counts.attracted)
  })
  
  # Filters data by field and purpose of speed dating
  filtered.purpose <- reactive({
    data.info <- data %>% 
      filter(field_cd == input$field.input)
    
    count.purpose <- function(number) {
      
      count <- select(data.info, goal) %>% 
        filter(goal == number) %>% 
        nrow()
      return (count)
    }
    purpose <- c(1,2,3,4,5,6)
    counts <- c(count.purpose(1),count.purpose(2),count.purpose(3),
                count.purpose(4),count.purpose(5), count.purpose(6))
    
    data.counts.purpose <- data.frame(purpose, counts)
    
    return(data.counts.purpose)
  })
  
  # Filters data by field and amount individuals go out
  filtered.out <- reactive({
    data.info <- data %>% 
      filter(field_cd == input$field.input)
    
    count.go.out <- function(number) {
      
      count <- select(data.info, go_out) %>% 
        filter(go_out == number) %>% 
        nrow()
      return (count)
    }
    
    go.out <- c(1,2,3,4,5,6,7)
    counts <- c(count.go.out(1),count.go.out(2),count.go.out(3),
                count.go.out(4),count.go.out(5), count.go.out(6),
                count.go.out(7))
    
    data.counts.out <- data.frame(go.out, counts)
    
    return(data.counts.out)
  })
  
  # Outputs plot on rating counts
  output$plot.rate <- renderPlot({
    p <- ggplot(data = filtered.rating()) +
      geom_bar( mapping = aes(x = rates, y = counts, fill = rates), stat = "identity") +
      labs (x = "Rating (10)") +
      labs (y = "Counts") +
      labs (title = "Counts of Rates given by Partner of Individuals in this Field")+
      theme(plot.title = element_text(hjust = 0.5))
    return(p)
    
    
  })
  
  # Output for brushing a bar
  output$info.rate <- renderText({
    if(is.null(input$plot_brush)){
      xy <- ("Select a Bar")
    }else{
      paste0("Count: ", round(input$plot_brush$ymax, 0))
    }
    
    
  })
  
  # Outputs plot on purpose of speed dating 
  output$plot.purpose <- renderPlot({
    p <- ggplot(data = filtered.purpose()) +
      geom_bar( mapping = aes(x = purpose, y = counts, fill = purpose), stat = "identity") +
      labs (x = "Purpose (1-6)") +
      labs (y = "Counts") +
      labs (title = "Counts of the Specific Purpose of Speed Dating for People in this Field ")+
      theme(plot.title = element_text(hjust = 0.5))
    return(p)
    
    
  })
  
  # Output for brushing a bar
  output$info.purpose <- renderText({
    if(is.null(input$plot_brush)){
      xy <- ("Select a Bar")
    }else{
      paste0("Count: ", round(input$plot_brush$ymax, 0))
    }
    
    
  })
  
  # Outputs plot on data counts of how many times individuals go out
  output$plot.out <- renderPlot({
    p <- ggplot(data = filtered.out()) +
      geom_bar( mapping = aes(x = go.out, y = counts, fill = go.out), stat = "identity") +
      labs (x = "Go Out(1-7)") +
      labs (y = "Counts") +
      labs (title = "Counts of how many Individuals in this Field Go Out") +
      theme(plot.title = element_text(hjust = 0.5))
    return(p)
    
    
  })
  
  # Output for brushing a bar
  output$info.out <- renderText({
    if(is.null(input$plot_brush)){
      xy <- ("Select a Bar")
    }else{
      paste0("Count: ", round(input$plot_brush$ymax, 0))
    }
  })

  
#master server code ends here--------------------  
  
}

#Server code ends here---------------------------------------------------------------------------------

shinyApp(ui = dating.ui, server = dating.server)
