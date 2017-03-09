#Libraries: ggplot for visualization, dplyr for data cleaning, and shiny for interaction and publishing

library("ggplot2")
library("shiny")
library("dplyr")

#Creates dataframe containing all of the speed dating data, uncleaned
data <- read.csv("data/Speed Dating Data.csv", stringsAsFactors = FALSE)

look.for.opp.sex <- select(data, iid, gender, age, race, attr1_1, sinc1_1, intel1_1, fun1_1, amb1_1, shar1_1,attr1_2,	sinc1_2,	intel1_2,	fun1_2,	amb1_2,	shar1_2)
look.for.opp.sex <- na.omit(look.for.opp.sex[!duplicated(look.for.opp.sex),])
same.gen.look.for <- select(data, iid, gender, age, race, attr4_1,	sinc4_1,	intel4_1,	fun4_1,	amb4_1,	shar4_1, attr4_2,	sinc4_2,	intel4_2,	fun4_2,	amb4_2,	shar4_2)
same.gen.look.for <- na.omit(same.gen.look.for[!duplicated(same.gen.look.for),])
same.gen.look.for <- same.gen.look.for[92:363,]
opp.gen.look.for <- select(data, iid, gender, age, race, attr2_1,	sinc2_1,	intel2_1,	fun2_1,	amb2_1,	shar2_1,attr2_2,	sinc2_2,	intel2_2,	fun2_2,	amb2_2,	shar2_2)
opp.gen.look.for <- na.omit(opp.gen.look.for[!duplicated(opp.gen.look.for),])
measure.up <- select(data, iid,gender, age, race, attr3_1,	sinc3_1,	fun3_1,	intel3_1,	amb3_1, attr3_2,	sinc3_2,	intel3_2,	fun3_2,	amb3_2)
measure.up <- na.omit(measure.up[!duplicated(measure.up),])
measure.up[,5:14]<- measure.up[,5:14]*10
others.perc <- select(data, iid, gender, age, race, attr5_1,	sinc5_1,	intel5_1,	fun5_1,	amb5_1, attr5_2,	sinc5_2,	intel5_2,	fun5_2,	amb5_2)
others.perc <- na.omit(others.perc[!duplicated(others.perc),])
others.perc[,5:14] <- others.perc[,5:14]*10
age.range.opp.sex <- range(look.for.opp.sex$age)

#UI code starts here, will be used by everybody so expect merge conflicts and keep it clean------------
dating.ui <- fluidPage(
  #master UI code starts here------------------  
  #We'll use this as the control panel for user control
  sidebarPanel(
    h3("Fields/Careers"),
    selectInput("field.input", "Select Field", 
                c("Law"=1, "Math"=2, "Social Science, Psychologist"=3, 
                  "Medical Science, Pharmaceuticals, and Bio Tech"=4, "Engineering"=5,
                  "English/Creative Writing/ Journalism"=6, "History/Religion/Philosophy"=7,
                  "Business/Econ/Finance"=8, "Education, Academia"=9, 
                  "Biological Sciences/Chemistry/Physics"=10, "Social Work"=11,
                  "Undergrad/undecided"=12, "Political Science/International Affairs"=13,
                  "Film"=14, "Fine Arts/Arts Administration"=15, "Languages"=16,
                  "Architecture"=17, "Other"=18)),
    h3("Survey Response Analysis"),
    selectInput('trait', 'Trait of Subject', c("Attractive" = "attr","Sincerety" = "sinc",	"Intellegence" = "intel",	"fun" = "fun",	"ambitious" = "amb", "shared interests" = "shar" ) ),
    selectInput('diffset', 'Different Survey Questions', c("What do you look for in a person?" = "look.for.opp.sex", "What does your same gender look for?" = "same.gen.look.for", "What does the opposite gender look for?" = "opp.gen.look.for", "How do you meausre up to expectations?" = "measure.up", "How you think other percieve you?" = "others.perc")),
    selectInput('gen', 'Gender', c("Male" = 1, "Female" = 0, "Both"), selected = "Both"),
    sliderInput('agran', 'Age Range', min = age.range.opp.sex[1], max = age.range.opp.sex[2], value = age.range.opp.sex),
    checkboxGroupInput('rce', 'Race', choices = c("Black/African American" = 1, "Caucasian" = 2, "Hispanic" = 3, "Asian/Pacific Islander" = 4, "Other" = 6), selected = c(1,2,3,4,6) )
  ),
  
  #We'll use this with a tabsetpanel in order to show graphs, reports, figures, etc
  mainPanel(
    tabsetPanel( 
      tabPanel("Data Overview",
               h1("A Speed Dating Experiment"),
               h3("Where Did This Data Come From?"),
               p("This data was compiled by Ray Fisman and Sheena Lyengar, two Columbia Business School professors. 
                 The purpose of their data collection was to find evidence for their paper 'Gender Differences in Mate Selection: Evidence From a Speed Dating Experiment'. 
                 We found this data source through kaggle which offers users a database of public data sets and other resources similar to the one this app is built around."),
               helpText("Click here to see the data source:", a("Speed Dating Experiment",target = "_blank", href = "https://www.kaggle.com/annavictoria/speed-dating-experiment")),
               helpText("Click here to see the essay that utalizes this data set:", a("Essay", target = "_blank", href = "http://faculty.chicagobooth.edu/emir.kamenica/documents/genderDifferences.pdf")),
               h3("How Was This Data Collected?"),
               p("This data was collected using a series of speed dating events ranging from 2002-2004.
                 As the night continued participants would record their experience with every person they met that night using a rating system that was focused on the attributes of attractiveness, sincerity, intelligence, fun, ambition, and shared interests.
                 Not only was data collected about the date, but a questionnaire was used to gather information about peoples occupations, habits, demographics, and beliefs."),
               h3("How Will We Analyze This Data?"),
               p("To explore the data we broke it up into three parts. The first part of our analysis will explore how carres and occupations have different effects on an individuals attribute ratings and social life. 
                 Then we will explore the effect that race played in matching during the event. Finally we broke down the survey data to analyze how the experience of speed dating changed the individuals beliefs about themselves and their peers by analyzing how each participants ratings from the different survey questions changed after the experiment was conducted."),
               h3("Who Would Be Interested in Our Findings?"),
               p("The main audience for our analysis is college students, considering that we are presenting our findings to a college class. However, this experiment includes a wide range of ages, so if anyone is looking for dating trends this data can apply to anyone."),
               h3("Notes About The Data"),
               p("Some key elements of the data to consider. The biggest restriction about the data set is that no experiments were conducted to find dating trends in the same sex. Because of this our analysis is mainly applicable to heterosexual dating trends. Also a variety of races lacked from the experiment, and the majority of the participants were caucasian. Finally the data is not representitive of the entire population, not only are all races and sexual preferences represented but the data set is too small to apply to the population.")),
      
      tabPanel("Fields/Careers",
               h1("Analyzing The Effects of Fields/Careers"),
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
                  when exploring a field’s effects on how the individual’s partner rated them, how the individual rated their reason 
                  for coming to speed dating, and how the individual rated how much they went out. Ratings are largely normally 
                  distributed, with a rating of 5 or 6 being the norm, while reasoning for speed dating is largely skewed right, meaning
                  most are speed dating because it “seemed like a fun night out.” How many times people go out is largely twice a week or several 
                  times a week as well. When exploring the data there were however some correlations made between field and how individuals responded and 
                  were rated."),
                
                p("When observing individuals in Medical Science/Pharmaceuticals/Bio Tech, the data for how their partner rated them is 
                  largely skewed left, meaning their ratings are generally very high. The data on their reasoning for speed dating shows that a large 
                  majority are there to “meet new people,” and this may be because the majority say that they go out only twice a month. Working in the medical 
                  field can be very time consuming, so this statistic is very reasonable."), 
                 
                p("When observing individuals in the History/Religion/Philosophy fields, they received very high ratings overall, however, 
                  in contrast to the Medical Professionals, these individuals were largely looking for a serious relationship, and went out 
                  generally twice a week. Philosophy largely explores humans, including their actions and relationships, so the idea that many 
                  of these individuals are looking for love is very plausible."),
                 
                 p("When observing Undergraduate Students, they also seemed to receive high ratings over all, were mostly doing it for a “fun night out” 
                  and went out around twice a week, something that is very common for individuals attending university."),
                 
                  p("Most individuals in the field of English/Creative Writing/ Journalism were speed dating “to say that they did it” which was not a 
                  common choice for any of the other fields, this may be because the experience could be substance for their writing.  In contrast, most 
                  Social Science/Psychologists were doing it to “meet new people” and “because it seemed fun,” which also correlates to their subsequent lifestyles. 
                  Film, language and fine arts also seemed to follow the Social Sciences, showing a coloration between the arts, and speed dating to “meet new people.”"), 
                 
                 p("Although a large majority of the data followed a similar pattern, there are endless inferences that can be made, and it does seem as though the field 
                  you are in can affect why and how you speed date.")
    ),
    tabPanel(
      "Survey Analysis",
      h1("Breaking Down Survey Responses"),
      plotOutput('plotbfafpoint', click = 'clk'),
      verbatimTextOutput('outp'),
      tableOutput('plotbfafbar'),
      textOutput('text.gra')
    )
   )
  )
)


#UI code ends here-------------------------------------------------------------------------------------



#Server code starts here, will be used by everybody so expect merge conflicts and keep it clean--------
dating.server <- function(input, output) {
  #master server code starts here------------------  
  filteredSurvey.data <- reactive({
    
    data <-  get(input$diffset)
    if(input$gen == 0 | input$gen == 1){
      data <- filter(data, gender == input$gen)
    }
    data <- filter(data, age >= input$agran[1] & age <= input$agran[2])
    if(length(input$rce) == 1 ){
      data <- data %>% 
        filter(race == input$rce)
      
    }else if(length(input$rce) == 2){
      data <- data %>% 
        filter(race == input$rce[1] | race == input$rce[2])
      
    }else if (length(input$rce) == 3){
      data <- data %>%
        filter(race == input$rce[1] | race == input$rce[2] | race == input$rce[3])
    }else if (length(input$rce) == 4){
      data <- data %>%
        filter(race == input$rce[1] | race == input$rce[2] | race == input$rce[3] | race == input$rce[4])
    }else if (length(input$rce) == 5){
      data <- data  
    }else{
      return(NULL)
    }
    if(input$diffset == "look.for.opp.sex"){
      data <- select_(data, "iid","gender","age", "race", paste0(input$trait, "1_1"), paste0(input$trait, "1_2"))
    }else if (input$diffset == "same.gen.look.for"){
      data <- select_(data, "iid", "gender", "age", "race", paste0(input$trait, "4_1"), paste0(input$trait, "4_2"))
    }else if (input$diffset == "opp.gen.look.for"){
      data <- select_(data, "iid", "gender", "age", "race", paste0(input$trait, "2_1"), paste0(input$trait, "2_2"))
    }else if (input$diffset == "measure.up" & input$trait != "shar"){
      data <- select_(data, "iid", "gender", "age", "race", paste0(input$trait, "3_1"), paste0(input$trait, "3_2"))
    }else if (input$diffset == "others.perc" & input$trait != "shar"){
      data <- select_(data, "iid", "gender", "age", "race", paste0(input$trait, "5_1"), paste0(input$trait, "5_2"))
    }else{
      return(NULL)
    }
    return(data)
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
  
  # Filtes data by field and purpose of speed dating
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
  
  
#master server code starts here------------------  

  output$plotbfafpoint<-renderPlot({
    if(is.null(filteredSurvey.data())){
      return(NULL)
    }else{
    p <- ggplot(data = filteredSurvey.data())+
      geom_point(mapping = aes_(x = filteredSurvey.data()[,5], y = filteredSurvey.data()[,6], color = filteredSurvey.data()[,6]>filteredSurvey.data()[,5]), alpha = 1/5)+
      geom_smooth(mapping = aes(x = filteredSurvey.data()[,5], y = filteredSurvey.data()[,6]))+
      labs(title = "Exploring Participant Attribute Preferences Before and After Speed Dating", x = "Initial Response Value (Before Speed Dating)", y = "Final Response Value (After Speed Dating)", color = "Increased Rating After Night")+
      theme(plot.title = element_text(hjust = 0.5))+
      scale_color_manual(labels = c("Negitive/No Rating Change", "Positive Rating Change"), values = c("red", "blue"))
    
    return(p)
    }
  })
  sumserv <- reactive({
    `Average Initial Attribute Rating` <- mean(filteredSurvey.data()[,5])
    `Average Final Attribute Rating` <- mean(filteredSurvey.data()[,6])
    data <- data.frame(`Average Initial Attribute Rating`, `Average Final Attribute Rating`)
    return(data)
  }) 
  output$plotbfafbar <- renderTable({
    return(sumserv())
  
})
  
  output$outp <- renderText({
      if(is.null(input$clk)){
        return("Pick a Point \n")
      }else{
        paste0("Initial Rating = ", round(input$clk$x, 1), "\n", "Final Rating = ", round(input$clk$y,1), "\n")
      }
    })
  output$text.gra <- renderText({
    if(input$diffset == "look.for.opp.sex"){
      ques <- "What do you look for in a person?"    
    }else if(input$diffset == "same.gen.look.for"){
      ques <- "What does your same gender look for?"
    }else if (input$diffset == "opp.gen.look.for"){
      ques <- "What does the opposite gender look for?"
    }else if (input$diffset == "measure.up"){
      ques <- "How do you meausre up to expectations?"
    }else if (input$diffset == "others.perc"){
      ques <- "How you think other percieve you?"
    }
    `Average Initial Attribute Rating` <- round(mean(filteredSurvey.data()[,5]),1)
    `Average Final Attribute Rating` <- round(mean(filteredSurvey.data()[,6]),1)
    text.out <- paste("When asked the question'",ques,"' the average initial response about the trait was", `Average Initial Attribute Rating`, "and a day after the event the average final rating for the trait was", `Average Final Attribute Rating`,". The graph above shows this trend visually and colors each point depending on how the individuals response changed after a day. Blue represents a positive change, while red represents no/a negitive change in that specific trait. The graph also of trend line that can show the trait change, a positive exponential trend meaning increased ratings and a negitive exponential trend meaning decreased ratings. If the trend appears linear or close to linear that means the trait ratings changed very little if at all. The graph also offers a lot of interactive elements, allowing users to view the data by race, age range, gender, question asked and trait. Users can also click different points on the graph and see the individuals initial rating for that trait and their final rating as well." )
    return(text.out)
  })
  #master server code ends here--------------------  
}
#Server code ends here---------------------------------------------------------------------------------

shinyApp(ui = dating.ui, server = dating.server)
