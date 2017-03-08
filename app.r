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
  
  #We'll use this with a tabsetpanel in order to show graphs, reports, figures, etc
  mainPanel(
    tabsetPanel(
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
             
               
    )
   )
  )
 )
)

#UI code ends here-------------------------------------------------------------------------------------



#Server code starts here, will be used by everybody so expect merge conflicts and keep it clean--------
dating.server <- function(input, output) {
  
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

#master server code ends here--------------------  
}
#Server code ends here---------------------------------------------------------------------------------

shinyApp(ui = dating.ui, server = dating.server)
