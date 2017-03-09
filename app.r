#Libraries: ggplot for visualization, dplyr for data cleaning, and shiny for interaction and publishing

library("ggplot2")
library("shiny")
library("dplyr")
library("tidyr")
library("plotly")
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
#date.anly <- select(data, match,dec,	attr,	sinc,	intel,	fun,	amb,	shar,	like,	prob,	met,	match_es)
#postdate.data <- select(data, iid,you_call, them_cal, date_3,	numdat_3,	num_in_3)
#postdate.data <- postdate.data[!duplicated(postdate.data),]
age.range.opp.sex <- range(look.for.opp.sex$age)
#UI code starts here, will be used by everybody so expect merge conflicts and keep it clean------------
dating.ui <- fluidPage(
  #master UI code starts here------------------  
  
  sidebarLayout(
    
    #We'll use this as the control panel for user control
    sidebarPanel(
      selectInput('gen', 'Gender', c("Male" = 1, "Female" = 0, "Both")),
      sliderInput('agran', 'Age Range', min = age.range.opp.sex[1], max = age.range.opp.sex[2], value = age.range.opp.sex),
      checkboxGroupInput('rce', 'Race', choices = c("Black/African American" = 1, "Caucasian" = 2, "Hispanic" = 3, "Asian/Pacific Islander" = 4, "Other" = 6), selected = c(1,2,3,4,6) ),
      selectInput('diffset', 'Different Survey Questions', c("What do you look for in a person?" = "look.for.opp.sex", "What does your same gender look for?" = "same.gen.look.for", "What does the opposite gender look for?" = "opp.gen.look.for", "How do you meausre up to expectations?" = "measure.up", "How you think other percieve you?" = "others.perc")),
      selectInput('trait', 'Trait of Subject', c("Attractive" = "attr","Sincerety" = "sinc",	"Intellegence" = "intel",	"fun" = "fun",	"ambitious" = "amb", "shared interests" = "shar" ) )
    ),
    
    
    #We'll use this with a tabsetpanel in order to show graphs, reports, figures, etc
    mainPanel(
      plotOutput('plot'),
      #tableOutput('table'),
      verbatimTextOutput('event')
    )
  )
  
  #master UI code ends here--------------------
)
#UI code ends here-------------------------------------------------------------------------------------


#Server code starts here, will be used by everybody so expect merge conflicts and keep it clean--------
dating.server <- function(input, output) {
  #master server code starts here------------------  
  filtered.data <- reactive({
    
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
      data <- NULL
    }
    if(input$diffset == "look.for.opp.sex"){
      data <- select_(data, "iid","gender","age", "race", paste0(input$trait, "1_1"), paste0(input$trait, "1_2"))
    }else if (input$diffset == "same.gen.look.for"){
      data <- select_(data, "iid", "gender", "age", "race", paste0(input$trait, "4_1"), paste0(input$trait, "4_2"))
    }else if (input$diffset == "opp.gen.look.for"){
      data <- select_(data, "iid", "gender", "age", "race", paste0(input$trait, "2_1"), paste0(input$trait, "2_2"))
    }else if (input$diffset == "measure.up"){
      data <- select_(data, "iid", "gender", "age", "race", paste0(input$trait, "3_1"), paste0(input$trait, "3_2"))
    }else if (input$diffset == "others.perc"){
      data <- select_(data, "iid", "gender", "age", "race", paste0(input$trait, "5_1"), paste0(input$trait, "5_2"))
    }
    return(data)
  })

  output$plot<-renderPlot({
    
    p <- ggplot(data = filtered.data())+
      geom_point(mapping = aes_(x = filtered.data()[,5], y = filtered.data()[,6], color = filtered.data()[,6]>filtered.data()[,5]),position = "jitter")+
      geom_smooth(mapping = aes(x = filtered.data()[,5], y = filtered.data()[,6]))+
      labs(title = "Exploring Participant Preferences Over Time", x = "Initial Response Value (Before Speed Dating)", y = "Final Response Value (After Speed Dating)", color = "Increased Rating After Night")+
      scale_color_manual(labels = c("Negitive/No Rating Change", "Positive Rating Change"), values = c("red", "blue"))
    
    return(p)
  })
  #master server code ends here--------------------  
}
#Server code ends here---------------------------------------------------------------------------------

shinyApp(ui = dating.ui, server = dating.server)