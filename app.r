#Libraries: ggplot for visualization, dplyr for data cleaning, and shiny for interaction and publishing

library("ggplot2")
library("shiny")
library("dplyr")

#Creates dataframe containing all of the speed dating data, uncleaned
data <- read.csv("data/Speed Dating Data.csv", stringsAsFactors = FALSE)

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

#Creates a new data frame only containing the responses from the first survey question, and removes any duplicate or na rows.
look.for.opp.sex <- select(data, iid, gender, age, race, attr1_1, sinc1_1, intel1_1, fun1_1, amb1_1, shar1_1,attr1_2,	sinc1_2,	intel1_2,	fun1_2,	amb1_2,	shar1_2)
look.for.opp.sex <- na.omit(look.for.opp.sex[!duplicated(look.for.opp.sex),])

#Creates a new data frame only containing the responses from the fourth survey question, and removes any duplicate or na rows. The first 91 rows are removed because their data was recorded incorrectly and cannot be analyzed with the rest of the data.
same.gen.look.for <- select(data, iid, gender, age, race, attr4_1,	sinc4_1,	intel4_1,	fun4_1,	amb4_1,	shar4_1, attr4_2,	sinc4_2,	intel4_2,	fun4_2,	amb4_2,	shar4_2)
same.gen.look.for <- na.omit(same.gen.look.for[!duplicated(same.gen.look.for),])
same.gen.look.for <- same.gen.look.for[92:363,]

#Creates a new data frame only containing the responses from the second survey question, and removes any duplicate or na rows.
opp.gen.look.for <- select(data, iid, gender, age, race, attr2_1,	sinc2_1,	intel2_1,	fun2_1,	amb2_1,	shar2_1,attr2_2,	sinc2_2,	intel2_2,	fun2_2,	amb2_2,	shar2_2)
opp.gen.look.for <- na.omit(opp.gen.look.for[!duplicated(opp.gen.look.for),])

#Creates a new data frame only containing the responses from the third survey question, and removes any duplicate or na rows. This multiplies the responses by the partic by 10 since they were recorded in the wrong scale compared to the rest of the study.
measure.up <- select(data, iid,gender, age, race, attr3_1,	sinc3_1,	fun3_1,	intel3_1,	amb3_1, attr3_2,	sinc3_2,	intel3_2,	fun3_2,	amb3_2)
measure.up <- na.omit(measure.up[!duplicated(measure.up),])
measure.up[,5:14]<- measure.up[,5:14]*10

#Creates a new data frame only containing the responses from the fifth survey question, and removes any duplicate or na rows. This multiplies the responses by the partic by 10 since they were recorded in the wrong scale compared to the rest of the study.
others.perc <- select(data, iid, gender, age, race, attr5_1,	sinc5_1,	intel5_1,	fun5_1,	amb5_1, attr5_2,	sinc5_2,	intel5_2,	fun5_2,	amb5_2)
others.perc <- na.omit(others.perc[!duplicated(others.perc),])
others.perc[,5:14] <- others.perc[,5:14]*10

#Creates the range of ages for the UI slider.
age.range.opp.sex <- range(look.for.opp.sex$age)

#End of Data Section-----------------------------------------------------------------------------------

#UI code starts here-------------------------

dating.ui <- fluidPage(
  #master UI code starts here------------------  
  titlePanel("What We Can Learn From Speed Dating"),
#master UI code starts here------------------  
#We'll use this as the control panel for user control
  sidebarLayout(
    
    sidebarPanel(
      
      #Creates the race trends widgets
      h3("Race Trends"),
      
      selectInput('selected.race', "Select Race", c("Black", "Caucasian", "Latino", "Asian", "Other")),
      
      #Creates the fields/careers widgets
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
      
      #Creates the survey response analysis widgets
      h3("Survey Response Analysis"),
      
      selectInput('trait', 'Trait of Subject', c("Attractive" = "attr","Sincerity" = "sinc",	"Intellegence" = "intel",	"fun" = "fun",	"ambitious" = "amb", "shared interests" = "shar" ) ),
      
      selectInput('diffset', 'Different Survey Questions', c("What do you look for in a person?" = "look.for.opp.sex", "What does your same gender look for?" = "same.gen.look.for", "What does the opposite gender look for?" = "opp.gen.look.for", "How do you meausre up to expectations?" = "measure.up", "How do you think others perceive you?" = "others.perc")),
      
      selectInput('gen', 'Gender', c("Male" = 1, "Female" = 0, "Both"), selected = "Both"),
      
      sliderInput('agran', 'Age Range', min = age.range.opp.sex[1], max = age.range.opp.sex[2], value = age.range.opp.sex),
      
      checkboxGroupInput('rce', 'Race', choices = c("Black/African American" = 1, "Caucasian" = 2, "Hispanic" = 3, "Asian/Pacific Islander" = 4, "Other" = 6), selected = c(1,2,3,4,6) )
  ),

  #End of SidebarPanel code----------------------------------------------
  
  #Tabsetpanel separation for each individuals different analysises
  mainPanel(
    tabsetPanel( 
      
      tabPanel("Data Overview",
               h1("A Speed Dating Experiment"),
               
               
               h3("Where Did This Data Come From?"),

               p("This data was compiled by Ray Fisman and Sheena Lyengar, two Columbia Business School professors. 
                 The purpose of their data collection was to find evidence for their paper 'Gender Differences in Mate Selection: Evidence From a Speed Dating Experiment'. 
                 We found this data source through kaggle which offers users a database of public data sets and other resources similar to the one this app is built around."),
               
               helpText("Click here to see the data source:", a("Speed Dating Experiment",target = "_blank", href = "https://www.kaggle.com/annavictoria/speed-dating-experiment")),
               
               helpText("Click here to see the essay that utilizes this data set:", a("Essay", target = "_blank", href = "http://faculty.chicagobooth.edu/emir.kamenica/documents/genderDifferences.pdf")),
               
               
               h3("How Was This Data Collected?"),

               p("This data was collected using a series of speed dating events ranging from 2002-2004.
                 As the night continued participants would record their experience with every person they met that night using a rating system that was focused on the attributes of attractiveness, sincerity, intelligence, fun, ambition, and shared interests.
                 Not only was data collected about the date, but a questionnaire was used to gather information about people's occupations, habits, demographics, and beliefs."),
               
               
               h3("How Will We Analyze This Data?"),

               p("To explore the data we broke it up into three parts. The first part of our analysis will explore how careers and occupations have different effects on an individual's attribute ratings and social life. 
                 Then we will explore the effect that race played in matching during the event. Finally we broke down the survey data to analyze how the experience of speed dating changed the individuals beliefs about themselves and their peers by analyzing how each participants ratings from the different survey questions changed after the experiment was conducted."),
               
               
               h3("Who Would Be Interested in Our Findings?"),
               
               p("The main audience for our analysis is college students, considering that we are presenting our findings to a college class. However, this experiment includes a wide range of ages, so if anyone is looking for dating trends this data can apply to anyone."),
               
               
               h3("Notes About The Data"),

               p("Some key elements of the data to consider. The biggest restriction about the data set is that no experiments were conducted to find dating trends in the same sex. Because of this our analysis is mainly applicable to heterosexual dating trends. Also a variety of races lacked from the experiment, and the majority of the participants were Caucasian. 
                 Finally the data is not representative of the entire population, not only are all races and sexual preferences represented but the data set is too small to apply to the population.")),
      
      
      tabPanel("Race Trends",
               h1("Analyzing Matches By Race"),
               
               plotOutput('plot.race', brush = "plot_brush"),
               
               verbatimTextOutput("info.race"),
               
               
               h3('Percentage of Positive Matches by Race'),

               p('This bar graph shows how members of the race you have selected matched members of other races during speed
                 dating; it is important to note that while a category for Native Americans was included in the raw data, no
                 individuals who identified as Native American took part in this study. The bars represent the percent of
                 individuals in each race that your selected race considered a "match" divided by the total amount of dates
                 your selected race had with each race. Do note that the bar graph scales for each selected race, so keep an
                 eye on the y axis!'),
               
               
               h3('Analysis of Data'),

               p('For the purpose of this analysis we will break up our data into the six different race groups that the data
                  presents. The maximum match percentage across the board was Black individuals and other Black individuals, 
                  with 56% of dates resulting in a match between these groups of individuals. The lowest match percentage was tied
                  between Asian individuals with Asian individuals and Asian individuals with Other individuals, making a .13 percent
                  match rate. The following are the prominent statistics for each race group:'),

               p('Black individuals matched with other Black individuals the most with a match rate of 56% and matched with Asian
                 individuals the least, with a percentage below .5. Caucasian and Latino match rates were comparable with rates
                 of 18% and 21%, respectively. Match rate with individuals who identified as another gender were slightly above
                 match rates with Asians, but not enough to bring it above 1%.'),

               p('Caucasian individuals matched with Latino individuals the most with
                 a match rate of 18% followed closely by Caucasian individuals and
                 Black individuals, deviating by roughly 1 percent. Both Asian and
                 Other match rates were very close to 1 percent.'),

               p('Latino individuals matched with Latino individuals the most with a
                 match rate of 23 percent. They matched with Asian individuals the
                other match rates were near 20, 18, and 1 percent respectively.'),

               p('Asian individuals matched with Black individuals the most with a 
                 match rate of 18 percent, and matched with other Asians the least
                 with a value of less than 1 percent, followed closely by Other
                 individuals slightly below 1 percent as well. The match rates for
                 Caucasian and Latino individuals were both slightly above 13 
                 percent.'),

               p('Individuals who identied as the race Other matched with Black
                 individuals the most with a rate of 41 percent and matched with
                 Other individuals the least with a rate of 9 percent. Latino,
                 Caucasian, and Asian match rates were 34, 19, and 18 percent
                 respectively.')
               ),

      
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
                  Film, language and fine arts also seemed to follow the Social Sciences, showing a coloration between the arts, and speed dating to meet new people."), 
                 
                p("Although a large majority of the data followed a similar pattern, there are endless inferences that can be made, and it does seem as though the field 
                  you are in can affect why and how you speed date.")
    ),
    
    
    tabPanel(
      "Survey Analysis",
      h1("Breaking Down Survey Responses"),
      
      plotOutput('plotbfafpoint', click = 'clk'),
      
      verbatimTextOutput('outp'),
      
      tableOutput('plotbfafbar'),
      
      textOutput('text.gra'),
      
      
      h3("Analysis of Data"),

      p("This data is very unique because it allows for the user to see how peoples preferences changed after their experience speed dating. 
        Because of everything that was recorded we can do a broad analysis that looks at all the data, to a more focused analysis that explores how the experimental 
        experience effect people of different races, ages, and genders. To focus the analysis I will be organizing my write up over the different survey questions."),

      p("The first survey question was 'What do you look for in a person?' Without filtering the data, the largest increase in trait rating was attractiveness. The average initial  rating was 22.32, and after the experience it was 25.92. The larges decrease in rating importance came from intelligence that dropped from an average of 20.18 to 17.18.
        When looking at the element of gender males held true to the overall average with their largest rating increase coming from the attractiveness rating which went from 26.50 to 29.98. Their greatest decrease in rating was intelligence that dropped from an initial attribute  rating of 19.47 to 16.72.
        Just like the males, the females greatest change was the rating of attractiveness which went from 17.96 to 21.68, and their greatest decrease was intelligence which fell by 1.83 points.
        Out of all the races, the only race to defy the maximum increase in rating as attractiveness was the group of other races who had its largest increase from the trait of having shared interests."),

      p("The second survey question was 'what does your same gender look for?' Looking at the entire data, the greatest trait increase was sincerity which increased from 12.22 to 13.47. The greatest decrease was fun which fell from 18.22 to 17.52.
        When looking at gender the male beliefs where consistent with what they find important when searching for a partner. The greatest rating increase was ambition and went from 7.94 to 9.04. For females their greatest increase was in sincerity.
        Exploring all of the races there was a similar trend as the genders, just more extreme due to the smaller sample size. 
        "),

      p("The third survey question was 'what does the opposite gender look for?' By the end of the experiment the greatest rated trait overall was attractiveness with a final rating of 28.89.
        The highest values for both genders was attractiveness however, females believed males thought attractiveness was more important with a final value of 32.70 compared to the males belief about females interests in looks which finished at around 25.24.
        Over the course of the night both genders however, went in opposite directions. Males started to believe  that females were more interested in attractiveness and females showed a trend representing their belief that males do not care about looks as much.
        Across all of the races attractiveness was the highest rated value in each one, showing peoples emphasis on the importance of appearance."),
      
      
      h3("Talking About The First Three Questions"),

      p("The first three questions are very insightful about our own interests and beliefs. Attractiveness was by far the highest rated value out of all the values. So much so that in all of the questions asking about how your gender and the opposite gender perceived attractiveness, it was peaking high 20s to low 30s in importance.
        The light in the data though comes from the survey question about why we look for in a person. Though appearance was still the highest rated value, other values were a lot closer to its final results.
        This can be due to a lot of things, but the biggest one being our belief that appearance holds high importance in society. However, when people stopped analyzing what they believed to be society's trend,
        their own beliefs were a lot more rounded that what was thought to be true."),
      
      
      h3("Continuing with the questions"),

      p("The fourth survey question was 'how do you measure up to expectations?' The overall data shows a decreased rating in almost every category exceptfor a slight increase in attractiveness. Males and females kept true to the overall trend by only feeling more attractive leaving the event. All of the other categories 
        decreased showing a possible loss of confidence in certain traits that came over the experiment. Only two races had varying results, the first on came from the Black/African American race which had a greater intelligence rating leaving the event along with an increased activeness rating. The other race group was the only 
        group who did not have an increased attractiveness rating but instead they had an increased ambitious rating. This could be due to different places of residence, culture or just the fact that the other group has a much smaller data set."),

      p("The fifth and final survey question was 'how do you think others perceive you?' The analysis of the entire data set showed no increase in rating value by the end of the experiment. This trend did not change over gender, except for a couple of traits in the black/African American and other races. 
        The Black/African American race experienced an increased rating in the traits of fun and ambition, while the other race group experienced an increased rating in fun."),
      
      
      h3("Talking About Questions Four and Five"),

      p("Questions four and five are important questions. Analyzing how people think of themselves after interacting with others and putting themselves in a vulnerable position is important. From the data it's clear to see that only a select number of conditions would warrant a higher self rating.
        One reason for this could be the fact that people are very critical of themselves. In a dating situation people will tend to notice their minor mistakes more making them think that they 'messed up' more than they truly did.
        This data can also be due to the fact that after meeting a new group of people, they can change how they had thought about themselves before hand, trying to rank themselves in the group where they think they truly fall. However, the second reason is more far fetched.")
    )
   )
  )
 )
)

#UI code ends here-------------------------------------------------------------------------------------



#Server code starts here-------------------------------------------------------------------------------
dating.server <- function(input, output) {
  
  #Filters the data frames above based on the users input and then creates a data frame to be used for the survey analysis.
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

  #Output a scatter plot for the survey analysis
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
  
  #create a reactive summary dataframe containing the mean values for the two traits for the survey analysis
  sumserv <- reactive({
    
    `Average Initial Attribute Rating` <- mean(filteredSurvey.data()[,5])
    `Average Final Attribute Rating` <- mean(filteredSurvey.data()[,6])
    
    data <- data.frame(`Average Initial Attribute Rating`, `Average Final Attribute Rating`)
    names(data) <- gsub(".", " ", names(data), fixed = TRUE)
    
    return(data)
  }) 
  
  #outputs a table showing the means of each column for the survey analysis
  output$plotbfafbar <- renderTable({
    return(sumserv())
  })
  
  #outputs the interactive click information to show the before and after values of each point in the survey analysis
  output$outp <- renderText({
      
    if(is.null(input$clk)){
      return("Pick a Point \n")
      
    }else{
      paste0("Initial Rating = ", round(input$clk$x, 1), "\n", "Final Rating = ", round(input$clk$y,1), "\n")
    }
    })
  
  #Creates an updated graph explanation for the survey analysis part
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
      ques <- "How do you think others perceive you?"
    }
    
    `Average Initial Attribute Rating` <- round(mean(filteredSurvey.data()[,5]),1)
    `Average Final Attribute Rating` <- round(mean(filteredSurvey.data()[,6]),1)
    
    text.out <- paste("When asked the question'",ques,"' the average initial response about the trait was", `Average Initial Attribute Rating`, "and a day after the event the average final rating for the trait was", `Average Final Attribute Rating`,". The graph above shows this trend visually and colors each point depending on how the individuals response changed after a day. Blue represents a positive change, while red represents no/a negitive change in that specific trait. The graph also of trend line that can show the trait change, a positive exponential trend meaning increased ratings and a negitive exponential trend meaning decreased ratings. If the trend appears linear or close to linear that means the trait ratings changed very little if at all. The graph also offers a lot of interactive elements, allowing users to view the data by race, age range, gender, question asked and trait. Users can also click different points on the graph and see the individuals initial rating for that trait and their final rating as well." )
    
    return(text.out)
  })
}

shinyApp(ui = dating.ui, server = dating.server)
