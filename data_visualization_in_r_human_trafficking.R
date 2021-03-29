#######################################################################
#                                                                     #
#    DATA VISUALIZATION IN R                                          #
#    HUMAN TRAFFICKING                                                #
#                                                                     #
#######################################################################


######## LIBRARY: Loading Packages  ----
#Checking the version 

getRversion()

#Packages and libraries

#Install packages
if(!'ggplot2'%in%installed.packages()){
  install.packages('ggplot2')}
if(!'ggthemes'%in%installed.packages()){
  install.packages('ggthemes')}
if(!'nlme'%in%installed.packages()){
  install.packages('nlme')}
if(!'gapminder'%in%installed.packages()){
  install.packages('gapminder')}
if(!'gganimate'%in%installed.packages()){
  install.packages('gganimate')}
if(!'ggExtra'%in%installed.packages()){
  install.packages('ggExtra')}
if(!'psych'%in%installed.packages()){
  install.packages('psych')}
if(!'reshape2'%in%installed.packages()){
  install.packages('reshape2')}
if(!'dplyr'%in%installed.packages()){
  install.packages('dplyr')}
if(!'nycflights13'%in%installed.packages()){
  install.packages('nycflights13')}
if(!'ggcorrplot'%in%installed.packages()){
  install.packages('ggcorrplot')}
if(!'waffle'%in%installed.packages()){
  install.packages('waffle')}
if(!'tidyr'%in%installed.packages()){
  install.packages('tidyr')}
if(!'scales'%in%installed.packages()){
  install.packages('scales')}
if(!'ggalt'%in%installed.packages()){
  install.packages('ggalt')}
if(!'data.table'%in%installed.packages()){
  install.packages('data.table')}
if(!'extrafont'%in%installed.packages()){
  install.packages('extrafont')}
if(!'lubridate'%in%installed.packages()){
  install.packages('lubridate')}
if(!'DT'%in%installed.packages()){
  install.packages('DT')}
if(!'grid'%in%installed.packages()){
  install.packages('grid')}
if(!'gridExtra'%in%installed.packages()){
  install.packages('gridExtra')}
if(!"prettydoc" %in% installed.packages()) {
  install.packages("prettydoc")}
if(!"devtools" %in% installed.packages()) {
  install.packages("devtools")}
if(!"tidyverse" %in% installed.packages()) {
  install.packages("tidyverse")}
if(!"ggdark" %in% installed.packages()) {
  install.packages("ggdark")}
if(!"here" %in% installed.packages()) {
  install.packages("here")}
if(!"gifski" %in% installed.packages()) {
  install.packages("gifski")}
if(!"forcats" %in% installed.packages()) {
  install.packages("forcats")}
if(!"tufte" %in% installed.packages()) {
  install.packages("tufte")}
if(!"colorspace" %in% installed.packages()) {
  install.packages("colorspace")}
if(!"viridisLite" %in% installed.packages()) {
  install.packages("viridisLite")}
if(!"formatR" %in% installed.packages()) {
  install.packages("formatR")}
if(!"DiagrammeR" %in% installed.packages()) {
  install.packages("DiagrammeR")}
if(!"xaringan" %in% installed.packages()) {
  install.packages("xaringan")}
if(!"ggridges" %in% installed.packages()) {
  install.packages("ggridges")}
if(!"GGally" %in% installed.packages()) {
  install.packages("GGally")}
if(!"corrplot" %in% installed.packages()) {
  install.packages("corrplot")}
if(!"ggplot2movies" %in% installed.packages()) {
  install.packages("ggplot2movies")}
if(!"ggpointdensity" %in% installed.packages()) {
  install.packages("ggpointdensity")}
if(!"rstat" %in% installed.packages()) {
  install.packages("rstat")}
if(!"ggstatsplot" %in% installed.packages()) {
  install.packages("ggstatsplot")}
if(!"ggbeeswarm" %in% installed.packages()) {
  install.packages("ggbeeswarm")}
if (!require(devtools)) {
  install.packages('devtools')
}
devtools::install_github('Ather-Energy/ggTimeSeries')
devtools::install_github('erocoar/gghalves')

#Connect with the libraries
library(ggplot2)
library(ggthemes)
library(nlme)
library(gganimate)
library(gapminder)
library(ggExtra)
library(psych)
library(reshape2)
library(dplyr)
library(nycflights13)
library(ggcorrplot)
library(waffle)
library(tidyr)
library(scales)
library(ggalt)
library(data.table)
library(extrafont)
library(lubridate)
library(DT)
library(grid)
library(gridExtra)
library(prettydoc)
library(devtools)
library(tidyverse)
library(ggdark)
library(here)
library(gifski)
library(forcats)
library(tufte)
library(colorspace)
library(viridisLite)
library(formatR)
library(DiagrammeR)
library(xaringan)
library(ggridges)
library(GGally)
library(ggplot2movies)
library(corrplot)
library(ggpointdensity)
library(ggstatsplot)
library(ggTimeSeries)
library(ggbeeswarm)
library(gghalves)


######## THEME: Defining the general design   ----

#Defining the general colors to avoid hard coding
#fill_color = '#550000'
#decoration_color = '#FFAAAA'
#main1_color = '#D46A6A'
#main2_color = '#AA3939'

fill_color = '#1A1A1C' #dark grey (background)
decoration_color = "white" 
main1_color = '#A60311' #dark red
main2_color = '#F21F31' #bright red
contrast_color = '#008B8B' # cyan contrasting color
weak1_color = "#D98997" #light highlights
weak2_color = "#D46A6A" #light highlights rose
weak3_color = "#A6A6A6" #light grey

color_palette <- c(main1_color, main2_color, contrast_color, weak1_color, weak2_color, weak3_color)


#Create a personalized theme
g1_theme <- theme_bw() + theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.title = element_text(
    size = 16,
    hjust = 0.2,
    color = decoration_color
  ),
  axis.title = element_text(
    size = 12,
    hjust = 0.5,
    color = weak3_color
  ),
  axis.text = element_text(colour = weak3_color, size = 10),
  axis.ticks = element_blank(),
  #axis.line.x = element_line(
    #colour = decoration_color,
    #size = 0.3,
    #linetype = "dashed"
  #),
  axis.line = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  strip.text = element_text(size = 12, color = weak3_color),
  panel.background = element_rect(fill = fill_color),
  strip.background = element_rect(fill = fill_color),
  plot.background = element_rect(fill = fill_color),
  legend.text	= element_text(
    size = 10,
    hjust = 0.5,
    color = weak3_color
  ),
  legend.position = c(0.815, 0.27),
  legend.key = element_blank(),
  legend.title = element_blank(),
  legend.background = element_rect(fill = fill_color)
)

#Now we set the new defined theme to the default option
theme_set(g1_theme)

######## PREP I: IMPORTING DATASET ----

# Import Dataset from Gist
filename <-
  "https://gist.githubusercontent.com/frau-web/5f3e9bf04fbe1f61315594261d6ace5d/raw/570b36653a471fd674305a7898687d7009b3718e/Human_Trafficking_raw.csv"
df <- read.csv(filename,
               na = '-',
               header = TRUE,
               strip.white = TRUE)
df %>% select(-c(
  "Datasource",
  "majorityStatus",
  "majorityStatusAtExploit",
  "majorityEntry"
)) -> df # Drop Variables which will not be used in the analysis


# Import Continent mapping from Gist
filename <-
  (
    "https://gist.githubusercontent.com/frau-web/50f65ad7110f0ab0af760b97b51a3534/raw/ab2346c8189859bc1cbee2ce4e99a1784e390a1a/Continents.csv"
  )
continents <-
  read.csv(filename,
           na = '-',
           header = TRUE,
           strip.white = TRUE)
continents %>% select(-c("Continent_Code")) -> continents # Drop unnecessary Columns



# Outer Join: Mapping Continents with our Dataframe to enable filtering for continents
library(sqldf)
df <- sqldf("select a.*,b.Citizenship_Continent, b.Citizenship_Country from df a left outer join continents b on a.citizenship=b.Two_Letter_Country_Code")
df <-
  sqldf(
    "select a.*,b.Exploitation_Continent, b.Exploitation_Country from df a left outer join continents b on a.CountryOfExploitation=b.Two_Letter_Country_Code"
  )



######## PREP II: Prepare DATASET ----

# Rename Variables
df %>% rename(Year = yearOfRegistration) -> df


# Factorize Variables
df %>% mutate(gender = factor(gender, levels = unique(df$gender))) -> df #factor levels
df %>% mutate(meansOfControlConcatenated = factor(meansOfControlConcatenated, levels = unique(df$meansOfControlConcatenated))) -> df #factor levels
df %>% mutate(typeOfExploitConcatenated = factor(typeOfExploitConcatenated, levels = unique(df$typeOfExploitConcatenated))) -> df #factor levels


# Factor Variable ageBroad and rename it to Age 
df %>% rename(Age = ageBroad) -> df
#df <- df %>% mutate(Age = replace(Age, Age=="-99",""Not reported""))
unique(df$Age) #check unique values
df %>% mutate(Age = factor(
  Age,
  levels = c("0--8", "9--17", "18--20", "21--23", "24--26", "27--29", "30--38", "39--47", "48+","-99"),
  ordered = TRUE
)) -> df


# Merge null values into one value
df <- df %>% mutate(Exploitation_Continent = replace(Exploitation_Continent, Exploitation_Continent==0,"Not reported")) %>% 
  mutate(Exploitation_Continent = replace(Exploitation_Continent, Exploitation_Continent=="NA","Not reported")) %>% 
  mutate(Exploitation_Continent = replace(Exploitation_Continent,is.na(Exploitation_Continent),"Not reported")) %>% 
  mutate(Exploitation_Continent = replace(Exploitation_Continent,Exploitation_Continent=="None","Not reported"))



df <- df %>% mutate(Citizenship_Continent = replace(Citizenship_Continent, Citizenship_Continent==0,"Not reported")) %>% 
  mutate(Citizenship_Continent = replace(Citizenship_Continent, Citizenship_Continent=="NA","Not reported")) %>%
  mutate(Citizenship_Continent = replace(Citizenship_Continent,is.na(Citizenship_Continent),"Not reported")) %>% 
  mutate(Citizenship_Continent = replace(Citizenship_Continent,Citizenship_Continent=="None","Not reported"))



df <- df %>% mutate(typeOfExploitConcatenated = replace(typeOfExploitConcatenated, typeOfExploitConcatenated=="","Other"))
df <- df %>% mutate(typeOfLabourConcatenated = replace(typeOfLabourConcatenated, typeOfLabourConcatenated=="","Not specified")) %>% 
  mutate(typeOfLabourConcatenated = replace(typeOfLabourConcatenated, typeOfLabourConcatenated=="OtherNot specified","Not specified"))



# Drop values for 2019 since only 28 cases
df <- subset(df, Year!= 2019)


# Create a Variable Age Category by Gender
df$agecat[df$gender=="Female" & (df$Age =="0--8" | df$Age =="9--17")] <- "Girl" 
df$agecat[df$gender=="Female" & (df$Age =="18--20" | df$Age =="21--23" | df$Age =="24--26" | df$Age =="27--29" | df$Age =="30--38" | df$Age =="39--47" | df$Age =="48+")] <- "Woman"
df$agecat[df$gender=="Male" & (df$Age =="0--8" | df$Age =="9--17")] <- "Boy" 
df$agecat[df$gender=="Male" & (df$Age =="18--20" | df$Age =="21--23" | df$Age =="24--26" | df$Age =="27--29" | df$Age =="30--38" | df$Age =="39--47" | df$Age =="48+")] <- "Man"
df$agecat[df$Age =="-99" | is.na(df$Age)] <- "Not reported"



## FEATURE ENGINEERING
# Exploring Exploitation_Country==Nome, where are these people from: Majority from Philippines
df %>% filter(Exploitation_Country=="None") %>% group_by(Citizenship_Country,Year, gender, agecat) %>% 
  summarize(count_by_year = n()) 

# Philippine people mostly exploited in Asia so we reassign people from Philippines to Exploitation Continent Asia
df %>% filter(Citizenship_Country=="Philippines") %>% group_by(Exploitation_Country,Year, gender, agecat) %>% 
  summarize(count_by_year = n()) 

df <- df %>% mutate(Exploitation_Continent = replace(Exploitation_Continent, Citizenship_Country=="Philippines","Asia"))



######## PART 1: Exploratory Data Analysis ----
names(df)
head(df, n=10)
str(df)
summary(df)

## Visual exploration for categoricals
#install.packages("inspectdf")
library(inspectdf)
show_plot(inspect_cat(df))

######## PART 2: Intro ----

## Chart 1.1: Development of global reported numbers of human trafficking by year
#install.packages("gganimate")
#library(gganimate)
df %>% group_by(Year) %>% summarize(count_by_year = n()) %>% 
  ggplot(aes (x = Year, y = count_by_year)) + 
    geom_line(lwd=1.5, show.legend=FALSE, color=main2_color) +
    labs(title="Number of reported Human Trafficking Victims globally",
       x = "Year",
       y = "Reported victims") #+
  transition_reveal(Year)


## Chart 1.2: Global reported numbers, by continent (small multiples)
df %>% group_by(Year, Exploitation_Continent) %>% 
  summarize(count_by_year = n()) %>% 
  ggplot() +
  geom_line(aes (Year, count_by_year,color=Exploitation_Continent), lwd=0.8, show.legend=FALSE) + 
  facet_wrap(~ Exploitation_Continent, ncol=5, strip.position="bottom") +
  scale_color_manual(values=c(main2_color,main2_color,main2_color,main2_color,main2_color,main2_color)) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        strip.background.x = element_blank()) +
  labs(title="Reported Human Trafficking Victims by continent",
       x = "Year",
       y = "Reported victims")



# Chart 1.3: Understand Asia Spike
df %>% filter(Exploitation_Continent=="Asia") %>% group_by(Year, Exploitation_Country) %>% 
  summarize(count_by_year = n()) %>% filter(count_by_year>0) %>% 
  ggplot(aes(x=Year, y=count_by_year, fill = count_by_year > 3000)) +
  geom_col(position = "stack", show.legend=TRUE) + 
  scale_fill_manual(labels=c("Other Asian countries","Philippines"),values = c(weak3_color, muted(main1_color)))+
  theme(axis.text.x = element_text(angle=45, hjust=1),
      strip.background.x = element_blank(),
      legend.position="bottom") +
  labs(title="Reported Human Trafficking Victims in Asia by Exploitation Country",
     x = "Year",
     y = "Reported victims")




######## PART 3: Who are the victims and why? ----

# Chart 3.1: By Gender: Share of men vs women
gender_df <- df %>% group_by(gender) %>% summarize(count_by_year = n())
gender_df[1,2]/(gender_df[1,2]+gender_df[2,2]) #share females (72% globally)

# Importing mock-dataframe to mock share women 72% vs. men 28%
filename <-
  "https://gist.githubusercontent.com/frau-web/dc93b343879a6d1b5a8c25a276c943cd/raw/ac881280700b8c835ae1453eeb4a030e6341d084/Gender_distribution.csv"
gender_dist <- read.csv(filename, 
                        sep = ",",
                        header = TRUE,
                        strip.white = TRUE)
as.character(gender_dist$x, gender_dist$y)

# Graph: Plot Chart to show Gender Distribution 
ggplot() + 
  geom_point(data=gender_dist, aes(x=x, y=y, color=z), shape=16, size=15)+
  theme(line=element_blank(),rect=element_blank()) + 
  labs(title="Gender Share of Reported Human Trafficking Victims 2002-2018")+
  theme(legend.position = "right",
        axis.text=element_blank(),
        axis.title = element_blank())+
  scale_color_manual(values = c(weak2_color, contrast_color))

# Chart 3.2: By Age/Gender Pyramid
df %>% filter(Age!="-99") %>% group_by(Age, gender) %>% summarize(count_by_age = n()) -> by_age
# Plotting the Age/Gender Pyramid
ggplot(by_age, aes(x = Age, fill = gender,
                   y = ifelse(test = gender == "Male",
                              yes = -count_by_age, no = count_by_age))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(by_age$count_by_age) * c(-1,1)) +
  coord_flip() + 
  labs(title="Reported Human Trafficking Victims globally, by Age and Gender (2002-2018)",
       x = "Age",
       y = "Reported victims") +
  theme(legend.position = "right")+
  scale_fill_manual(values = c(weak2_color, contrast_color))


# Data to be interpreted with care since especially for women high share of 
# data without age given, could totally change the picture



# Chart 3.3: Reasons for exploitation 
# (a) Both Genders
df %>% filter(typeOfExploitConcatenated!="-99") %>% group_by(typeOfExploitConcatenated) %>%
  summarize(count = n()) %>%
  ggplot(aes(x=reorder(typeOfExploitConcatenated, count), y = count, fill=count))+
  geom_col(show.legend=FALSE)+
  coord_flip()+
  theme(strip.background.x = element_blank(), 
        axis.title.y=element_blank()) +
  scale_fill_gradient2(position="left",low="white", mid=weak1_color, high=muted(main2_color))+
  labs(title="Type of Exploit of Reported Human Trafficking Victims (2002-2018)",
     x = "Type of Exploit",
    y = "Reported Victims")

# (b) By Gender
df %>% filter(typeOfExploitConcatenated!="-99") %>% group_by(gender, typeOfExploitConcatenated) %>% 
  summarize(count = n()) %>% filter(count>50) %>% 
  ggplot(aes(x=reorder(typeOfExploitConcatenated, count), y = count, fill=gender))+
  geom_col(position = "dodge")+
  coord_flip()+
  facet_wrap(. ~ gender) +
  labs(title="Main Types of Exploit of Reported Human Trafficking Victims (2002-2018)",
       x = "Type of Exploit",
       y = "Reported Victims") +
  theme(strip.background.x = element_blank(), 
        axis.title.y=element_blank(),
        legend.position="bottom") +
  scale_fill_manual(values = c(weak2_color, contrast_color))


## Chart 3.4: Men & Labor
# Forced Labour by age group
df %>% filter(typeOfExploitConcatenated=="Forced labour", gender=="Male", Age!="-99") %>% group_by(Age) %>%  
  summarize(count = n()) %>% ggplot(aes(x = Age, y=count, fill=count>500)) + 
  geom_bar(colour = "black", stat="identity",show.legend = FALSE) +
  scale_fill_manual(values = c(weak3_color,contrast_color)) +
  labs(title="Age distribution of Men reported to have been exploited for forced labour (2002-2018)",
     x = "Age",
     y = "Reported Victims") 

# Forced Labour by Type
df %>% filter(typeOfLabourConcatenated!="-99", typeOfExploitConcatenated!="") %>%  group_by(typeOfLabourConcatenated) %>%  
  summarize(count = n()) %>% filter(count>70) %>% 
  ggplot(aes(x = reorder(typeOfLabourConcatenated, count), y=count,fill=count>2000)) + 
  geom_bar(colour = "black", stat="identity", show.legend=FALSE)+
  coord_flip() +
  scale_fill_manual(values = c(weak3_color,contrast_color)) +
  theme(axis.title.y=element_blank())+
  labs(title="Main Types of Forced Labour of Men reported to have been exploited (2002-2018)",
     x = "Type of Forced Labour",
     y = "Reported Victims") 

#df2 <- df
#df2<-df %>% mutate_all(funs(str_replace(df$meansOfControlConcatenated, '\t', '/')))
#df2<-df %>% mutate_all(funs(str_replace(., "\t", "/")))

#unique(df$meansOfControlConcatenated)


# Control: How is it possible to control men of that age
df %>% filter(gender=="Male",meansOfControlConcatenated!="-99" ) %>% group_by(meansOfControlConcatenated) %>%  
  summarize(count = n()) %>% filter(count>30) %>% 
  ggplot(aes(x=reorder(meansOfControlConcatenated, count), y = count, fill=count>350))+
  geom_col(colour = "black", stat="identity", show.legend=FALSE)+
  coord_flip()+
  theme(axis.title.y=element_blank())+
  scale_fill_manual(values = c(weak3_color,contrast_color)) +
  labs(title="Main Means of Control of Male Trafficking Victims (2002-2018)",
       x = "Means of Control",
       y = "Reported Victims") 


## Chart 3.5: Women & Sex
# Sexual Exploitation by age group
df %>% filter(typeOfExploitConcatenated=="Sexual exploitation", gender=="Female", Age!="-99") %>% group_by(Age) %>%  
  summarize(count = n()) %>% ggplot(aes(x = Age, y=count, fill=count>2400)) + 
  geom_bar(colour = "black", stat="identity", show.legend=FALSE) + 
  scale_fill_manual(values = c(weak3_color,weak2_color)) +
  labs(title="Age distribution of Women reported to have been sexually exploited (2002-2018)",
       x = "Age",
       y = "Reported Victims") 


# Sexual Exploitation by Type
df %>% filter(typeOfSexConcatenated!="-99", typeOfSexConcatenated!="") %>%  group_by(typeOfSexConcatenated) %>%  
  summarize(count = n()) %>% filter(count>0) %>% 
  ggplot(aes(x = reorder(typeOfSexConcatenated, count), y=count,fill=count>1000)) + 
  geom_bar(colour = "black", stat="identity", show.legend=FALSE)+
  coord_flip()+
  scale_fill_manual(values = c(weak3_color,weak2_color)) +
  theme(axis.title.y=element_blank())+
  labs(title="Main Types of Sexual Exploitation of Women (2002-2018)",
       x = "Type of Sexual Exploitation",
       y = "Reported Victims") 



# Control
df %>% filter(gender=="Female",meansOfControlConcatenated!="-99" ) %>% group_by(meansOfControlConcatenated) %>%  
  summarize(count = n()) %>% filter(count>200) %>% 
  ggplot(aes(x=reorder(meansOfControlConcatenated, count), y = count, fill=count>1000))+
  geom_col(colour = "black", stat="identity", show.legend=FALSE)+
  coord_flip()+
  theme(axis.title.y=element_blank())+
  scale_fill_manual(values = c(weak3_color,weak2_color)) +
  labs(title="Main Means of Control of Female Trafficking Victims (2002-2018)",
       x = "Means of Control",
       y = "Reported Victims") 





# Chart 3.3: Movements - where do people come from and where are they exploited? 
#install.packages("ggforce")
library("ggforce")

# Create condensed data frame with variables needed
df %>% group_by(Citizenship_Continent,Exploitation_Continent) %>%  
  summarize(count_total = n()) %>% filter(!is.na(c(Citizenship_Continent))) %>% 
  filter(count_total>150) -> from_to

df %>% group_by(Citizenship_Continent,Exploitation_Continent) %>%  
  summarize(count_total = n())
from_to <- gather_set_data(from_to,1:2) #transform df into long format

# Plot Flow Chart
ggplot(from_to, aes(x, id = id, split = y, value = count_total)) +
  geom_parallel_sets(aes(fill = count_total), alpha = 0.4, axis.width = 0.2, show.legend=FALSE, fill=main2_color) +
  geom_parallel_sets_axes(axis.width = 0.2) +
  geom_parallel_sets_labels(colour = weak3_color,angle = 360,size = 4)+ 
  theme(axis.text.y=element_blank())+
  labs(title="Origin of Victims of Human Trafficking vs. their place of Exploitation (2002-2018)",
       x = "Victim Origin vs. Place of Exploitation")


######## PART 4: Conclusion -----
# Chart 4.1: By Expl country: Top10 EU Countries 
df %>% filter(Exploitation_Continent=="Europe") %>% 
  group_by(Exploitation_Country) %>%  
  summarize(count = n()) %>% filter(count>50) %>% 
  ggplot(aes(x=reorder(Exploitation_Country, count), y = count, fill=count))+
  geom_col(colour = "black", stat="identity", show.legend=FALSE)+
  coord_flip()+
  theme(axis.title.y=element_blank())+
  scale_fill_gradient2(position="left",low="white", mid=weak1_color, high=muted(main2_color))+
  #scale_fill_manual(values = c(weak3_color,main2_color)) +
  labs(title="Victims of Human Trafficking in Europe by country (2002-2018)",
       x = "Exploitation Country",
       y = "Reported Victims") 







