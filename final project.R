rm(list=ls(all = TRUE))     # this code is used to clear the environment in R
graphics.off()              # turning off all the graphical objects
shell("cls")                #for clearing the console

setwd("C:\\Users\\DELL\\Desktop\\stats 317")
getwd()

library(readtext)
library(treemap)
library(treemapify)
library(readxl)
library(dplyr)
library(tidyverse)
library(treemap)
library(DataExplorer)
library(corrplot)
library(ggcorrplot)
library(visreg)
library(janitor)
library(scatterplot3d)
library(GGally)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(RCurl)
library(XML)


data<-read_excel("project_data.xlsx")
View(data)

###############################
#
#
#tree maps and bar charts 
#
#
###############################

rating<-(data$`Rating of Online Class experience`)
table(rating)
rating_data<-data.frame(c("Average", "Excellent", "Good", "Poor", "Very Poor"), c(387,
                            98, 230, 30, 413))
names(rating_data)<-c("Ratings", "Values")

treemap(rating_data, index="Ratings", vSize="Values", type="index", palette = "Reds",
        title="Tree Map", fontsize.labels=12, fontsize.title=14)

rating_data$Percentage <- scales::percent(rating_data$Values / sum(rating_data$Values))

a<-ggplot(rating_data, aes(x=Ratings, y=Values)) + 
  geom_bar(stat = "identity", color="orange",
           fill="red") +
  labs("Rating of Online Class Experience") + theme_grey()

a + geom_text(aes(label = Percentage), 
              vjust = -0.5, 
              color = "black", 
              size = 4, 
              position = position_stack(vjust = 0.5))

medium<-data$`Medium for online class`
table(medium)
medium_data<-data.frame(c("Any Gadget", "Laptop/Desktop", "Smartphone", "Tablet"),c(
  5,550, 539, 37))
names(medium_data)<-c("Medium", "Values")

treemap(medium_data, index="Medium", vSize="Values", type="index", palette = "Reds",
        title="Tree Map", fontsize.labels=12, fontsize.title=14)
medium_data$Percentage <- scales::percent(medium_data$Values / sum(medium_data$Values))

b<-ggplot(medium_data, aes(x=Medium, y=Values)) + 
  geom_bar(stat = "identity", color="orange",
           fill="red") +
  labs("Medium for Online Class") + theme_grey()

b + geom_text(aes(label = Percentage), 
              vjust = -0.5, 
              color = "black", 
              size = 4, 
              position = position_stack(vjust = 0.5))

social<-data$`Prefered social media platform`
table(social)
social_data<-data.frame(c("Elyment", "Facebook", "Instagram", "Linkedin", "Omegle",
                         "Quora", "Reddit", "Snapchat", " Talklife", "Telegram",
                         "Twitter", " Whatsapp", "Youtube"),c(1, 52, 352, 61, 1, 1, 
                          5, 8, 1, 3, 28, 337, 314))
names(social_data)<-c("Social", "Values")

treemap(social_data, index="Social", vSize="Values", type="index", palette = "Reds",
        title="Preferred Social Media Platform", fontsize.labels=8, fontsize.title=14)

social_data$Percentage <- scales::percent(social_data$Values / sum(social_data$Values))


c<-ggplot(social_data, aes(x=Social, y=Values, fill=Social)) + 
  geom_bar(stat = "identity", color="orange",
           fill="red") + 
  labs("Preferred Social Media Platform") + theme_grey()

c + geom_text(aes(label = Percentage), 
              vjust = -0.5, 
              color = "black", 
              size = 4, 
              position = position_stack(vjust = 0.5))


################################################
#
#
#correlation matrix
#
#
################################################

data_numeric<-dplyr::select_if(data, is.numeric)
data_numeric
r<-cor(data_numeric, use = "complete.obs")
ggcorrplot(r, hc.order=TRUE, type = "lower", lab =TRUE)

############################
#
#general linear regression 
#
############################

data_clean<-clean_names(data)

data_clean$health_issue_during_lockdown<-as.factor(data_clean$health_issue_during_lockdown)
class(data_clean$health_issue_during_lockdown)

names(data_clean)

reg<-glm(health_issue_during_lockdown ~ age_of_subject +time_utilized +
           time_spent_on_online_class + time_spent_on_self_study +
            time_spent_on_fitness + time_spent_on_sleep , family="binomial", 
         data_clean)

summary(reg)

visreg(reg, "age_of_subject", gg=TRUE, scale="response") +
  labs(y="prob (health issues)", x="age",
       title="Relationship of Age and Health Issues During Lockdown")

visreg(reg, "age_of_subject", by="time_utilized", gg=TRUE, scale="response") +
  labs(y="prob (health issues)", x="age",
       title="Relationship of Age and Health Issues During Lockdown", 
       caption="Indiviuals who did not utilize their time during 
       lockdown were more susceptible to health problems")

visreg(reg, "time_spent_on_online_class", gg=TRUE, scale="response") +
  labs(y="prob (health issues)", x="Time Spent on Online Class",
       title="Relationship of Time Spent on Online Class and Health Issues")

visreg(reg, "time_spent_on_self_study", gg=TRUE, scale="response") +
  labs(y="prob (health issues)", x="Time Spent on Self Study",
       title="Relationship of Time Spent on Self Study and Health Issues")

visreg(reg, "time_spent_on_fitness", gg=TRUE, scale="response") +
  labs(y="prob (health issues)", x="Time Spent on Fitness",
       title="Relationship of Time Spent on Fitness and Health Issues")

visreg(reg, "time_spent_on_sleep", gg=TRUE, scale="response") +
  labs(y="prob (health issues)", x="Time Spent on Sleep",
       title="Relationship of Time Spent on Sleep and Health Issues")


########################################################
#
#
#point graph
#
#
########################################################

names(data_clean)
p<-ggplot(data_clean, aes(x=time_spent_on_online_class, y=time_spent_on_social_media, 
                       color=region_of_residence)) + geom_point(size=3, alpha=0.6) + 
  labs(title="Hours Spent on Study in Comparision to Sleep", y="sleep (hours)", 
       x="online class (hours)") + geom_smooth(method = "lm", se = FALSE)
p + labs(color="Region of Residence")

data$`Age of Subject`<-as.factor(data$`Age of Subject`)

ggplot(data, aes(x=`Age of Subject`, y=`Time spent on social media`, 
                 color=`Prefered social media platform`, shape=`Time utilized`)) +
  geom_point() + facet_wrap(~`Age of Subject`, scales="free")

###########################################
#
#
#histogram
#
#
############################################

ggplot(data_clean, aes(x=time_spent_on_online_class)) +
  geom_histogram(fill="red", color="orange") +
  facet_wrap(~region_of_residence) + labs(title="Time Spent on Online Class", x="Time (hours)")

###########################
#
#
#3d scatter plot
#
#
############################

three_d<-data_clean%>%select(id, time_spent_on_online_class,
                             time_spent_on_self_study, time_spent_on_social_media)
three_d<-three_d[c(1:45),]

View(three_d)

with (three_d, {
  s3d <- scatterplot3d(
    x = time_spent_on_online_class, y =time_spent_on_self_study, 
    z = time_spent_on_social_media, color="red", pch = 19, type = "h",
    main="How Did the the Sampled Individuals Utilize Their Time",
    xlab = "online classes (hours)",
    ylab = "self study (hours)",
    zlab = "social media (hours)")
  # convert 3-D coords to 2D projection
  s3d.coords <- s3d$xyz.convert(time_spent_on_online_class, 
                                time_spent_on_self_study, time_spent_on_social_media) # plot text with 50% shrink and place to right of points
  text(s3d.coords$x,
       s3d.coords$y,
       labels= three_d$id,
       cex= 0.75,
       pos = 4)
})

#####################################
#
#
#ggpairs visualization
#
#
#######################################

my_fn <- function(data, mapping, pts=list(), smt=list(), ...){
  ggplot(data = data, mapping = mapping, ...) + 
    do.call(geom_point, pts)
}

ggpairs(data_numeric, 
        lower = list(continuous = 
                       wrap(my_fn,
                            pts=list(size=0.5, colour="red"))),
        diag=list(continuous=wrap("barDiag", fill="orange", color="red")))

##############################
#
#
#word cloud
#
#
############################

doc <- readtext("317 research paper.docx")
stop_words <- c("a", "an", "and", "as", "at", "but", "by", 
                "for", "from", "in", "is", "it", "of", "on", 
                "or", "that", "the", "this", "to", "with", "time", "can",
                "spent", "also", "may")


# Create a corpus from the text data
corpus <- Corpus(VectorSource(doc))

# Preprocess the corpus
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, stop_words)

# Convert the pre-processed corpus to a document-term matrix
dtm <- DocumentTermMatrix(corpus)

# Get the frequency of each word2.
word_freq <- colSums(as.matrix(dtm))
word_freq

# Generate the word cloud with the word frequencies
wordcloud(names(word_freq), word_freq, max.words=70, 
          colors= brewer.pal(8, "Dark2"), scale=c(2.5, 0.25))
