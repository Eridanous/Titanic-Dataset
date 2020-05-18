# clear all
rm(list=ls())

library(data.table)
library(ggplot2)
library(ggExtra)
library(ggcorrplot)
# library(dplyr)
# library(ggfortify)

# getwd()
setwd("C:/Users/Eridanus/Desktop/R assignment/")

titanic0 = fread("titanic3.csv")

# we remove the columns we don't need
titanic = titanic0[,-c("name", "home.dest", "ticket", "V15","boat")]

# we remove the rows with no "survived" value (if any)
titanic = na.omit(titanic, cols = "survived")


# Important values
total_passengers = titanic[,.N]
male = titanic[sex=="male",.N]
female = titanic[sex=="female",.N]

bodies_found = titanic[,.N] - titanic[is.na(body),.N]
male_bodies_found = titanic[sex=="male",.N] - titanic[sex=="male"&is.na(body),.N]
female_bodies_found = titanic[sex=="female",.N] - titanic[sex=="female"&is.na(body),.N]
total_dead = titanic[survived == 0, .N]
survivors = titanic[survived == 1, .N]
# cabin_holders = titanic[,.N] - titanic[cabin== "", .N]
cabin_holders = titanic[cabin != "", .N]
class1 = titanic[pclass == 1, .N]
class2 = titanic[pclass == 2, .N]
class3 = titanic[pclass == 3, .N]

# test = titanic[sibsp<10 & sibsp>-1 & parch<10 & parch>-1,.N]

parch_new_col = data.table( parch_new =  titanic[,ifelse(parch<3,parch,"more than 2") ] )
titanic = cbind(titanic, parch_new_col)

# titanic$parch_new[titanic$parch_new==0] <- "0"
# titanic$parch_new[titanic$parch_new==1] <- "1"
# titanic$parch_new[titanic$parch_new==2] <- "2"
### titanic$parch_new[titanic$parch_new==3] <- "3"

sibsp_new_col = data.table( sibsp_new =  titanic[,ifelse(parch<4,parch,">3") ] )
titanic = cbind(titanic, sibsp_new_col)

age_new_col = titanic[,.(age_new =age)]
titanic = cbind(titanic, age_new_col)
  # data.table( age_new =  titanic[,.(age_new)] )

titanic$age_new = titanic[,.(age_new = ifelse(age<1, "infants", age_new))]
titanic$age_new = titanic[,.(age_new = ifelse(age>=1 & age<18, "children",  age_new))]
titanic$age_new = titanic[,.(age_new = ifelse(age>=18 & age<60, "adults",  age_new))]
titanic$age_new = titanic[,.(age_new = ifelse(age>=60, "over 60",  age_new))]


# Bar chart by gender

ggplot(titanic, aes(x = sex, fill = factor(survived))) + geom_bar(width=.5) +
  # scale_fill_manual(breaks = c("0", "1"), values=c("black", "green")) +
  labs(title = "Stacked Barchart", subtitle = "Gender Comparison",x = "Gender", y = "Passengers") +
  scale_x_discrete(labels=c("female" = "Female", "male" = "Male")) +
  scale_fill_manual("", breaks=c("0", "1"),values=c("deepskyblue4","turquoise1"), labels=c("Passed Away", "Survived")) #+
  # coord_flip()

ggplot(titanic, aes(x = pclass, fill = factor(survived))) + geom_bar(pos = "dodge") +
  # scale_fill_manual(breaks = c("0", "1"), values=c("black", "green")) +
  labs(title = "Stacked Barchart", subtitle = "Class Comparison",x = "Class", y = "Passengers") +
  # scale_x_discrete(labels=c("female" = "Female", "male" = "Male")) +
  scale_fill_manual("", breaks=c("0", "1"),values=c("deepskyblue4","turquoise1"), labels=c("Passed Away", "Survived")) +
  facet_grid(~ sex )


# change tick labels
# # Solution 1
# p + scale_x_discrete(breaks=c("0.5","1","2"),
#                      labels=c("Dose 0.5", "Dose 1", "Dose 2"))
# # Solution 2 : same plot as solution 1
# p + scale_x_discrete(labels=c("0.5" = "Dose 0.5", "1" = "Dose 1","2" = "Dose 2"))


# ggplot(titanic, aes(x = sex, fill = factor(survived))) + geom_bar() +
#   scale_fill_manual(breaks = c("0", "1"), values=c("black", "green")) +
#   labs(title = "Stacked Barchart", subtitle = "Gender Comparison",x = "Gender", y = "Passengers", fill = "") +
#   scale_fill_discrete("Survival", breaks=c("0", "1"), labels=c("dead", "alive"))

# ggplot(titanic, aes(x = sex, fill = factor(survived))) + geom_bar() +
#   # scale_fill_manual(breaks = c("0", "1"), values=c("black", "green")) +
#   labs(title = "Stacked Barchart", subtitle = "Gender Comparison",x = "Gender", y = "Passengers") +
#   scale_fill_manual("Survival", breaks=c("0", "1"),values=c("mediumorchid3", "cyan3"), labels=c("dead", "alive"))



ggplot(titanic[cabin != ""], aes(x = pclass, fill = factor(survived))) + geom_bar(pos = "dodge")+
labs(title = "Grouped Barchart", subtitle = "Cabin Holders Class Comparison",x = "Class", y = "Passengers") +
  # scale_x_discrete(labels=c("female" = "Female", "male" = "Male")) +
  scale_fill_manual("", breaks=c("0", "1"),values=c("deepskyblue4","turquoise1"), labels=c("Passed Away", "Survived"))







ggplot(titanic, aes(x = survived, fill = factor(parch))) + geom_bar() +scale_fill_brewer(palette = "Set2")#scale_fill_gradientn(colours = rainbow(5))
  # scale_fill_manual(breaks = c("0", "1"), values=c("black", "green")) +
  # labs(title = "Stacked Barchart", subtitle = "Gender Comparison",x = "Gender", y = "Passengers") +
  # scale_x_discrete(labels=c("female" = "Female", "male" = "Male")) +
  # scale_fill_manual("", breaks=c("0", "1"),values=c("deepskyblue4","turquoise1"), labels=c("Passed Away", "Survived"))











# Pie Chart
ggplot(titanic, aes(x="", fill=factor(survived))) + geom_bar(pos = "fill" , width = 1) +  # an bgalw to pos='fill' ginetai count
  theme(axis.line = element_blank(),plot.title = element_text(hjust=0.5)) +  
  labs(fill="Survival", x = NULL, y = NULL, title = "Pie chart")  +
  coord_polar(theta = "y", start = 0) +
  # scale_fill_manual( values = c("olivedrab","olivedrab1"), labels = c("dead", "alive")) +
  # scale_fill_manual( values = c("orchid4","orchid1"), labels = c("dead", "alive")) +
  scale_fill_manual( values = c("deepskyblue4","turquoise1"), labels = c("dead", "alive")) +
  # scale_fill_manual(brakes=c("0","1"), values = c("pink","cyan"), labels = c("dead", "alive")) +
  facet_grid(~ sex ) +
  theme(
    axis.text.x = element_blank(), #remove values
    axis.text.y = element_blank(),
    axis.ticks = element_blank())  # remove ticks 
    


ggplot(titanic[survived== 1], aes(x="", fill=factor(parch))) + geom_bar(pos = "fill" , width = 1) +  # an bgalw to pos='fill' ginetai count
  theme(axis.line = element_blank(),plot.title = element_text(hjust=0.5)) +  
  labs(fill="Parents / Children", x = NULL, y = NULL, title = "Pie chart of Survivors")  +
  coord_polar(theta = "y", start = 0) +
  scale_fill_brewer(palette = "Set2")+
  # scale_fill_manual( values = c("olivedrab","olivedrab1"), labels = c("dead", "alive")) +
  # scale_fill_manual( values = c("orchid4","orchid1"), labels = c("dead", "alive")) +
  # scale_fill_manual( values = c("deepskyblue4","turquoise1"), labels = c("dead", "alive")) +
  # scale_fill_manual(brakes=c("0","1"), values = c("pink","cyan"), labels = c("dead", "alive")) +
  facet_grid(~ sex ) +
  theme(
    axis.text.x = element_blank(), #remove values
    axis.text.y = element_blank(),
    axis.ticks = element_blank())  # remove ticks 


ggplot(titanic[survived== 0], aes(x="", fill=factor(parch_new))) + geom_bar(pos = "fill" , width = 1) +  # an bgalw to pos='fill' ginetai count
  theme(axis.line = element_blank(),plot.title = element_text(hjust=0.5)) +  
  labs(fill="Parents / Children", x = NULL, y = NULL, title = "Pie chart of Deceased")  +
  coord_polar(theta = "y", start = 0) +
  scale_fill_brewer(palette = "Set2")+
  # scale_fill_manual( values = c("olivedrab","olivedrab1"), labels = c("dead", "alive")) +
  # scale_fill_manual( values = c("orchid4","orchid1"), labels = c("dead", "alive")) +
  # scale_fill_manual( values = c("deepskyblue4","turquoise1"), labels = c("dead", "alive")) +
  # scale_fill_manual(brakes=c("0","1"), values = c("pink","cyan"), labels = c("dead", "alive")) +
  facet_grid(~ sex ) +
  theme(
    axis.text.x = element_blank(), #remove values
    axis.text.y = element_blank(),
    axis.ticks = element_blank())  # remove ticks 




titanic_noageNA = na.omit(titanic, cols = "age_new")
ggplot(titanic_noageNA[survived== 1], aes(x="", fill=factor(age_new))) + geom_bar(pos = "fill" , width = 1) +  # an bgalw to pos='fill' ginetai count
  theme(axis.line = element_blank(),plot.title = element_text(hjust=0.5)) +  
  labs(fill="Age", x = NULL, y = NULL, title = "Pie chart of Survivors")  +
  coord_polar(theta = "y", start = 0) +
  scale_fill_brewer(palette = "Set2")+
  # scale_fill_manual( values = c("olivedrab","olivedrab1"), labels = c("dead", "alive")) +
  # scale_fill_manual( values = c("orchid4","orchid1"), labels = c("dead", "alive")) +
  # scale_fill_manual( values = c("deepskyblue4","turquoise1"), labels = c("dead", "alive")) +
  # scale_fill_manual(brakes=c("0","1"), values = c("pink","cyan"), labels = c("dead", "alive")) +
  facet_grid(~ sex ) +
  theme(
    axis.text.x = element_blank(), #remove values
    axis.text.y = element_blank(),
    axis.ticks = element_blank())  # remove ticks 



# Grouped Box plot
titanic_noagefareNA = na.omit(titanic_noageNA, cols = "fare")

titanic_noageNA$pclass[titanic_noageNA$pclass==1] <- "Class 1"
titanic_noageNA$pclass[titanic_noageNA$pclass==2] <- "Class 2"
titanic_noageNA$pclass[titanic_noageNA$pclass==3] <- "Class 3"


ggplot(titanic_noagefareNA[age_new!="infants"], aes(age_new,fare)) + geom_boxplot(varwidth=T, aes(fill=factor(survived))) +
  coord_flip() +
scale_fill_manual("", breaks=c("0", "1"),values=c("deepskyblue4","turquoise1"), labels=c("Passed Away", "Survived"))+
labs(title="Box plot",
       subtitle="Ticket Fare grouped by Age",
       caption="Source: ???",
       x="Age",
       y="Ticket Fare")



g <-ggplot(titanic_noagefareNA[survived==1], aes(age, parch)) +
  geom_count() +
  labs(title="Survivors Counts Plot",
       subtitle="Parents / Children on Board vs Age",
       caption="Source: ???",
       x="Age",
       y="Parents / Children")
ggMarginal(g , type = "histogram", fill="turquoise1")

g <-ggplot(titanic_noagefareNA[survived==0], aes(age, parch)) +
  geom_count() +
  labs(title="Deceased Counts Plot",
       subtitle="Parents / Children on Board vs Age",
       caption="Source: ???",
       x="Age",
       y="Parents / Children")
ggMarginal(g , type = "histogram", fill="deepskyblue4")










# age_new_col = titanic[,.(age_new =age)]
# titanic = cbind(titanic, sibsp_new_col)

class1_col = titanic_noagefareNA[,.(pclass1 =ifelse(pclass==1,1,0))]
class2_col = titanic_noagefareNA[,.(pclass2 =ifelse(pclass==2,1,0))]
class3_col = titanic_noagefareNA[,.(pclass3 =ifelse(pclass==3,1,0))]

male_col = titanic_noagefareNA[,.(male =ifelse(sex=="male",1,0))]
female_col = titanic_noagefareNA[,.(female =ifelse(sex=="female",1,0))]

titanic2 = titanic_noagefareNA[,.(survived)]

titanic2 = cbind(titanic2, class1_col)
titanic2 = cbind(titanic2, class2_col)
titanic2 = cbind(titanic2, class3_col)
titanic2 = cbind(titanic2, male_col)
titanic2 = cbind(titanic2, female_col)

titanic2 = cbind(titanic2, titanic_noagefareNA[,.(age )])
titanic2 = cbind(titanic2, titanic_noagefareNA[,.(fare)])
titanic2 = cbind(titanic2, titanic_noagefareNA[,.(sibsp )])
titanic2 = cbind(titanic2, titanic_noagefareNA[,.(parch )])



# Gia na afairesoume tis seires opou to fare == 0 !!!!!!!!!!!!
# titanic3 = titanic2
# titanic3 = titanic3[-row(titanic3)[fare == 0],]



# data(titanic2)
corr <-round(cor(titanic2), 1)
# corr <-cor(titanic2)
# Plot
ggcorrplot(corr, type = "lower", hc.order = FALSE,
           lab = TRUE,
           lab_size = 4,
           method="square",
           colors = c("darkviolet", "white", "forestgreen"),
           outline.color = "black",
           title="Correlogram of Titanic",
           ggtheme=theme_bw)





#  diverging bars

titanic4 = titanic_noagefareNA

titanic4$pclass[titanic4$pclass==1] <- "Class 1"
titanic4$pclass[titanic4$pclass==2] <- "Class 2"
titanic4$pclass[titanic4$pclass==3] <- "Class 3"
category_col = data.frame(category =  paste(titanic4$pclass, titanic4$sex,sep = " & "))
# category_col = data.frame(category =  paste(titanic4$pclass, titanic4$sex,titanic4$age_new ,sep = " & "))








# Survived =titanic4$survived
# 
# titanic4a = cbind(Survived,category_col)
# 
# theme_set(theme_bw())
# # Data Prep
# # data("mtcars") # load data
# # mtcars$`car name` <-rownames(mtcars) # create new column for car names
# 
# titanic4a$Survived_z <-round((titanic4a$Survived -mean(titanic4a$Survived))/sd(titanic4a$Survived), 2)
# # compute normalized mpg
# titanic4a$Survived_type <-ifelse(titanic4a$Survived_z < 0, "below", "above") # above / below avg flag
# # mtcars <-mtcars[order(mtcars$mpg_z), ] # sort
# titanic4a <-titanic4a[order(titanic4a$Survived_z), ] # sort
# # mtcars$`car name` <-factor(mtcars$`car name`, levels = mtcars$`car name`) # convert to factor to retain sorted order in plot.
# titanic4a$`category name` <-factor(titanic4a$category, levels = sort(unique(titanic4a$category)))
# # Diverging Barcharts
# ggplot(titanic4a, aes(x=`category name`, y=Survived_z, label=Survived_z)) +
#   geom_bar(stat='identity', aes(fill=Survived_type), width=.5) +
#   scale_fill_manual(name="Mileage",
#                     labels = c("Above Average", "Below Average"), values = c("above"="turquoise1", "below"="deepskyblue4")) +
#   labs(subtitle="Normalised mileage from 'mtcars'",
#        title= "Diverging Bars") +
#   coord_flip()





titanic5 = titanic4

titanic5 = titanic4[, .N, keyby = .(survived,pclass,sex )][survived==1]

theme_set(theme_bw())

titanic5$"N_z" <-round((titanic5$N -mean(titanic5$N))/sd(titanic5$N), 2)
titanic5$Category <- paste(titanic5$pclass, titanic5$sex, sep = " & ")
titanic5 = titanic5[,.(Category,N_z)]

titanic5$N_type <-ifelse(titanic5$N_z < 0, "below", "above") # above / below avg flag

titanic5 <-titanic5[order(titanic5$N_z), ] # sort

titanic5$Category <-factor(titanic5$Category, levels = titanic5$Category)

# Diverging Barcharts
ggplot(titanic5, aes(x=Category, y=N_z, label=N_z)) +
  geom_bar(stat='identity', aes(fill=N_type), width=.5) +
  scale_fill_manual(name="Survivors",
                    labels = c("Above Average", "Below Average"), values = c("above"="coral2", "below"="springgreen4")) +
  labs(subtitle="Normalised survivors from Titanic",
       title= "Diverging Bars") +
  coord_flip()






titanic5 = titanic4

titanic5 = titanic4[, .N, keyby = .(survived,pclass,sex )][survived==0]

theme_set(theme_bw())

titanic5$"N_z" <-round((titanic5$N -mean(titanic5$N))/sd(titanic5$N), 2)
titanic5$Category <- paste(titanic5$pclass, titanic5$sex, sep = " & ")
titanic5 = titanic5[,.(Category,N_z)]

titanic5$N_type <-ifelse(titanic5$N_z < 0, "below", "above") # above / below avg flag

titanic5 <-titanic5[order(titanic5$N_z), ] # sort

titanic5$Category <-factor(titanic5$Category, levels = titanic5$Category)

# Diverging Barcharts
ggplot(titanic5, aes(x=Category, y=N_z, label=N_z)) +
  geom_bar(stat='identity', aes(fill=N_type), width=.5) +
  scale_fill_manual(name="Deceased",
                    labels = c("Above Average", "Below Average"), values = c("above"="coral2", "below"="springgreen4")) +
  labs(subtitle="Normalised deceased from Titanic",
       title= "Diverging Bars") +
  coord_flip()











# density

# theme_set(theme_classic())
# Plot
g <-ggplot(titanic4, aes(age))
g + geom_density(aes(fill=factor(survived)), alpha=0.8) +
  labs(title="Density plot",
       subtitle="Age of Passengers Grouped by Survival and Class",
       caption="Source: Titanic",
       x="Age",
       y="Density",
       fill="") +
  facet_grid(~ pclass ) + 
  scale_fill_manual("", breaks=c("0", "1"),values=c("deepskyblue4","turquoise1"), labels=c("Passed Away", "Survived"))































