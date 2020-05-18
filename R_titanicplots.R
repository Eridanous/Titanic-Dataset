# clear all
rm(list=ls())
# getwd()
setwd("C:/Users/Eridanus/Desktop/R assignment/")


library(data.table)
library(ggplot2)
library(ggExtra)
library(ggcorrplot)



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



# Bar chart by gender


# 1 Fig_Bar_gender_comp
ggplot(titanic, aes(x = sex, fill = factor(survived))) + geom_bar(width=.35) +
  labs(title = "Stacked Barchart", subtitle = "Gender Comparison",x = "Gender", y = "Passengers") +
  scale_x_discrete(labels=c("female" = "Female", "male" = "Male")) +
  scale_fill_manual("", breaks=c("0", "1"),values=c("deepskyblue4","turquoise1"), labels=c("Passed Away", "Survived")) 



# 2 Fig_Bar_class_comp
ggplot(titanic, aes(x = pclass, fill = factor(survived))) + geom_bar(pos = "dodge")+
  labs(title = "Grouped Barchart", subtitle = "Ticket Class Comparison",x = "Class", y = "Passengers") +
  scale_fill_manual("", breaks=c("0", "1"),values=c("deepskyblue4","turquoise1"), labels=c("Passed Away", "Survived"))


# 3 Fig_Bar_genderclass_comp
ggplot(titanic, aes(x = pclass, fill = factor(survived))) + geom_bar(pos = "dodge") +
  labs(title = "Stacked Barchart", subtitle = "Class & Gender Comparison",x = "Class", y = "Passengers") +
  scale_fill_manual("", breaks=c("0", "1"),values=c("deepskyblue4","turquoise1"), labels=c("Passed Away", "Survived")) +
  facet_grid(~ sex )










# Pie0

parch_new_col = data.table( parch_new =  titanic[,ifelse(parch<3,parch,"more than 2") ] )
titanic = cbind(titanic, parch_new_col)

sibsp_new_col = data.table( sibsp_new =  titanic[,ifelse(parch<4,parch,">3") ] )
titanic = cbind(titanic, sibsp_new_col)

age_new_col = titanic[,.(age_new =age)]
titanic = cbind(titanic, age_new_col)

titanic$age_new = titanic[,.(age_new = ifelse(age<1, "infants", age_new))]
titanic$age_new = titanic[,.(age_new = ifelse(age>=1 & age<18, "children",  age_new))]
titanic$age_new = titanic[,.(age_new = ifelse(age>=18 & age<60, "adults",  age_new))]
titanic$age_new = titanic[,.(age_new = ifelse(age>=60, "over 60",  age_new))]






# Pie1 # Fig_pieParchS # Fig_pieParchD =================================================================================

# Pie chart of Survivors
ggplot(titanic[survived== 1], aes(x="", fill=factor(parch_new))) + geom_bar(pos = "fill" , width = 1) + 
  theme(axis.line = element_blank(),plot.title = element_text(hjust=0.5)) +  
  labs(fill="Parents / Children", x = NULL, y = NULL, title = "Pie Chart of Survivors")  +
  coord_polar(theta = "y", start = 0) +
  scale_fill_brewer(palette = "Set2") +
  facet_grid(~ sex ) +
  theme(axis.text.x = element_blank()) #remove values
   
# Pie chart of Deceased

ggplot(titanic[survived== 0], aes(x="", fill=factor(parch_new))) + geom_bar(pos = "fill" , width = 1) +  
  theme(axis.line = element_blank(),plot.title = element_text(hjust=0.5)) +  
  labs(fill="Parents / Children", x = NULL, y = NULL, title = "Pie Chart of Deceased")  +
  coord_polar(theta = "y", start = 0) +
  scale_fill_brewer(palette = "Set2") +
  facet_grid(~ sex ) +
  theme(axis.text.x = element_blank()) #remove values
    
# =================================================================================



# Pie2 Fig_pieAgeS # Fig_pieAgeD =================================================================================

titanic_noageNA = na.omit(titanic, cols = "age_new")

titanic_noageNA$pclass[titanic_noageNA$pclass==1] <- "Class 1"
titanic_noageNA$pclass[titanic_noageNA$pclass==2] <- "Class 2"
titanic_noageNA$pclass[titanic_noageNA$pclass==3] <- "Class 3"

# Pie of Survivors

ggplot(titanic_noageNA[survived== 1], aes(x="", fill=factor(age_new))) + geom_bar(pos = "fill" , width = 1) + 
  theme(axis.line = element_blank(),plot.title = element_text(hjust=0.5)) +  
  labs(fill="Age", x = NULL, y = NULL, title = "Pie chart of Survivors")  +
  coord_polar(theta = "y", start = 0) +
  scale_fill_brewer(palette = "Accent")+
  facet_grid(~ pclass ) +
  theme(axis.text.x = element_blank()) #remove values
    

# Pie of Deceased

ggplot(titanic_noageNA[survived== 0], aes(x="", fill=factor(age_new))) + geom_bar(pos = "fill" , width = 1) + 
  theme(axis.line = element_blank(),plot.title = element_text(hjust=0.5)) +  
  labs(fill="Age", x = NULL, y = NULL, title = "Pie chart of Deceased")  +
  coord_polar(theta = "y", start = 0) +
  scale_fill_brewer(palette = "Accent")+
  facet_grid(~ pclass ) +
  theme(axis.text.x = element_blank()) #remove values
    
#  =================================================================================




# Grouped Box plot  # Fig_box


# Box plot
titanic_noagefareNA = na.omit(titanic_noageNA, cols = "fare")

ggplot(titanic_noagefareNA[age_new!="infants"], aes(age_new,fare)) + geom_boxplot(varwidth=T, aes(fill=factor(survived))) +
  coord_flip() +
  scale_fill_manual("", breaks=c("0", "1"),values=c("deepskyblue4","turquoise1"), labels=c("Passed Away", "Survived"))+
  labs(title="Box plot",
       subtitle="Ticket Fare grouped by Age",
       caption="Source: Titanic",
       x="Age",
       y="Ticket Fare")


# Grouped Box plot  # Fig_box2


# Box plot
titanic_noportfareNA = na.omit(titanic, cols = "fare")
titanic_noportfareNA$embarked = ifelse(titanic_noportfareNA$embarked == "", NA, titanic_noportfareNA$embarked)
titanic_noportfareNA = na.omit(titanic_noportfareNA, cols = "embarked")


ggplot(titanic_noportfareNA, aes(embarked,fare)) + geom_boxplot(varwidth=T, aes(fill=factor(survived))) +
  coord_flip() +
  scale_fill_manual("", breaks=c("0", "1"),values=c("deepskyblue4","turquoise1"), labels=c("Passed Away", "Survived"))+
  labs(title="Box plot",
       subtitle="Ticket Fare grouped by Port of Embarkation",
       caption="Source: Titanic",
       x="Port of Embarkation",
       y="Ticket Fare")









# Marginal MargS MargD

# Survivors
titanic_noagefareNA = na.omit(titanic_noageNA, cols = "fare")
g <-ggplot(titanic_noagefareNA[survived==1], aes(age, parch)) +
  geom_count() +
  labs(title="Survivors Counts Plot",
       subtitle="Parents / Children on Board vs Age",
       caption="Source: Titanic",
       x="Age",
       y="Parents / Children")

ggMarginal(g , type = "histogram", fill="turquoise1")

# Deceased
g <-ggplot(titanic_noagefareNA[survived==0], aes(age, parch)) +
  geom_count() +
  labs(title="Deceased Counts Plot",
       subtitle="Parents / Children on Board vs Age",
       caption="Source: Titanic",
       x="Age",
       y="Parents / Children")
ggMarginal(g , type = "histogram", fill="deepskyblue4")










# Correlation Fig_cor ===============================================================================

class1_col = titanic_noagefareNA[,.(pclass1 =ifelse(pclass=="Class 1",1,0))]
class2_col = titanic_noagefareNA[,.(pclass2 =ifelse(pclass=="Class 2",1,0))]
class3_col = titanic_noagefareNA[,.(pclass3 =ifelse(pclass=="Class 3",1,0))]

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


# Plot
corr <-round(cor(titanic2), 1)

ggcorrplot(corr, type = "lower", hc.order = FALSE,
           lab = TRUE,
           lab_size = 4,
           method="square",
           colors = c("darkviolet", "white", "forestgreen"),
           outline.color = "black",
           title="Correlogram of Titanic",
           ggtheme=theme_bw)

# ===============================================================================







# density Fig_density

titanic4 = titanic_noagefareNA

# Plot
g <-ggplot(titanic4, aes(age))
g + geom_density(aes(fill=factor(survived)), alpha=0.8) +
  labs(title="Density plot",
       subtitle="Age of Passengers Grouped by Class",
       caption="Source: Titanic",
       x="Age",
       y="Density",
       fill="") +
  facet_grid(~ pclass ) + 
  scale_fill_manual("", breaks=c("0", "1"),values=c("deepskyblue4","turquoise1"), labels=c("Passed Away", "Survived"))















# Diverge ===============================================================================================

# Survivors
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



# Deceased
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

# ===============================================================================================================






































