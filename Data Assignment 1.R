#Obtain and import the dataset
# import downloaded data set Coffee quality
library(readr)
Coffee_Qlty <- read_csv("Desktop/Assignment/Coffee_Qlty.csv")
View(Coffee_Qlty)

#Basic descriptive statistics

summary(Coffee_Qlty)
#get frequency for categorical variables
table(Coffee_Qlty$Species)
table(Coffee_Qlty$Continent.of.Origin)
table(Coffee_Qlty$Color)
table(Coffee_Qlty$Processing.Method)

#Standard deviations for continuous variables

sd(Coffee_Qlty$Aroma)
sd(Coffee_Qlty$Aftertaste)
sd(Coffee_Qlty$Flavor)
sd(Coffee_Qlty$Acidity)
sd(Coffee_Qlty$Body)
sd(Coffee_Qlty$Balance)
sd(Coffee_Qlty$Uniformity)
sd(Coffee_Qlty$Clean.Cup)
sd(Coffee_Qlty$Sweetness)
sd(Coffee_Qlty$Moisture)
sd(Coffee_Qlty$Quakers)
sd(Coffee_Qlty$Category.One.Defects)
sd(Coffee_Qlty$Category.Two.Defects)

#Clean and process the data - removal of outliers from the variable aroma was is shown

#Generate appropriate plots (e.g., bar plot, line plot) to visualise the data
install.packages("ggplot2") #to install packages in R studio
install.packages("ggpubr")
library(ggplot2) #This retrieves the packages so they are ready to use
library(ggpubr)

#Barplots to visualize level of aroma for species and continent of origin
barplot <- ggplot(Coffee_Qlty, aes(x=Species, y=Aroma)) + 
  geom_bar(stat = "identity") 
barplot

#This is before the outlier is removed.

barplot1 <- ggplot(data = subset(Coffee_Qlty, !is.na(Continent.of.Origin)), aes(x=Continent.of.Origin, y=Aroma, fill = Species)) + 
  geom_bar(stat = "identity", position="dodge") +
  labs(y= "Aroma", x = "Continent of Origin")
barplot1 
#Before removing the outlier we didnt add it in as the cleanded data looked better.

#scatterplots to assess difference of continuous variables (check technical report)
scatterplot <- ggplot(Coffee_Qlty, aes(x=Aftertaste, y=Aroma)) +
  geom_point() 
scatterplot
#To print the plot

scatterplot1 <- ggplot(Coffee_Qlty, aes(x=Sweetness, y=Aroma)) +
  geom_point() 
scatterplot1

scatterplot2 <- ggplot(Coffee_Qlty, aes(x=Body, y=Aroma)) +
  geom_point() 
scatterplot2

scatterplot3 <- ggplot(Coffee_Qlty, aes(x=Acidity, y=Aroma)) +
  geom_point() 
scatterplot3

#Some outliers tests are conducted again. To remove any outliers:

Q <- quantile(Coffee_Qlty$Aroma, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(Coffee_Qlty$Aroma)
up <-  Q[2]+1.5*iqr   
low<- Q[1]-1.5*iqr
eliminated<- subset(Coffee_Qlty, Coffee_Qlty$Aroma > (Q[1] - 1.5*iqr) & Coffee_Qlty$Aroma < (Q[2]+1.5*iqr))

#barplots

barplot <- ggplot(eliminated, aes(x=Species, y=Aroma)) + 
  geom_bar(stat = "identity") 
barplot 
#Barplot after removal of the outliers

bar <- ggplot(data = subset(eliminated, !is.na(Continent.of.Origin)), aes(x=Continent.of.Origin, y=Aroma)) + 
  geom_bar(stat = "identity") +
  labs(y= "Aroma", x = "Continent of Origin")
bar 
#Bar plot figure called After removal of outlier


barplot1 <- ggplot(data = subset(eliminated, !is.na(Continent.of.Origin)), aes(x=Continent.of.Origin, y=Aroma, fill = Species)) + 
  geom_bar(stat = "identity", position="dodge") +
  labs(y= "Aroma", x = "Continent of Origin")
barplot1 
#Bar plot

#scatterplots

scatter <- ggplot(eliminated, aes(x=Aftertaste, y=Aroma)) +
  geom_point() + 
  stat_cor(method = "pearson")
scatter  

scatter1 <- ggplot(eliminated, aes(x=Sweetness, y=Aroma)) +
  geom_point() + 
  stat_cor(method = "pearson")
scatter1

scatter2 <- ggplot(eliminated, aes(x=Body, y=Aroma)) +
  geom_point() + 
  stat_cor(method = "pearson")
scatter2

scatter3 <- ggplot(eliminated, aes(x=Acidity, y=Aroma)) +
  geom_point() + 
  stat_cor(method = "pearson")
scatter3

#Interpret the descriptive statistics and graphs to summarize the main features 
#and patterns of the data (e.g., trends, variability, outliers, and relationships between variables).
#See report


#To investigate for any significant difference between the mean score of Aroma and Species group.
#The mean Aroma score is higher in the Robusta species than in the Arabica species, with a p-value = 0.0212. see report


lmAroma = lm(Aroma ~ Aftertaste + Acidity + Sweetness + Body, data = Coffee_Qlty)
summary(lmAroma) 
#linear regression model before removal of outliers

lmAroma = lm(Aroma ~ Aftertaste + Acidity + Sweetness + Body, data = eliminated)
summary(lmAroma) 
#Multiple linear regression model after removal of outliers. This looks at Aroma as a function. 

#Discuss any limitations of our work as well as unresolved questions or problems 
#(i.e., technical issues, limitations of the dataset, etc.) seen from our interpretation. 
#See report mm







