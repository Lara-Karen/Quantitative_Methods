#######################  PACKAGES  #########################

install.packages("here")
library(here)

install.packages("ggplot2")
library(ggplot2)

install.packages("janitor")
library(janitor)

install.packages("plotly")
library(plotly)

install.packages("rlang")
library(rlang)
library(factoextra)
library(dplyr)
library(tidyverse)

install.packages("ggbetweenstats")
library(ggbetweenstats)

library(dplyr) 
library(ggplot2)

install.packages("sprintf"")
library(sprintf)

install.packages("ggpmisc")
library(ggpmisc)

################### IMPORTING CSV ################

obesity_data <- read.csv("obesity_data.csv")
obesity_data <- obesity_data %>%
  clean_names()

#With Janitor Package, standardizes names
df <- df %>%
  clean_names()

################## BOX PLOT ####################

#Box Plot by Region population~average cases 
ggplot(df, aes(x=mean_population, y=average_cases_for_3_years, color = region, group = region)) + 
  geom_boxplot(outlier.shape = 4, outlier.size = 3) +
  stat_boxplot(orientation= TRUE) +
  xlab("Mean Population for years 2008, 2013 and 2018") +
  ylab("Mean Cases of 'Condition X' for years 2008, 2013 and 2018")

#Box Plot for budget per person
ggplot(df, aes(budget_per_person)) + 
  geom_boxplot(outlier.shape = 4, outlier.size = 3, color = "darkslateblue") +
  stat_boxplot(orientation= TRUE)+
  xlab("Budget Per Person in £ per year")


check_outliers(df$mean_population)

  boxplot.stats(df$mean_population)$out
  boxplot.stats(df$total_budget)$out


#box plot not grouped by region 
ggplot(df, aes(x=mean_population, y=average_cases_for_3_years))+
  geom_boxplot(outlier.shape = 4, outlier.size = 3) +
  stat_boxplot(orientation= TRUE)

################# HISTOGRAM ##################
# Histogram (one variable)
qplot(df$average_cases_for_3_years,
      geom="histogram",
      binwidth = 5,  
      main = "Histogram for Average Cases", 
      xlab = "Cases",  
      fill=I("blue"), 
      col=I("red"))

################# CREATING DATAFRAMES ##################

# Creating a new dataframe with only what is needed for the regression
regression_data <- diff_years %>%
  select(contains("average"),
         contains("mean"),
         contains("budget_per"),
         contains("diff_of_cases"),
         contains("local_authority_area"))

# Creating a new dataframe with the Sectors for regression
regression_sectors <- diff_years %>%
  select(contains("average"),
         contains("mean"),
         contains("budget_per"),
         contains("diff_of_cases"),
         contains("local_authority_area"),
         contains("clean"),
         contains("school"),
         contains("health"),
         contains("media"),
         contains("counselling"))

#creating a new df dataset with the diff in cases
df_df <- df%>%
  mutate(diff_of_cases = ((x2018_cases_ratio - x2008_cases_ratio)/(x2008_cases_ratio)*100))

regression_sectors <- regression_sectors%>%
  slice(-c(115, 27, 9)) 

# Creating a new dataframe with only what is needed for the regression REMOVED OUTLIERS
regression_data_no_outliers <- regression_data%>%
  slice(-c(27, 50, 9, 115)) 

# Creating a new dataframe REMOVED only city of Scisily, city of london, and Rutland
regression_data_no_outliers_1 <- regression_data%>%
  slice(-c(115, 27, 9)) 

# Creating a new dataframe REMOVED only city of Scisily, city of london
regression_data_no_outliers_2 <- regression_data%>%
  slice(-c(115, 27)) 
  

regression_data = subset(regression_data, select = -c(average_cases_for_3_years, mean_population))

#rename
names(regression_data)[2] <- "budget"
names(regression_data)[1] <- "cases"


# Creating a new dataframe REMOVED only city of Scisily, city of london, and Rutland
regression_for_sectors <- diff_years%>%
  slice(-c(115, 27, 9)) 

#to create subcouselling per person
regression_sectors <- regression_for_sectors%>%
  mutate(sub_counselling_proportion = ((sub_counselling)/(total_budget)))

regression_sectors <- regression_sectors%>%
  mutate(sub_counselling_per_person = ((sub_counselling_proportion)/(mean_population)))

#regression for sub counselling
ggplot(data = regression_sectors, aes(x = sub_counselling_per_person, y = diff_of_cases)) + 
  geom_point(color='purple') +
  geom_smooth(method = "lm", se = FALSE)

#adding a new dataset with difference in years
diff_years <- df%>%
  mutate(diff_of_cases = ((x2018_cases_ratio - x2008_cases_ratio)/(x2008_cases_ratio)*100))

#trying to group????? WHY IS THIS NOT WORKING
difference_of_years <- diff_years %>%
  mutate(diff_of_cases = case_when(diff_of_cases >=0 ~
                                     str_c("obesity rates have increased"),
                                   TRUE ~
                                     str_c("rates have decreased"))%>%
           group_by(diff_of_cases)%>%
           summarise(count=n)

###################### REGRESSION #########################

# Regression Option 1, does not seem right
ggplot(regression_data_no_outliers_1, aes(x = budget_per_person ,y = diff_of_cases)) + 
  stat_smooth(method = "lm", formula = y ~ x, size = 1.5) + 
  geom_point(alpha = 0.3) +
  stat_poly_eq(aes(label = paste(..rr.label..)),
               label.x.npc = "right", label.y.npc = 0.15, formula = formula, 
               parse=TRUE, size = 5) + 
  stat_fit_glance(method = 'lm', method.args = list(formula = formula),
                  geom='text', aes(label=paste0("P-value = ", sprintf("%1.4e", ..p.value..))),
                  label.x.npc = 'right',
                  label.y.npc = 0.4, size = 6)

#adding the regression line Regression Option 2
ggplot(data = regression_data_no_outliers_1, aes(x = budget_per_person, y = diff_of_cases)) + 
  geom_point(color = "firebrick4", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE ,color = "firebrick4")+
  xlab("Budget Per Person in £ per year") +
  ylab("Percentage of Change in Cases from 2008 to 2018")

#adding the regression line Regression with outliers
ggplot(data = regression_data, aes(x = budget_per_person, y = diff_of_cases)) + 
  geom_point(color = "firebrick4", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE ,color = "firebrick4")+
  xlab("Budget Per Person in £ per year") +
  ylab("Percentage of Change in Cases from 2008 to 2018")



###############################   REGRESSION FOR CLEAN AIR   ###################################

# Regression for clean air 
ggplot(data = regression_sectors, aes(x = clean_air_per_person, y = diff_of_cases)) + 
  geom_point(color = "darkseagreen4", alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, stat = "smooth", color = "darkseagreen4", size = .5)+
  xlab("Budget Allocated to Clean Air Per Person in £ per year") +
  ylab("Percentage of Change in Cases from 2008 to 2018")

###########################   REGRESSION FOR CLEAN ENVIRONMENT   ################################

#regression for clean environment
ggplot(data = regression_sectors, aes(x = clean_environment_per_person, y = diff_of_cases)) + 
  geom_point(color = "indianred3", alpha = 0.65) +
  geom_smooth(method = "lm", se = FALSE, stat = "smooth", color = "indianred3", size = .5) +
  xlab("Budget Allocated to Clean Environment Per Person in £ Per Year") +
  ylab("Percentage of Change in Cases from 2008 to 2018")

############################   REGRESSION FOR SCHOOL AWARENESS   ################################

#regression for school awareness
ggplot(data = regression_sectors, aes(x = school_awareness_per_person, y = diff_of_cases)) + 
  geom_point(color='mediumpurple3', alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, stat = "smooth", color = "mediumpurple3", size = .5) +
  xlab("Budget Allocated to School Awareness Per Person in £ Per Year") +
  ylab("Percentage of Change in Cases from 2008 to 2018")

############################   REGRESSION FOR HEALTH TRAINING   ################################

#regression for health training
ggplot(data = regression_sectors, aes(x = health_training_per_person, y = diff_of_cases)) + 
  geom_point(color='lightslateblue', alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, stat = "smooth", color = "lightslateblue", size = .5)+
  xlab("Budget Allocated to Health Training Per Person in £ Per Year") +
  ylab("Percentage of Change in Cases from 2008 to 2018")

############################   REGRESSION FOR MEDIA AWARENESS   ################################

#regression for media awareness
ggplot(data = regression_sectors, aes(x = media_awareness_per_person, y = diff_of_cases)) + 
  geom_point(color='deepskyblue4', alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, stat = "smooth", color = "deepskyblue4", size = .5) +
  xlab("Budget Allocated to Media Awareness Per Person in £ Per Year") +
  ylab("Percentage of Change in Cases from 2008 to 2018")

############################   REGRESSION FOR MEDIA AWARENESS   ################################

#regression for media awareness
ggplot(data = regression_sectors, aes(x = sub_counselling_per_person, y = diff_of_cases)) + 
  geom_point(color='lightseagreen', alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, stat = "smooth", color = "lightseagreen", size = .5) +
  xlab("Budget Allocated to Sub Counselling Per Person in £ Per Year") +
  ylab("Percentage of Change in Cases from 2008 to 2018")


  
# Regression of df with outliers
ggplot(data = regression_data, aes(x = budget, y = cases)) + 
  geom_point(color='purple') +
  geom_smooth(method = "lm", se = FALSE)


regression_data_no_outliers_1 <- regression_data_no_outliers_1 %>%
cor(x = budget_per_person, y = diff_of_casses, use = "everything", method = "pearson")


#TEXPORT DATAFRAME AS CVS
write.csv(regression_sectors,"~/Documents/CASA/Obesity_data_set.csv", row.names = TRUE)
