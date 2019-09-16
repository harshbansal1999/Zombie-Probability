#Data PreProcessing
zombies=read.csv('zombies.csv')
summary(zombies)       #Summary

water.person=zombies$water/zombies$household  #Water Per Person
summary(water.person)

library(ggplot2)
install.packages('gridExtra')
library(gridExtra)

#AgeZombies Graph
ageZombies <- ggplot(data = zombies, aes(x = age, fill = zombie)) +
  geom_density(alpha = 0.3) +  
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

#WaterPerPersonZombies Graph
waterPersonZom <- ggplot(data = zombies, aes(x = water.person, fill = zombie)) +
  geom_density(alpha = 0.3) +  
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

#Plotting Both Graphs Together
grid.arrange(ageZombies, waterPersonZom, ncol = 2)

#Subset of the zombies data with only factors
zombies.factors <- zombies[ , sapply(zombies, is.factor)]

#Percentage Distribution of each column
perc.zombies <- lapply(zombies.factors, 
                       function(x){ 
                         return(prop.table(table(x, zombies.factors$zombie),
                                           margin = 1))})
perc.zombies

#Changing Na To No Clothing
levels(zombies$clothing) <- c(levels(zombies$clothing), "No clothing")
zombies$clothing[is.na(zombies$clothing)] <- "No clothing"

#Changing Na To No Documents
levels(zombies$documents) <- c(levels(zombies$documents), "No documents")
zombies$documents[is.na(zombies$documents)] <- "No documents"

summary(zombies)

#Updating The zombies.factors
zombies.factors <- zombies[ , sapply(zombies, is.factor)]

# Chi-squared for factors
chi.zombies <- lapply(zombies.factors, 
                      function(x){
                        return(chisq.test(x, zombies$zombie))})

# T-tests for numeric
ttest.age <- t.test(zombies$age ~ zombies$zombie)
ttest.water <- t.test(zombies$water ~ zombies$zombie)    

chi.zombies 
ttest.age 
ttest.water

#Training the Classification Model- Logistic Regression
zombie.model <- glm(zombie ~ age + water.person + food + rurality + medication  + sanitation,
                    data = zombies, family = binomial(logit))

# Model significance, fit, and odds ratios with 95% CI
#install.packages('odds.n.ends')
library(odds.n.ends)
zombie.model.fit <- odds.n.ends(zombie.model)
zombie.model.fit

#GVIF
#install.packages('car')
library(car)
vif(zombie.model)

zombies$logitZombie <- log(zombie.model$fitted.values/(1-zombie.model$fitted.values))

ageLinearity <- ggplot(data = zombies, aes(x = age, y = logitZombie))+
  geom_point(color = "gray") +
  geom_smooth(method = "loess", se = FALSE, color = "orange") + 
  geom_smooth(method = "lm", se = FALSE, color = "gray") + 
  theme_bw() 

waterPersonLin <- ggplot(data = zombies, aes(x = water, y = logitZombie))+
  geom_point(color = "gray") +
  geom_smooth(method = 'loess', se = FALSE, color = "orange") + 
  geom_smooth(method = 'lm', se = FALSE, color = "gray") + 
  theme_bw() 

grid.arrange(ageLinearity, waterPersonLin, ncol = 2)


#Data For Testing - Change the year as requirement
newdata <- data.frame(age = c(71, 20), 
                      water.person = c(5,8),
                      food = c("Food", "No food"),
                      rurality = c("Suburban", "Urban"),
                      medication = c("Medication", "No medication"),
                      sanitation = c("Sanitation", "No sanitation"))

#Prediction with Probability of becoming the zombie
predictions <- predict(zombie.model, newdata, type = "response")

predictions
