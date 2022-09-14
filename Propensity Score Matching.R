#------------------------------------------------------------------------------#
#   Propensity Score Matching: MatchIt
#   Titanic: What is the impact of third class cabin on survival rate?
#------------------------------------------------------------------------------#

#set Working Directory
getwd()
setwd("C:\\Users\\medma\\Documents\\DataScience\\data\\titanic")

#load libraries
library(MatchIt)
library(dplyr)
library(ggplot2)
library(cobalt)
library(gridExtra)
library(optmatch)

#dataset
titanic <- read.csv("train.csv")

#view data
head(titanic)
str(titanic)

#create a factor class for Pclass, sex, and embarked
#class cabin = 3, sex = male, and embarked = c (Cherbourg)
titanic$Pclass <- as.factor(ifelse(titanic$Pclass==3,1,0)) 
titanic$Sex <- as.factor(ifelse(titanic$Sex=="male",1,0))
titanic$Embarked <- as.factor(ifelse(titanic$Embarked=="C",1,0))
str(titanic)

#convert other categoricals to factors
titanic$Survived <- as.factor(titanic$Survived)
str(titanic)

#check for the number of missing values for age and sibsp
sum(is.na(titanic$Age)) #177 values missing
sum(is.na(titanic$SibSp)) #0 values missing
sum(is.na(titanic$Parch)) #0 values missing
sum(is.na(titanic$Fare)) #0 values missing
sum(is.na(titanic$Embarked)) #0 values missing

#subset the data
# Y/Outcome = Survived, X/Confounder = Sex, T/Treatment = Pclass (level 3)
t <- subset(titanic, select = c("Survived","Pclass","Sex","SibSp","Parch","Embarked"))
head(t)

#--------------------Significance--------------------#
#check if the confounders are significantly related to treatment (lr model)
cov_test1 <- glm(Pclass ~ Sex + SibSp + Parch + Embarked, data = t, family = "binomial")
#check if the confounders are significantly related to outcome (lr model)
cov_test2 <- glm(Survived ~ Sex + SibSp + Parch + Embarked, data = t, family = "binomial")

summary(cov_test1)
summary(cov_test2)

#only sex and sibsp are significant to both treatment and outcome
#--------------------Propensity Score Estimation--------------------#
#logit model
t <- subset(t, select = c("Survived","Pclass","Sex","SibSp","Embarked"))
#SibSp was not significant to both treatment and outcome, so remove it
t_ps <- glm(Pclass ~ Sex + SibSp + Embarked, data = t, family = "binomial")
summary(t_ps)
#propensity score calculation
prs_df <- data.frame(pr_score = predict(t_ps, type = "response"),
                     Pclass = t_ps$model$Pclass)
head(prs_df)
tail(prs_df)

#plot
labs <- paste("Class Cabin:", c("Third", "First or Second"))
prs_df %>%
  mutate(Pclass = ifelse(Pclass == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~Pclass) +
  xlab("Probability of Third Class Cabin") +
  theme_bw()

#--------------------MatchIt--------------------#
#utilize MatchIt to find pairs of observations that have very similar 
#propensity scores, but that differ in their treatment status
#matchit argumets = exact, replace, m.order, ratio, and caliper

# Note. Certain matching method allow 
# for different types of treatment effect to be
# estimated. There are 3:
# ATT -- Average Treatment of Treated
# ATC -- Average Treatment of Control
# ATE -- Average Treatment Effect

# This determines whether you can interpret the  
# treatment effect for the entire sample (ATE) 
# or just those that recieved treatment only (ATT) 
# or were in the control only (ATC). 

# Some matching methods (e.g., NNM and OM) do not 
# allow for ATE to be estimated but FM does. The 
# estimand argument controls how the weights will 
# be computed for FM. 

# Between ATT and ATC estimad will just adjust 
# the focal group used to be matched on.

#-#-#-#Nearest Neighbor Matching#-#-#-#
mod_match <- matchit(Pclass ~ Sex + SibSp + Embarked,
                     distance = "glm", data = t, method = "nearest",
                     m.order = "largest", replace = F)
#description
mod_match

#summary
#Std. Mean Diff. close to 0 indicates good balance; 1 for Var. Ratio
#0 for Std. Pair Dist. (how close the pairings are to one another)
summary(mod_match)

#create a data frame that contains only the matched observations
df_m <- match.data(mod_match)
dim(df_m)

#note distance, which represents propensity score
summary(df_m)



#-#-#-#Optimal Matching#-#-#-#
#note that "optmatch" library is necessary
mod_match_o <- matchit(Pclass ~ Sex + SibSp + Embarked,
                     distance = "glm", data = t, method = "optimal")
#description
mod_match_o

#summary
summary(mod_match_o)



#-#-#-#Full Matching#-#-#-#
#note that "optmatch" library is necessary
mod_match_f <- matchit(Pclass ~ Sex + SibSp + Embarked,
                       distance = "glm", data = t, method = "full",
                       estimand = "ATE")
#description
mod_match_f

#summary
summary(mod_match_f)

#--------------------Balance Evaluation--------------------#
#visualize co-variate balance of NNM; all of the adjusted differences (minus distance)
#fall within the dotted lines, indicating co-variate balance
#however, the same cannot be said for the ratios
love.plot(bal.tab(mod_match), 
          stat = c("m","v"),
          grid=TRUE,
          threshold=c(m=.25,v=1.25))

#take a look at the weights
head(match.data(mod_match), 15)

#take a look at the max weight and min weight
match.data(mod_match) %>%
  group_by(Pclass) %>%
  summarise(min.weight = min(weights),
            max.weight = max(weights),
            mean.weight = mean(weights),
            sum.weights = sum(weights),
            n=n(),
            max.match.ratio = max.weight/min.weight)

#take a look at the overall distribution of weights
match.data(mod_match) %>%
  group_by(Pclass, weights) %>%
  tally %>%
  group_by(Pclass) %>%
  mutate(weight.ratio = weights/min(weights))

#--------------------Treatment Estimation--------------------#
#propensity score near neighbors weighted regression
mod <- glm(Survived ~ Pclass + Sex + SibSp + Embarked, data = df_m, 
           weights = weights, family = "binomial")

#ATT for Pclass
summary(mod)

#without propensity score
mod2 <- glm(Survived ~ Pclass + Sex + SibSp + Embarked, data = t, family = "binomial")

summary(mod2)

#Note that with propensity scores, there is a slightly higher likelihood that
#the titanic was survived
