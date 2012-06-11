#######################################################################################
#template_secondary_data_analysis.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
#######################################################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky

#link to manuscript: http://goo.gl/FW5tJ

#####################################################################################
#SETTING ENVIRONMENT
#####################################################################################
#remove all objects and then check if they were removed
rm(list = ls())
ls()
#dettach all packages
detach()

#command below will install all packages and is only run once. remove the #if this is the first time you are running the code on RStudio, and then you can add the hash tag again
#lapply(c("ggplot2", "psych", "RCurl", "irr", "car","Hmisc", "gmodels", "DAAG", "gdata"), install.packages, character.only=T)
# install.packages("catspec")
# library(catspec)
library(gdata)
lapply(c("ggplot2", "psych", "RCurl", "irr","Hmisc", "gmodels","qpcR", "car"), library, character.only=T)

#####################################################################################
#IMPORTING DATA
#####################################################################################

#if you are using a file that is local to your computer, then replace path below by path to the data file. command will throw all the data into the templateData object
airwayDehiscence <- read.csv("/Users/rpietro/Google Drive/R/nonpublicdata_publications/AirwayDehiscenceUNOSData/AirwayDehiscenceUNOSData.csv")
#airwayDehiscence <- read.csv("/Users//mathiasworni/UNOS datasets/BronchialStrictureUNOSData.csv")

#below is just to get a sense of what the dataset contains
head(airwayDehiscence)
str(airwayDehiscence)
names(airwayDehiscence)
summary(airwayDehiscence)

#below will view data in a spreadsheet format. comment if you don't need this, but my advice is to always look at the data in a sheet format when you start getting unexpected errors, as the error might be connected to some kind of data format problem
#View(airwayDehiscence)

attach(airwayDehiscence)


###########################################################################################
#ABSTRACT RESULTS
###########################################################################################
#Tony, below are examples of commands that we will use to determine each of the things you want for the abstract. you're welcome to give it a try, but once you define the specific variables I can help you get the specific commands for each. "var" means variable

#A total of XX,XXX patients were included -- to get total number of subjects you do 
str(airwayDehiscence)

#X,XXX (YY%) in induction group 1, X,XXX (YY%) in induction group 2, X,XXX (YY%) in induction group 3, and X,XXX (YY%) receiving no induction. The overall incidence of AD was ZZ% (= X,XXX/YY,YYY).
#percentage for categories
ctab(AA_IND_REGIMEN)
levels(AA_IND_REGIMEN)
#recoding, below is just a start. to know the names, please check the output of levels above (levels is the R word for alternative responses)
steroid  <- car::recode(AA_IND_REGIMEN, " 'STEROIDS/'= 1 ; 'STEROIDS/OTHER/' = 1 ; ''  = NA ; else = '0' ")
mode(steroid)


#There either was or was not a significant difference in induction regimen and AD following LTx (OR: X.X, 95%CI YY, ZZ)
model1  <- glm(PST_AIRWAY ~ steroid + GENDER + AGE, family=binomial(link="logit")) #this is just an example of how you would build a logistic model, need to add specific variables later. notice that the results of this model are being thrown into an object called "model1" and all commands below then simply query model1

summary(model1) #gives you model results
coefficients(model1) # model coefficients
confint(model1, level=0.95) # CIs for model parameters 
fitted(model1) # predicted values
residuals(model1) # residuals
anova(model1) # anova table, something like anova(model1, model2) will compare two nested models
vcov(model1) # covariance matrix for model parameters 
influence(model1) # regression diagnostics
layout(matrix(c(1,2,3,4),2,2)) # creates the white space for 4 graphs/page 
plot(model1) #generates 4 graphs/page



#######################################################################################
#template_secondary_data_analysis.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. You are free: to Share — to copy, distribute and transmit the work to Remix — to adapt the work, under the following conditions: Attribution — You must attribute the work in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work). Noncommercial — You may not use this work for commercial purposes. With the understanding that: Waiver — Any of the above conditions can be waived if you get permission from the copyright holder. Public Domain — Where the work or any of its elements is in the public domain under applicable law, that status is in no way affected by the license. Other Rights — In no way are any of the following rights affected by the license: Your fair dealing or fair use rights, or other applicable copyright exceptions and limitations; The author's moral rights; Rights other persons may have either in the work itself or in how the work is used, such as publicity or privacy rights. Notice — For any reuse or distribution, you must make clear to others the license terms of this work. The best way to do this is with a link to this web page. For more details see http://creativecommons.org/licenses/by-nc/3.0/
#######################################################################################
