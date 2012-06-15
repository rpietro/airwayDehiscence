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
#lapply(c("ggplot2", "psych", "RCurl", "irr", "car","Hmisc", "gmodels", "DAAG", "gdata", "catspec"), install.packages, character.only=T)
#install.packages("catspec")
#install.packages("survival", repos="http://cran.r-project.org" )
#lapply(c("ggplot2", "psych", "RCurl", "irr","Hmisc", "gmodels","qpcR", "car", "catspec", "gdata" , "catspec"), library, character.only=T)
library(survival)
library(foreign)
library(epicalc)
library(ggplot2)
library(gmodels)
library(graphics)
#####################################################################################
#IMPORTING DATA
#####################################################################################

#if you are using a file that is local to your computer, then replace path below by path to the data file. command will throw all the data into the templateData object
airwayDehiscence <- read.csv("/Users/rpietro/Google Drive/R/nonpublicdata_publications/AirwayDehiscenceUNOSData/AirwayDehiscenceUNOSData.csv")
airwayDehiscence <- read.csv("/Users//mathiasworni/UNOS datasets/10_01_tbl_Airway Dehiscence Analysis.csv")
airwayDehiscence <- read.csv("/Users/ac205/Desktop/R Source Data/10_01_tbl_Airway Dehiscence Analysis.csv")


#below is just to get a sense of what the dataset contains
head(airwayDehiscence)
str(airwayDehiscence)
names(airwayDehiscence)
attach(airwayDehiscence)

graphics.frame1 <- data.frame(BMI_RECIP)
ggplot(melt(graphics.frame1), aes(x = variable, y = value)) + geom_boxplot() 

summary(airwayDehiscence)

#below will view data in a spreadsheet format. comment if you don't need this, but my advice is to always look at the data in a sheet format when you start getting unexpected errors, as the error might be connected to some kind of data format problem

names(airwayDehiscence)
qplot(BMI_RECIP)
BMI_RECIP[BMI_RECIP>100] <- NA
qplot(BMI_RECIP)

#View(airwayDehiscence)
LIFE_SUP_TRR <- car::recode(LIFE_SUP_TRR, " 'Y' = 1; 'N' = 0; '' = NA ")
levels(LIFE_SUP_TRR)
summary(LIFE_SUP_TRR)

HIST_CIG_DON <- car::recode(HIST_CIG_DON, " 'Y' = 1; 'N' = 0; '' = NA ")#notice that ''=NA means two single quotes = NA rather than double quotes = NA. two single quotes together means missing for a factor as you know
levels(HIST_CIG_DON)
summary(HIST_CIG_DON)

head(AA_TX_TYPE)
CrossTable(Year.of.Tx, AA_TX_TYPE, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)

AA_TX_TYPE_2_GROUPS <- car::recode(AA_TX_TYPE, " 'Bilateral Sequential Lung'=0; 'Single Left Lung'=1; 'Single Right Lung'=1; ''=NA ")
CrossTable(AA_TX_TYPE, AA_TX_TYPE_2_GROUPS, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)

###########################################################################################
#ABSTRACT RESULTS
###########################################################################################
#Tony, below are examples of commands that we will use to determine each of the things you want for the abstract. you're welcome to give it a try, but once you define the specific variables I can help you get the specific commands for each. "var" means variable

#A total of XX,XXX patients were included -- to get total number of subjects you do 
str(airwayDehiscence)

#See GoogleDoc "Airway Dehiscence Backup Code" for induction medication recode if needed.

#A total of 18,906 patients were analyzed. ST were included in 57% on induction regimens (n=10803), PCA in 14% (n=2650), IL2 in 28% (n=5303), CD52 antibodies in 4% (n=736) and no induction in 29% (n=5523). The overall incidence of AD was 1.34% (= 253/18,906).

#I calculated the above in MS Access but might be good to build code for R for future purposes. - TC

names(airwayDehiscence)
CrossTable(Year.of.Tx, GENDER, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(PST_AIRWAY, GENDER, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
chisq.test(table(PST_AIRWAY, GENDER))

CrossTable(Year.of.Tx, AA_Steroid_IND, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(PST_AIRWAY, AA_Steroid_IND, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
chisq.test(table(PST_AIRWAY, AA_Steroid_IND))

CrossTable(Year.of.Tx, AA_Polyclonal_IND, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(PST_AIRWAY, AA_Polyclonal_IND, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
chisq.test(table(PST_AIRWAY, AA_Polyclonal_IND))

CrossTable(Year.of.Tx, AA_IL.2_IND, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(PST_AIRWAY, AA_IL.2_IND, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
chisq.test(table(PST_AIRWAY, AA_IL-2_IND))

CrossTable(Year.of.Tx, AA_Campath_IND, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(PST_AIRWAY, AA_Campath_IND, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
chisq.test(table(PST_AIRWAY, AA_Campath_IND))

describe(AGE~PST_AIRWAY)
t.test(AGE~PST_AIRWAY)
sd(AGE, na.rm = FALSE) #na.rm=FALSE removes the missing from SD calculation
tapply(AGE,PST_AIRWAY,IQR) #Will give you the IQR by grups
tapply(AGE,PST_AIRWAY,summary) #Also will give some regular descriptives by groups
describe.by(AGE,PST_AIRWAY) # Will give you regular descriptives by groups
ggplot(df, aes(x=AGE)) + geom_histogram(binwidth=.5, colour="black", fill="white")

describe(AGE_DON~PST_AIRWAY)
t.test(AGE_DON~PST_AIRWAY)
sd(AGE_DON, na.rm = FALSE) #na.rm=FALSE removes the missing from SD calculation
tapply(AGE_DON,PST_AIRWAY,IQR) #Will give you the IQR by grups
tapply(AGE_DON,PST_AIRWAY,summary) #Also will give some regular descriptives by groups
describe.by(AGE_DON,PST_AIRWAY) # Will give you regular descriptives by groups
ggplot(df, aes(x=AGE_DON)) + geom_histogram(binwidth=.5, colour="black", fill="white")

CrossTable(Year.of.Tx, AA_RACE, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(PST_AIRWAY, AA_RACE, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
chisq.test(table(PST_AIRWAY, AA_RACE))

describe(BMI_TCR~PST_AIRWAY)
t.test(BMI_TCR~PST_AIRWAY)
sd(BMI_TCR, na.rm = FALSE) #na.rm=FALSE removes the missing from SD calculation
tapply(BMI_TCR,PST_AIRWAY,IQR) #Will give you the IQR by grups
tapply(BMI_TCR,PST_AIRWAY,summary) #Also will give some regular descriptives by groups
describe.by(BMI_TCR,PST_AIRWAY) # Will give you regular descriptives by groups
ggplot(df, aes(x=BMI_TCR)) + geom_histogram(binwidth=.5, colour="black", fill="white")

CrossTable(Year.of.Tx, Diag.Category, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(PST_AIRWAY, Diag.Category, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
chisq.test(table(PST_AIRWAY, Diag.Category))

CrossTable(Year.of.Tx, AA_TX_TYPE, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(PST_AIRWAY, AA_TX_TYPE, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
chisq.test(table(PST_AIRWAY, AA_TX_TYPE))

CrossTable(Year.of.Tx, ERA.of.TX, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(PST_AIRWAY, ERA.of.TX, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
chisq.test(table(PST_AIRWAY, ERA.of.TX))

CrossTable(Year.of.Tx, AA_Recip_DM, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(PST_AIRWAY, AA_Recip_DM, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
chisq.test(table(PST_AIRWAY, AA_Recip_DM))

CrossTable(Year.of.Tx, LIFE_SUP_TRR, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(PST_AIRWAY, LIFE_SUP_TRR, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
chisq.test(table(PST_AIRWAY, LIFE_SUP_TRR))

#AA_CMV_MISMATCH is a variable with a lot of missing values!!! Since PST_AIRWAY most likely happens early, the influence of CMV mismatch might not be that important
CrossTable(Year.of.Tx, AA_CMV_MISMATCH, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(PST_AIRWAY, AA_CMV_MISMATCH, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
chisq.test(table(PST_AIRWAY, AA_CMV_MISMATCH))

CrossTable(Year.of.Tx, HIST_CIG_DON, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(PST_AIRWAY, HIST_CIG_DON, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
chisq.test(table(PST_AIRWAY, HIST_CIG_DON))

CrossTable(Year.of.Tx, HLAMAT, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(PST_AIRWAY, HLAMAT, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
chisq.test(table(PST_AIRWAY, HLAMAT))

#PO2 has a lot of missing values (4,622)
describe(PO2~PST_AIRWAY)
t.test(PO2~PST_AIRWAY)
sd(PO2, na.rm = FALSE) #na.rm=FALSE removes the missing from SD calculation
tapply(PO2,PST_AIRWAY,IQR) #Will give you the IQR by grups
tapply(PO2,PST_AIRWAY,summary) #Also will give some regular descriptives by groups
describe.by(PO2,PST_AIRWAY) # Will give you regular descriptives by groups
ggplot(df, aes(x=PO2)) + geom_histogram(binwidth=.5, colour="black", fill="white")

#has about 1,691 missing values
describe(ISCHTIME~PST_AIRWAY)
t.test(ISCHTIME~PST_AIRWAY)
sd(ISCHTIME, na.rm = FALSE) #na.rm=FALSE removes the missing from SD calculation
tapply(ISCHTIME,PST_AIRWAY,IQR) #Will give you the IQR by grups
tapply(ISCHTIME,PST_AIRWAY,summary) #Also will give some regular descriptives by groups
describe.by(ISCHTIME,PST_AIRWAY) # Will give you regular descriptives by groups
ggplot(df, aes(x=ISCHTIME)) + geom_histogram(binwidth=.5, colour="black", fill="white")

CrossTable(Year.of.Tx, HLAMAT, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(PST_AIRWAY, HLAMAT, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
chisq.test(table(PST_AIRWAY, HLAMAT))

#Univariate and multivariate regression analysis with the following predictor variables: AA_Steroid_IND +  AA_Polyclonal_IND + 
# AA_IL-2_IND +  AA_Campath_IND;  adjusted for possible confounders: AGE + AGE_DON + GENDER + AA_RACE + BMI_TCR + Diag.Category + 
# AA_TX_TYPE + ERA.of.TX + AA_Recip_DM + LIFE_SUP_TRR + AA_CMV_MISMATCH + HIST_CIG_DON + HLAMAT + PO2 + ISCHTIME + END_MATCH_LAS
#There either was or was not a significant difference in induction regimen and AD following LTx (OR: X.X, 95%CI YY, ZZ)

names(airwayDehiscence)

simple.logistic1  <- glm(PST_AIRWAY ~ AA_Steroid_IND, na.action=na.omit) 
summary(simple.logistic1) #gives you model results
coefficients(simple.logistic1) # model coefficients
confint(simple.logistic1, level=0.95) # CIs for model parameters

logistic1  <- glm(PST_AIRWAY ~ AA_Steroid_IND + AA_Polyclonal_IND + AA_Campath_IND + AGE + AGE_DON + GENDER + AA_RACE + BMI_TCR + AA_TX_TYPE + AA_Recip_DM + LIFE_SUP_TRR + AA_CMV_MISMATCH + HIST_CIG_DON + HLAMAT + PO2 + ISCHTIME + END_MATCH_LAS, family=binomial(link="logit")) 

#We also reported the outcomes of patients who developed AD regardless of preceding induction regimen.  

KMPlot <- survfit(Surv(AA_FU_TIME_IN_YEARS, AA_SURVIVAL_STATUS) ~ PST_AIRWAY, data=aml)
plot(KMPlot)


dehispreduni1  <- glm(PST_AIRWAY ~ AA_Steroid_IND , family=binomial(link="logit")) 
logistic.display(dehispreduni1)

dehispreduni2  <- glm(PST_AIRWAY ~ AA_Polyclonal_IND , family=binomial(link="logit")) 
logistic.display(dehispreduni2)

dehispreduni3  <- glm(PST_AIRWAY ~ AA_Campath_IND , family=binomial(link="logit")) 
logistic.display(dehispreduni3)

dehispreduni4  <- glm(PST_AIRWAY ~ AGE , family=binomial(link="logit")) 
summary(dehispreduni4) #gives you model results
logistic.display(dehispreduni4)

dehispreduni5  <- glm(PST_AIRWAY ~ AGE_DON , family=binomial(link="logit")) 
summary(dehispreduni5) #gives you model results
logistic.display(dehispreduni5)

dehispreduni6  <- glm(PST_AIRWAY ~ GENDER , family=binomial(link="logit")) 
logistic.display(dehispreduni6)

dehispreduni7  <- glm(PST_AIRWAY ~ AA_RACE , family=binomial(link="logit")) 
logistic.display(dehispreduni7)

#have to change BMI!!!!
dehispreduni8  <- glm(PST_AIRWAY ~ BMI_RECIP , family=binomial(link="logit")) 
summary(dehispreduni8) #gives you model results
logistic.display(dehispreduni8)
CrossTable(PST_AIRWAY, BMI_RECIP, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)


dehispreduni9  <- glm(PST_AIRWAY ~ AA_TX_TYPE , family=binomial(link="logit")) 
logistic.display(dehispreduni9)

dehispreduni10  <- glm(PST_AIRWAY ~ AA_Recip_DM , family=binomial(link="logit")) 
logistic.display(dehispreduni10)

dehispreduni11  <- glm(PST_AIRWAY ~ LIFE_SUP_TRR , family=binomial(link="logit")) 
summary(dehispreduni11) #gives you model results
logistic.display(dehispreduni11)
class(LIFE_SUP_TRR) #tells you that the variable is a factor
levels(LIFE_SUP_TRR)#tells you that one of the levels is " " which is why missing is becoming the referent for the dummy

LIFE_SUP_TRR_num <- car::recode(LIFE_SUP_TRR, " 'Y' = 1; 'N'= 0; ''=NA ") #converts the variable into a numeric variable where the missing for a factor ( " ") is transformed into missing for a numeric variables (NA)

summary(LIFE_SUP_TRR_num)


#CrossTable(PST_AIRWAY, LIFE_SUP_TRR, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
dehispreduni12  <- glm(PST_AIRWAY ~ AA_CMV_MISMATCH , family=binomial(link="logit")) 
summary(dehispreduni12) #gives you model results
logistic.display(dehispreduni12)

#MISSING VALUES HAVE TO BE EXCLUDED - RECODED!
dehispreduni13  <- glm(PST_AIRWAY ~ HIST_CIG_DON , family=binomial(link="logit")) 
summary(dehispreduni13) #gives you model results
logistic.display(dehispreduni13)
CrossTable(PST_AIRWAY, HIST_CIG_DON, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)


dehispreduni33  <- glm(PST_AIRWAY ~ ERA.of.TX , family=binomial(link="logit")) 
summary(dehispreduni33) #gives you model results
logistic.display(dehispreduni33)



#I DID REPLACE AA_TX_TYPE BY AA_TX_TYPE_2_GROUPS - EASIER FOR THE ABSTRACT!
dehis_pred1  <- glm(PST_AIRWAY ~ AA_Steroid_IND +AA_IL.2_IND + AA_Polyclonal_IND + AA_Campath_IND + AGE + AGE_DON + GENDER + AA_RACE + BMI_RECIP + AA_TX_TYPE_2_GROUPS + AA_Recip_DM + LIFE_SUP_TRR  + HIST_CIG_DON  + ISCHTIME  + Diag.Category + ERA.of.TX , family=binomial(link="logit")) 
summary(dehis_pred1) #gives you model results
logistic.display(dehis_pred1)
exp(confint(dehis_pred1))

survival1 <- coxph(Surv(AA_FU_TIME_IN_YEARS, AA_SURVIVAL_STATUS) ~ AA_Steroid_IND + AA_IL.2_IND, ties="efron")
survival1 <- coxph(Surv(AA_FU_TIME_IN_YEARS, AA_SURVIVAL_STATUS) ~ PST_AIRWAY+AA_Steroid_IND +AA_IL.2_IND + AA_Polyclonal_IND + AA_Campath_IND + AGE + AGE_DON + GENDER + AA_RACE + BMI_RECIP + AA_TX_TYPE_2_GROUPS + AA_Recip_DM + LIFE_SUP_TRR  + HIST_CIG_DON  + ISCHTIME  + Diag.Category + ERA.of.TX, data=airwayDehiscence)
#below will test for proportional hazards assumption
prop.assump1 <- cox.zph(survival1) 
print(prop.assump1) #display results for assumption 
plot(prop.assump1)  #plot curves -- from the help page: "If the proportional hazards assumption is true, beta(t) will be a horizontal line. The printout gives a test for slope=0."
#summary results for the model
summary(survival1)

#######################################################################################
#template_secondary_data_analysis.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. You are free: to Share — to copy, distribute and transmit the work to Remix — to adapt the work, under the following conditions: Attribution — You must attribute the work in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work). Noncommercial — You may not use this work for commercial purposes. With the understanding that: Waiver — Any of the above conditions can be waived if you get permission from the copyright holder. Public Domain — Where the work or any of its elements is in the public domain under applicable law, that status is in no way affected by the license. Other Rights — In no way are any of the following rights affected by the license: Your fair dealing or fair use rights, or other applicable copyright exceptions and limitations; The author's moral rights; Rights other persons may have either in the work itself or in how the work is used, such as publicity or privacy rights. Notice — For any reuse or distribution, you must make clear to others the license terms of this work. The best way to do this is with a link to this web page. For more details see http://creativecommons.org/licenses/by-nc/3.0/
#######################################################################################
