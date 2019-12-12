setwd("*path of the working directory*")

mydata <- read.csv("*path and name of the csv file for import*")

attach(mydata)

names(mydata)

head(mydata)

#--------------------------------1)APPLICATION-----------------------------------

# 1) Applied by Attended 
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0) #converting 'Yes' or 'No' from the 'APPLIED' column in my original dataset into '1' and '0', and storing these values in a newly created R vector named 'applied'
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes", 1,0)
linreg = lm(applied ~ attended, data = mydata) #performing linear regression to model relationship between attending the webinar(independent variable) and applying for the grant(dependent variable)
summary(linreg)

# 1)b) Applied by Attended, controlling for region 
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(applied ~ attended + DEVELOPMENT.REGION, data = mydata) #note that one of the regions will be omitted from the resulting table as this is the reference region
summary(linreg)

# 1)c) Applied by Attended, controlling for region + category + amount requested
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
#testing 'logit' regression below as this might fit our data better since our dependent variable 'applied' is binary. see: https://towardsdatascience.com/logistic-regression-for-dummies-a-detailed-explanation-9597f76edf46 
linreg = glm(applied ~ attended + DEVELOPMENT.REGION, + CATEGORY + REQUESTED.AMOUNT, family=binomial(link='logit'),data = mydata) 
summary(linreg)

#-------------------------------2)SUCCESSFUL APPLICATION-------------------------

# 2) Success by Attended
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(success ~ attended, data = subset(mydata,applied==1)) #subsetting because only those who applied are relevant here
summary(linreg)

# 2)b) Success by Attended, controlling for pass/fail 
#This should get rid of the effect of missed documents(which determines pass/fail) on the success
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(success ~ attended + PASS.FAIL, data = subset(mydata,applied==1))
summary(linreg)

# 2)c) Success by Attended, controlling for category 
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(success ~ attended + CATEGORY, data = subset(mydata,applied==1))
summary(linreg)

# 2) d) Success by Attended, controlling for region 
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(success ~ attended + DEVELOPMENT.REGION, data = subset(mydata,applied==1))
summary(linreg)

# 2) e) Success by Attended, controlling for region + category 
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(success ~ attended + DEVELOPMENT.REGION + CATEGORY, data = subset(mydata,applied==1))
summary(linreg)

# 2) f) Success by Attended, controlling for region + category + amount requested 
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(success ~ attended + DEVELOPMENT.REGION + CATEGORY + REQUESTED.AMOUNT, data = subset(mydata,applied==1))
summary(linreg)

#*******************Testing LOGIT on each of the above****************

# 2) Success by Attended
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = glm(success ~ attended, family=binomial(link='logit'), data = subset(mydata,applied==1))
summary(linreg)

# 2)b) Success by Attended, controlling for pass/fail 
#This should get rid of the effect of missed documents on the success, but 
#why does fail not have a negative coefficient?
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = glm(success ~ attended + PASS.FAIL, family=binomial(link='logit'), data = subset(mydata,applied==1))
summary(linreg)

# 2)c) Success by Attended, controlling for category 
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = glm(success ~ attended + CATEGORY, family=binomial(link='logit'), data = subset(mydata,applied==1))
summary(linreg)

# 2) d) Success by Attended, controlling for region 
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = glm(success ~ attended + DEVELOPMENT.REGION, family=binomial(link='logit'), data = subset(mydata,applied==1))
summary(linreg)

# 2) e) Success by Attended, controlling for region + category 
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = glm(success ~ attended + DEVELOPMENT.REGION + CATEGORY, family=binomial(link='logit'), data = subset(mydata,applied==1))
summary(linreg)

# 2) f) Success by Attended, controlling for region + category + amount requested 
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = glm(success ~ attended + DEVELOPMENT.REGION + CATEGORY + REQUESTED.AMOUNT, family=binomial(link='logit'), data = subset(mydata,applied==1))
summary(linreg)

# 2) g) Success by Attended, controlling for region + category + amount requested (with URBAN)
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
#adding an interaction variable below (attended*URBAN) to see impact of being from an urban region AND attending
linreg = glm(success ~ attended + URBAN + attended*URBAN + DEVELOPMENT.REGION + CATEGORY + REQUESTED.AMOUNT, family=binomial(link='logit'), data = subset(mydata,applied==1))
summary(linreg)

#-------------------------------------3)SCORE--------------------------------------

# 3) Score by Attended 
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(SCORE ~ attended, data = subset(mydata,applied==1)) #subsetting because only those who applied are relevant here
summary(linreg)

# 3)b) Score by Attended, controlling for amount requested 
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(SCORE ~ attended + REQUESTED.AMOUNT, data = subset(mydata,applied==1))
summary(linreg)

# 3)c) Score by Attended, controlling for amount requested + region + category 
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(SCORE ~ attended + DEVELOPMENT.REGION + CATEGORY + REQUESTED.AMOUNT, data = subset(mydata,applied==1))
summary(linreg)

#--------------------------------------4)PASS/FAIL---------------------------------
                                                                                                     
# 4) Pass/Fail by Attended 
mydata$passfail = ifelse(mydata$PASS.FAIL == "PASS", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(passfail ~ attended, data = subset(mydata,applied==1))
summary(linreg)

#---------------------------------------5)MISC--------------------------------------

# 5)Success by Score
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(success ~ SCORE, data = subset(mydata,applied==1))
summary(linreg)




