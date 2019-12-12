setwd("*path of the working directory*")

mydata <- read.csv("*path and name of the csv file for import*")

attach(mydata)

names(mydata)

head(mydata)

# 1) Number of Applications by Organization
mydata$time = ifelse(mydata$Intake == 5, 1, 0) #creating new vector indicating if this was the 5th intake
mydata$treated = ifelse(mydata$Attended.Webinar == "Yes", 1, 0) #attending the webinar is our treatment 
mydata$did = mydata$time * mydata$treated 
didreg = lm( Applications ~ treated + time + did, data = mydata)
summary(didreg)

# 1)b) Number of Applications by Organization (with subsetting)
mydata$time = ifelse(mydata$Intake == 5, 1, 0)
mydata$treated = ifelse(mydata$Attended.Webinar == "Yes", 1, 0)
mydata$did = mydata$time * mydata$treated
didreg = lm( Applications ~ treated + time + did, data = subset(mydata, Intake.5.Applications>0))
summary(didreg)

# 2) Number of Applications by Organization 
#   (not accounting for multiple applications by the same organization)
mydata$time = ifelse(mydata$Intake == 5, 1, 0)
mydata$treated = ifelse(mydata$Attended.Webinar == "Yes", 1, 0)
mydata$did = mydata$time * mydata$treated
mydata$outcome = ifelse(mydata$Applications == 0, 0, 1)
didreg = lm( outcome ~ treated + time + did, data = mydata)
summary(didreg)

# 3) Successful Applications by Organization
mydata$time = ifelse(mydata$Intake == 5, 1, 0)
mydata$treated = ifelse(mydata$Attended.Webinar == "Yes", 1, 0)
mydata$did = mydata$time * mydata$treated
didreg = lm( Approved ~ treated + time + did, data = subset(mydata, Intake.5.Applications>0))
summary(didreg)

# 4) Declined Applications by Organization
mydata$time = ifelse(mydata$Intake == 5, 1, 0)
mydata$treated = ifelse(mydata$Attended.Webinar == "Yes", 1, 0)
mydata$did = mydata$time * mydata$treated
didreg = lm( Declined ~ treated + time + did, data = subset(mydata, Intake.5.Applications>0))
summary(didreg)

# 5) Success Rate by Organization 
mydata$time = ifelse(mydata$Intake == 5, 1, 0)
mydata$treated = ifelse(mydata$Attended.Webinar == "Yes", 1, 0)
mydata$did = mydata$time * mydata$treated
mydata$successrate = Approved/(Approved+Declined)
didreg = lm( successrate ~ treated + time + did, data = subset(mydata, Intake.5.Applications>0))
summary(didreg)

# 5)b) Success Rate by Organization (without subsetting)
mydata$time = ifelse(mydata$Intake == 5, 1, 0)
mydata$treated = ifelse(mydata$Attended.Webinar == "Yes", 1, 0)
mydata$did = mydata$time * mydata$treated
mydata$successrate = Approved/(Approved+Declined)
didreg = lm( successrate ~ treated + time + did, data = mydata)
summary(didreg)

# 6) Success Rate by Organization with Urban interactions
mydata$time = ifelse(mydata$Intake == 5, 1, 0)
mydata$treated = ifelse(mydata$Attended.Webinar == "Yes", 1, 0)
mydata$did = mydata$time * mydata$treated
mydata$successrate = Approved/(Approved+Declined)
didreg = lm( successrate ~ treated + time + did + Urban + Urban*treated + Urban*time + Urban*did, data = subset(mydata, Intake.5.Applications>0))
summary(didreg)

# 6)b) Success Rate by Organization with Urban interactions(without subsetting)
mydata$time = ifelse(mydata$Intake == 5, 1, 0)
mydata$treated = ifelse(mydata$Attended.Webinar == "Yes", 1, 0)
mydata$did = mydata$time * mydata$treated
mydata$successrate = Approved/(Approved+Declined)
didreg = lm( successrate ~ treated + time + did + Urban + Urban*treated + Urban*time + Urban*did, data = mydata)
summary(didreg)

# 7)Success Rate by Organization with binary Approvals and Rejections (without subsetting)
mydata$time = ifelse(mydata$Intake == 5, 1, 0)
mydata$treated = ifelse(mydata$Attended.Webinar == "Yes", 1, 0)
mydata$did = mydata$time * mydata$treated
mydata$successrate = A/(A+B)
didreg = lm( successrate ~ treated + time + did, data = mydata)
summary(didreg)
