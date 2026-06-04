data<-Response_final_check
View(data)

#removal of non consent 
data<-data[data$Q92=="Yes, I give consent for my research data to be collected, stored, processed, and published as described in the information letter.",]
table(data$Q92)

colSums(is.na(data))

#deleting columns 
data$`Gender Identity_4_TEXT`<-NULL
data$Q92<-NULL
data$Q98<-NULL
data$DistributionChannel<-NULL
data$UserLanguage<-NULL
data <- data[, -c(1:8)]

table(data$Finished=="True")

#cleaning the data
##convert to numeric 
table(data$`NFC  _1`)
data$`NFC  _1`<-factor(data$`NFC  _1`, levels = c("Strongly disagree",
                                                  "Moderately disagree",
                                                  "Slightly disagree",
                                                  "Slightly Agree",
                                                  "Moderately Agree",
                                                  "Strongly agree"), ordered = TRUE)
data$NFC1_num<-as.numeric(data$`NFC  _1`)
as.numeric(data$`NFC  _1`)

table(data$`NFC  _2`)
data$`NFC  _2`<-factor(data$`NFC  _2`, levels = c("Strongly disagree",
                                                  "Moderately disagree",
                                                  "Slightly disagree",
                                                  "Slightly Agree",
                                                  "Moderately Agree",
                                                  "Strongly agree"), ordered = TRUE)
data$NFC2_num<-as.numeric(data$`NFC  _2`)

table(data$`NFC  _3`)
data$`NFC  _3`<-factor(data$`NFC  _3`, levels = c("Strongly disagree",
                                                  "Moderately disagree",
                                                  "Slightly disagree",
                                                  "Slightly Agree",
                                                  "Moderately Agree",
                                                  "Strongly agree"), ordered = TRUE)
data$NFC3_num<-as.numeric(data$`NFC  _3`)
table(data$`NFC  _4`)
data$`NFC  _4`<-factor(data$`NFC  _4`, levels = c("Strongly disagree",
                                                  "Moderately disagree",
                                                  "Slightly disagree",
                                                  "Slightly Agree",
                                                  "Moderately Agree",
                                                  "Strongly agree"), ordered = TRUE)
data$NFC4_num<-as.numeric(data$`NFC  _4`)

table(data$`NFC  _5`)
data$`NFC  _5`<-factor(data$`NFC  _5`, levels = c("Strongly disagree",
                                                  "Moderately disagree",
                                                  "Slightly disagree",
                                                  "Slightly Agree",
                                                  "Moderately Agree",
                                                  "Strongly agree"), ordered = TRUE)
data$NFC5_num<-as.numeric(data$`NFC  _5`)

table(data$`NFC  _6`)
data$`NFC  _6`<-factor(data$`NFC  _6`, levels = c("Strongly disagree",
                                                  "Moderately disagree",
                                                  "Slightly disagree",
                                                  "Slightly Agree",
                                                  "Moderately Agree",
                                                  "Strongly agree"), ordered = TRUE)
data$NFC6_num<-as.numeric(data$`NFC  _6`)

table(data$`NFC  _7`)
data$`NFC  _7`<-factor(data$`NFC  _7`, levels = c("Strongly disagree",
                                                  "Moderately disagree",
                                                  "Slightly disagree",
                                                  "Slightly Agree",
                                                  "Moderately Agree",
                                                  "Strongly agree"), ordered = TRUE)
data$NFC7_num<-as.numeric(data$`NFC  _7`)

table(data$`NFC  _8`)
data$`NFC  _8`<-factor(data$`NFC  _8`, levels = c("Strongly disagree",
                                                  "Moderately disagree",
                                                  "Slightly disagree",
                                                  "Slightly Agree",
                                                  "Moderately Agree",
                                                  "Strongly agree"), ordered = TRUE)
data$NFC8_num<-as.numeric(data$`NFC  _8`)

table(data$`NFC  _9`)
data$`NFC  _9`<-factor(data$`NFC  _9`, levels = c("Strongly disagree",
                                                  "Moderately disagree",
                                                  "Slightly disagree",
                                                  "Slightly Agree",
                                                  "Moderately Agree",
                                                  "Strongly agree"), ordered = TRUE)
data$NFC9_num<-as.numeric(data$`NFC  _9`)

##doing the same conversion for SWLS
table(data$`Life Satisfaction_1`)

likert_cols<-c("Life Satisfaction_1", "Life Satisfaction_2", "Life Satisfaction_3", "Life Satisfaction_4", "Life Satisfaction_5")
for(col in likert_cols){data[[paste0(col,"_num")]]<-as.numeric(factor(data[[col]],levels = c("Strongly disagree", 
                                                                                             "Disagree", 
                                                                                             "Somewhat disagree", 
                                                                                             "Neither disagree or agree", 
                                                                                             "Somewhat agree", 
                                                                                             "Agree", 
                                                                                             "Strongly agree"), 
                                                                      ordered = TRUE))}



##doing the same for PANAS-PA
table(data$`PA Scale_1`)
rm(likert_cols2)
rm(likert_levels)

table(data$`PA Scale_1`)
data$`PA Scale_1`<-factor(data$`PA Scale_1`, levels = c("Very slightly or not at all",
                                                        "A little",
                                                        "Moderately",
                                                        "Quite a bit",
                                                        "Extremely"
), ordered = TRUE)
data$PA1_num<-as.numeric(data$`PA Scale_1`)

table(data$`PA Scale_2`)
data$`PA Scale_2`<-factor(data$`PA Scale_2`, levels = c("Very slightly or not at all",
                                                        "A little",
                                                        "Moderately",
                                                        "Quite a bit",
                                                        "Extremely"
), ordered = TRUE)
data$PA2_num<-as.numeric(data$`PA Scale_2`)
"PA2_num" %in% names(data)
View(data)
head(data[, c("PA Scale_2", "PA2_num")])
tail(names(data), 52)

class(data)
names(data)


table(data$`PA Scale_3`)
data$`PA Scale_3`<-factor(data$`PA Scale_3`, levels = c("Very slightly or not at all",
                                                        "A little",
                                                        "Moderately",
                                                        "Quite a bit",
                                                        "Extremely"
), ordered = TRUE)
data$PA3_num<-as.numeric(data$`PA Scale_3`)


table(data$`PA Scale_4`)
data$`PA Scale_4`<-factor(data$`PA Scale_4`, levels = c("Very slightly or not at all",
                                                        "A little",
                                                        "Moderately",
                                                        "Quite a bit",
                                                        "Extremely"
), ordered = TRUE)
data$PA4_num<-as.numeric(data$`PA Scale_4`)


table(data$`PA Scale_5`)
data$`PA Scale_5`<-factor(data$`PA Scale_5`, levels = c("Very slightly or not at all",
                                                        "A little",
                                                        "Moderately",
                                                        "Quite a bit",
                                                        "Extremely"
), ordered = TRUE)
data$PA5_num<-as.numeric(data$`PA Scale_5`)


table(data$`PA Scale_6`)
data$`PA Scale_6`<-factor(data$`PA Scale_6`, levels = c("Very slightly or not at all",
                                                        "A little",
                                                        "Moderately",
                                                        "Quite a bit",
                                                        "Extremely"
), ordered = TRUE)
data$PA6_num<-as.numeric(data$`PA Scale_6`)


table(data$`PA Scale_7`)
data$`PA Scale_7`<-factor(data$`PA Scale_7`, levels = c("Very slightly or not at all",
                                                        "A little",
                                                        "Moderately",
                                                        "Quite a bit",
                                                        "Extremely"
), ordered = TRUE)
data$PA7_num<-as.numeric(data$`PA Scale_7`)


table(data$`PA Scale_8`)
data$`PA Scale_8`<-factor(data$`PA Scale_8`, levels = c("Very slightly or not at all",
                                                        "A little",
                                                        "Moderately",
                                                        "Quite a bit",
                                                        "Extremely"
), ordered = TRUE)
data$PA8_num<-as.numeric(data$`PA Scale_8`)


table(data$`PA Scale_9`)
data$`PA Scale_9`<-factor(data$`PA Scale_9`, levels = c("Very slightly or not at all",
                                                        "A little",
                                                        "Moderately",
                                                        "Quite a bit",
                                                        "Extremely"
), ordered = TRUE)
data$PA9_num<-as.numeric(data$`PA Scale_9`)


table(data$`PA Scale_10`)
data$`PA Scale_10`<-factor(data$`PA Scale_10`, levels = c("Very slightly or not at all",
                                                          "A little",
                                                          "Moderately",
                                                          "Quite a bit",
                                                          "Extremely"
), ordered = TRUE)
data$PA10_num<-as.numeric(data$`PA Scale_10`)


##converting political ideology 
data$Q28_1[is.na(data$Q28_1)] <- "Neither liberal nor conservative"
table(data$Q28_1)
data$Q28_1<-factor(data$Q28_1, levels = c("Strongly liberal",
                                          "Liberal",
                                          "Somewhat liberal",
                                          "Neither liberal nor conservative",
                                          "Somewhat conservative", 
                                          "Conservative",
                                          "Strongly Conservative"
), ordered = TRUE)
data$PolId_num<-as.numeric(data$Q28_1)


##reliability of NFCC 
str(data)
head(data)
install.packages("psych")
library(psych)
nfcc_items<-data[, c("NFC1_num", "NFC2_num", "NFC3_num", "NFC4_num", "NFC5_num", "NFC6_num", "NFC7_num", "NFC8_num", "NFC9_num")]
alpha(nfcc_items)
##raw_alpa is 0.86 

#Cronbachs alpha per subscale 

##order
order_items<-data[, c("NFC1_num", "NFC2_num", "NFC3_num")]
alpha(order_items)

##predictability 
pred_items<-data[, c("NFC4_num", "NFC5_num", "NFC6_num")] 
alpha(pred_items)

##ambiguity
amb_items<-data[, c("NFC7_num", "NFC8_num", "NFC9_num")]
alpha(amb_items)

#combining PANAS and SWLS
swls_items<-data[, c ("Life Satisfaction_1_num", "Life Satisfaction_2_num", "Life Satisfaction_3_num", "Life Satisfaction_4_num", "Life Satisfaction_5_num")]

pa_items<-data[, c("PA1_num", "PA2_num", "PA3_num", "PA4_num", "PA5_num", "PA6_num", "PA7_num", "PA9_num", "PA10_num")]

alpha(swls_items)
alpha(pa_items)


##create scale scores means
data$SWLS_mean<-rowMeans(swls_items, na.rm = TRUE)
data$PA_mean<-rowMeans(pa_items, na.rm = TRUE)

##standardise (z-scores)
data$SWLS_z<-scale(data$SWLS_mean)
data$PA_z<-scale(data$PA_mean)

##creating Subjective well-being index
data$SWB<-rowMeans(cbind(data$SWLS_z, data$PA_z), na.rm = TRUE)

swls_alpha<-alpha(swls_items)
pa_alpa<-alpha(pa_items)

swls_alpha$total$raw_alpha
pa_alpa$total$raw_alpha

data$SWLS_mean
data$PA_mean

data$SWLS_z

data$SWB
summary(data$SWB)

##violations of linear regression modelling checked 
data$NFCC<-rowMeans(data[, c("NFC1_num", "NFC2_num", "NFC3_num", "NFC4_num", "NFC5_num", "NFC6_num", "NFC7_num", "NFC8_num", "NFC9_num")], na.rm=TRUE)
model<-lm(SWB~PolId_num+NFCC, data = data)

#outliers cooks d 
cooks_d<-cooks.distance(model)

threshold<-4/nrow(data)

which(cooks_d>threshold)

model_no_outliers <- lm(SWB ~ PolId_num + NFCC, data = data[-c(2, 23, 28, 35, 90, 148, 183, 198, 210, 249, 268), ])
model_orig <- lm(SWB ~ PolId_num + NFCC, data = data)

summary(model_orig)
summary(model_no_outliers)

##normality and linearity 
qqnorm(residuals(model))
qqline(residuals(model), col="red")

# homoscedasticity: breusch-pagan 
install.packages("lmtest")
library(lmtest)

bptest(model)

## multicollinearity 
install.packages("car")
library(car)

vif(model)

##testing if NFCC predicts SWB? 
data$NFCC<-rowMeans(nfcc_items, na.rm = TRUE)
data$SWB

##running the regression 
model<-lm(SWB~NFCC, data=data)
summary(model)

#lavaan mediation model
install.packages("lavaan")
library(lavaan)

med_model<-'PolId_num~a*NFCC
SWB~b*PolId_num+c*NFCC
indirect:=a*b
total:=c+(a*b)'

##run the model with bootstrapping 
fit<-sem(med_model, data = data,
         se="bootstrap", 
         bootstrap=5000)
summary(fit,
        standardized=TRUE, 
        fit.measures=TRUE,
        ci=TRUE)

#creating table 1 - demographics table 
install.packages("gtsummary")
library(gtsummary)

nrow(data)  #270 participants 

#age
data$Age[data$Age == 2005] <- 21
data$Age<-as.numeric(data$Age)
mean(data$Age) #31.74
sd(data$Age)  #13.53
min(data$Age) #18
max(data$Age) #79
summary(data$PolId_num)
summary(data$Q28_1)

#visualising data - polid 
table(data$Q28_1)
barplot(table(data$Q28_1), main = "Politcal Ideology Distribution",
        xlab = "Ideology (1=Strongly Liberal, 7=Strongly Conservative", 
        ylab = "Frequency",
        col = "white")
#frequency table for polid
summary(data$Q28_1)
sd(data$PolId_num, na.rm=TRUE)  #sd=1.491601
table(data$PolId_num)
prop.table(table(data$PolId_num))

install.packages("psych")
library(psych)
describe(data$PolId_num)

#gender
table(data$`Gender Identity`)  #177 females, 91 males, 2 non-binary

#education
table(data$Education)
  #master or above 96
  #bachelor 72
  #primary 1 
  #Some secondary 3
  #some uni 59
  #prefer not to say 3 
  #secondary 29 
  #Vocational or similar 7 

table(data$`Country of Residence`)
#belgium 1, Cyprus, 17, Czech republic 1. France 2, Germany 51, Greece 32, Hungary 26, Italy 2, Netherlands 107, Spain 2, Switzerland 3, Turkey 19, Uk or Northern ireland 7


data<-data[-195,]


##descriptives table 
install.packages("tidyverse")
library(tidyverse)
install.packages("psych")
library(psych)

data <- data %>%
  mutate(
    NFC_Total = rowMeans(select(., NFC1_num:NFC9_num), na.rm = TRUE),)

stats_table <- data %>%
  select(PolId_num, NFC_Total, SWB) %>%
  describe() 
print(stats_table)


#correlation matrix 
cor(data[, c("NFCC", "PolId_num", "SWB")], 
    use="complete.obs", 
    method = "pearson")

install.packages("Hmisc")
library(Hmisc)
results<-rcorr(as.matrix(data[, c("NFCC", "PolId_num", "SWB")]))
results$r
results$P


##calculating bayes factor 
install.packages("BayesFactor")
library(BayesFactor)

correlationBF(data$NFCC, data$PolId_num)
correlationBF(data$PolId_num, data$SWB)

bf<-regressionBF(SWB~NFCC+PolId_num, 
                 data = data)
bf

#checking the R version 
R.version
