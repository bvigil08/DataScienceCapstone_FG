## Briana Vigil
## Data Science Capstone
## 4/14/22

#Summary: The data from allAlums.csv comes from the WellesleyHive and looks
#         at all the information provided by alums in their career after
#         Wellesley. Here we hope to see if there are any differences between
#         the pursuit of higher education differences between both first-gen
#         non first-gen alums from Wellesley

#Clear global environment
rm(list=ls())

getwd()
setwd('/Users/brianavigil/Desktop/DSCapstone/DSCapstoneData')

df <- read.csv('allAlumsUpdated.csv')
#View(df)

##Getting out the information needed for variables to see the majors of alums 
##*Note: This did raise some concerns because of the blurred line between Social Science and Humanities*

##STEM
stem <- c(grep('Math', df$EducationHistory), grep('Mathematics', df$EducationHistory), grep('Astronomy', df$EducationHistory), grep('Biology', df$EducationHistory), grep('Computer Science', df$EducationHistory),grep('Statistics', df$EducationHistory),grep('Neuroscience', df$EducationHistory),grep('Chemistry', df$EducationHistory),grep('Environmental Studies', df$EducationHistory),grep('Geosciences', df$EducationHistory),grep('Physics', df$EducationHistory),grep('Biochemistry', df$EducationHistory),grep('Chemical Physics', df$EducationHistory),grep('Health and Society', df$EducationHistory),grep('Data Science', df$EducationHistory))
unique(stem)
df$STEM <- 0
df$STEM[stem] <- 1
table(df$STEM)

##Social Sciences
socials <- c(grep('Economics', df$EducationHistory), grep('Political Science', df$EducationHistory), grep('Anthropology', df$EducationHistory), grep('International Relations', df$EducationHistory), grep('Sociology', df$EducationHistory),grep('Psychology', df$EducationHistory),grep('History', df$EducationHistory),grep('Education', df$EducationHistory),grep('Cognitive & Linguistic Science', df$EducationHistory),grep('Cognitive and Linguistic Science', df$EducationHistory))
unique(socials)
df$SocialScience <- 0
df$SocialScience[socials] <- 1
table(df$SocialScience)

##Humanities
humanities <- c(grep('English', df$EducationHistory), grep('Africana Studies', df$EducationHistory), grep('Art,', df$EducationHistory), grep('Art ', df$EducationHistory), grep('American Studies', df$EducationHistory), grep('Classical Studies', df$EducationHistory),grep('East Asian Languages and Cultures', df$EducationHistory),grep('French', df$EducationHistory),grep('German', df$EducationHistory),grep('Italian', df$EducationHistory),grep('Music', df$EducationHistory),grep('Philosophy', df$EducationHistory),grep('Religion', df$EducationHistory),grep('Russian', df$EducationHistory),grep('Spanish', df$EducationHistory),grep('Portuguese', df$EducationHistory),grep('Women and Gender Studies', df$EducationHistory))
unique(humanities)
df$Humanities <- 0
df$Humanities[humanities] <- 1
table(df$Humanities)

##Save these new variables into a csv
View(df)
#write.csv(df,"C:\\Users\\brianavigil\\Desktop\\DSCapstone\\DSCapstoneData\\Alums&Majors.csv", row.names = FALSE)

#Plotting the age of alums
#Histograms
hist(df$ApproxAge, main = "Age Range of All Alums", xlab = "Age", ylab = "Frequency")
#Plot for the ages of all FG alums
hist(df$ApproxAge[df$FirstGen == 1])
#Plot for the ages of all non-FG alums
hist(df$ApproxAge[df$FirstGen == 0])

#Boxplot
boxplot(ApproxAge~FirstGen,data=df, main="Approximate Age of Alums by First-Gen Status", col = c("deeppink", "darkorchid"), names.arg = c("non First-Gen", "First-Gen"))

##Show that you need to subset for 65 and below because of differences in what fg means over the decades

##Subset for people who are 65 and below
df2 <- df[which(df$ApproxAge < 65), ]
attach(df2)

#run all the same tests with new df to reevaluate
##Proportion of FG & NFg alums who have masters
mfg <- table(Masters, FirstGen)
total <- table(FirstGen)
mfg[1, ] <- mfg[1,]/total
mfg[2,] <- mfg[2,]/total
mfg
mfg <- mfg[2:1,]
barplot(mfg, xlab = "First-Gen Status", ylab = "Proportion", col=c("dodgerblue1", "darkorange"), legend = c("Masters", "No Masters"), names.arg = c("Not First-Gen", "First-Gen"), main = "Alums with Masters Degrees and First-Gen Status" )
#Put in labels on the x-axis and y, and the legend (masters not masters)

#same thing as above with phd
pfg <- table(PhD, FirstGen)
total1 <- table(FirstGen)
pfg[1, ] <- pfg[1,]/total1
pfg[2,] <- pfg[2,]/total1
pfg
pfg <- pfg[2:1,]
barplot(pfg, xlab = "First-Gen Status", ylab = "Proportion", col=c("deeppink1", "darkorchid1"), legend = c("PhD", "No PhD"), names.arg = c("Not First-Gen", "First-Gen"), main = "Alums with PhD's and First-Gen Status" )

#for each subgroup based on major, make barplots that show the breakdown of first gen people in each subgroup

stemfg <- table(STEM, FirstGen)
total <- table(FirstGen)
stemfg[1, ] <- stemfg[1,]/total
stemfg[2,] <- stemfg[2,]/total
stemfg
stemfg <- stemfg[2:1,]
barplot(stemfg, xlab = "First-Gen Status", ylab = "Proportion", col=c("cornflowerblue", "coral1"), legend = c("Stem", "Not Stem"), names.arg = c("Not First-Gen", "First-Gen"), main = "STEM Alums First-Gen Status")

ssfg <- table(SocialScience, FirstGen)
total <- table(FirstGen)
ssfg[1, ] <- ssfg[1,]/total
ssfg[2,] <- ssfg[2,]/total
ssfg
ssfg <- ssfg[2:1,]
barplot(ssfg, col=c("darkmagenta", "chartreuse1"), xlab = "First-Gen Status", ylab = "Proportion", legend = c("social science", "Not social science"), names.arg = c("Not First-Gen", "First-Gen"), main = "Social Science Alums First-Gen Status")

hfg <- table(Humanities, FirstGen)
total <- table(FirstGen)
hfg[1, ] <- hfg[1,]/total
hfg[2,] <- hfg[2,]/total
hfg
hfg <- hfg[2:1,]
barplot(hfg, xlab = "First-Gen Status", ylab = "Proportion", col=c("darkseagreen1", "darkslategray2"), legend = c("humanities", "Not humanities"), names.arg = c("Not First-Gen", "First-Gen"), main = "Humanities Alums First-Gen Status")

#age show how these rates change over time. Proportion of those who get degrees changes with age
#do it by decade,
#Masters
mspline2 <- smooth.spline(ApproxAge[FirstGen == 0], Masters[FirstGen == 0])
plot(mspline2, type = "o", col = "blue", main = "Percentage of Degrees Earned by Age", ylab = "Percentage of Degrees Earned", xlab = "Age of Alum")
mspline1 <- smooth.spline(ApproxAge[FirstGen == 1], Masters[FirstGen == 1])
points(mspline1, type = "l", col = "blue")

#PhD
pspline1 <- smooth.spline(ApproxAge[FirstGen == 1], PhD[FirstGen == 1])
points(pspline1, type = "l", col = "purple")

pspline2 <- smooth.spline(ApproxAge[FirstGen == 0], PhD[FirstGen == 0])
points(pspline2, type = "o", col = "purple")
#legend(21.5, 0.6, legend=c("First Gen Masters", "non First Gen Masters", "First Gen PhD", "non First Gen PhD"), col = c("blue", "blue", "purple", "purple"))

#Now breakdown the age by discipline

##Masters
#STEM
mastersforStem1 <- smooth.spline(ApproxAge[FirstGen == 1 & STEM == 1], Masters[FirstGen == 1 & STEM == 1])
plot (mastersforStem1, type = "o", col = "orange", xlab = "Age of Alums", ylab = "Percentage of Masters Degrees Earned", main = "Percentage of Masters Degrees Earned in Respective Discipline with Age of Alum and First Gen Status")
mastersforStem2 <- smooth.spline(ApproxAge[FirstGen == 0 & STEM == 1], Masters[FirstGen == 0 & STEM == 1])
points (mastersforStem2, type = "l", col = "orange")

#Social Science
mastersforSS1 <- smooth.spline(ApproxAge[FirstGen == 1 & SocialScience == 1], Masters[FirstGen == 1 & SocialScience== 1])
points(mastersforSS1, type = "o", col = "green")
mastersforSS2 <- smooth.spline(ApproxAge[FirstGen == 0 & SocialScience== 1], Masters[FirstGen == 0 & SocialScience == 1])
points (mastersforSS2, type = "l", col = "green")

#Humanities
mastersforHumanities1 <- smooth.spline(ApproxAge[FirstGen == 1 & Humanities == 1], Masters[FirstGen == 1 & Humanities == 1])
points(mastersforHumanities1, type = "o", col = "purple")
mastersforHumanities2 <- smooth.spline(ApproxAge[FirstGen == 0 & Humanities== 1], Masters[FirstGen == 0 & Humanities == 1])
points (mastersforHumanities2, type = "l", col = "purple")

##PhD
#STEM
phdforStem1 <- smooth.spline(ApproxAge[FirstGen == 1 & STEM == 1], PhD[FirstGen == 1 & STEM == 1])
plot (phdforStem1, type = "o", col = "orange" , xlab = "Age of Alums", ylab = "Percentage of PhDs Earned", main = "Percentage of PhDs Earned in Respective Discipline with Age of Alum and First Gen Status")
phdforStem2 <- smooth.spline(ApproxAge[FirstGen == 0 & STEM == 1], PhD[FirstGen == 0 & STEM == 1])
points (phdforStem2, type = "l", col = "orange")

#Social Science
phdforSS1 <- smooth.spline(ApproxAge[FirstGen == 1 & SocialScience == 1], PhD[FirstGen == 1 & SocialScience== 1])
points(phdforSS1, type = "o", col = "green")
phdforSS2 <- smooth.spline(ApproxAge[FirstGen == 0 & SocialScience== 1], PhD[FirstGen == 0 & SocialScience == 1])
points (phdforSS2, type = "l", col = "green")

#Humanities
phdforHumanities1 <- smooth.spline(ApproxAge[FirstGen == 1 & Humanities == 1], PhD[FirstGen == 1 & Humanities == 1])
points(phdforHumanities1, type = "o", col = "purple")
phdforHumanities2 <- smooth.spline(ApproxAge[FirstGen == 0 & Humanities== 1], PhD[FirstGen == 0 & Humanities == 1])
points (phdforHumanities2, type = "l", col = "purple")


#logistic models on fg, stem, age where one is with masters or phd
#see the interactions in one model and compare with no interactions: see the aic and the bic
#do this for masters and phd
mastersModel1 <- glm(Masters ~ FirstGen + STEM + SocialScience + Humanities + ApproxAge,family=binomial(link='logit'),data=df2)
AIC(mastersModel1)
#1708.381
BIC(mastersModel1)
#1739.812

mastersModel2 <- glm(Masters ~ FirstGen + ApproxAge,family=binomial(link='logit'),data=df2)
AIC(mastersModel2)
#1735.809
BIC(mastersModel2)
#1751.525

#this next model has the interactions for first gen and age variables
mastersModel3 <- glm(Masters ~ FirstGen + STEM + SocialScience + Humanities + ApproxAge + FirstGen:ApproxAge,family=binomial(link='logit'),data=df2)
AIC(mastersModel3)
#1710.32
BIC(mastersModel3)
#1746.989

#see the interactions between age and other disciplines 
mastersModel4 <- glm(Masters ~ FirstGen + STEM + SocialScience + Humanities + ApproxAge + STEM:ApproxAge,family=binomial(link='logit'),data=df2)
AIC(mastersModel4)
#1709.909
BIC(mastersModel4)
#1746.578

mastersModel5 <- glm(Masters ~ FirstGen + STEM + SocialScience + Humanities + ApproxAge + SocialScience:ApproxAge,family=binomial(link='logit'),data=df2)
AIC(mastersModel5)
#1707.881
BIC(mastersModel5)
#1744.551

mastersModel6 <- glm(Masters ~ FirstGen + STEM + SocialScience + Humanities + ApproxAge + Humanities:ApproxAge,family=binomial(link='logit'),data=df2)
AIC(mastersModel6)
#1707.652
BIC(mastersModel6)
#1744.321

#Make model summary to show output
install.packages('modelsummary')
library(modelsummary)

mastersModels <- list()
mastersModels[['Masters Degree - all predictors']] <- mastersModel1
mastersModels[['Masters Degree - no disciplines']] <- mastersModel2
mastersModels[['Masters Degree - interactions for first gen and age variables']] <- mastersModel3
mastersModels[['Masters Degree - interactions between age and STEM']] <- mastersModel4
mastersModels[['Masters Degree - interactions between age and Social Science']] <- mastersModel5
mastersModels[['Masters Degree - interactions between age and Humanities']] <- mastersModel6

modelsummary(mastersModels, stars = TRUE)

#do the same for phd
phdModel1 <- glm(PhD ~ FirstGen + STEM + SocialScience + Humanities + ApproxAge,family=binomial(link='logit'),data=df2)
summary(phdModel1)
AIC(phdModel1)
#618.7552
BIC(phdModel1)
#650.1862

phdModel2 <- glm(PhD ~ FirstGen + ApproxAge,family=binomial(link='logit'),data=df2)
AIC(phdModel2)
#876.6264
BIC(phdModel2)
#892.3419

#this next model has the interactions for first gen and age variables
phdModel3 <- glm(PhD ~ FirstGen + STEM + SocialScience + Humanities + ApproxAge + FirstGen:ApproxAge,family=binomial(link='logit'),data=df2)
AIC(phdModel3)
#617.4996
BIC(phdModel3)
#654.1691

#see the interactions between age and other disciplines 
phdModel4 <- glm(PhD ~ FirstGen + STEM + SocialScience + Humanities + ApproxAge + STEM:ApproxAge,family=binomial(link='logit'),data=df2)
AIC(phdModel4)
#618.5491
BIC(phdModel4)
#655.2186

phdModel5 <- glm(PhD ~ FirstGen + STEM + SocialScience + Humanities + ApproxAge + SocialScience:ApproxAge,family=binomial(link='logit'),data=df2)
AIC(phdModel5)
#620.7239
BIC(phdModel5)
#657.3934

phdModel6 <- glm(PhD ~ FirstGen + STEM + SocialScience + Humanities + ApproxAge + Humanities:ApproxAge,family=binomial(link='logit'),data=df2)
AIC(phdModel6)
#620.7552
BIC(phdModel6)
#657.4247

phdModels <- list()
phdModels[['PhD Degree - all predictors']] <- phdModel1
phdModels[['PhD Degree - no disciplines']] <- phdModel2
phdModels[['PhD Degree - interactions for first gen and age variables']] <- phdModel3
phdModels[['PhD Degree - interactions between age and STEM']] <- phdModel4
phdModels[['PhD Degree - interactions between age and Social Science']] <- phdModel5
phdModels[['PhD Degree - interactions between age and Humanities']] <- phdModel6

modelsummary(phdModels, stars = TRUE)

#Results: Some of the results that were found from these tests include that the 
##Masters model with all predictors and interaction between Humanities and Age has the lowest AIC 
#and the Masters model with the lowest BIC is the model with all predictors and no interactions
## In addition to that, the phd model with the interactions for first gen and age variables has the 
#lowest AIC, while the model with no interactions and all of the predictor variables is the lowest for BIC. 
