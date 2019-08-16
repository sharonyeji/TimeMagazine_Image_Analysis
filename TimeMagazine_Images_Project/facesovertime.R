install.packages("imager")
install.packages("jpeg")
require(imager)
require(jpeg)
library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
################### Image Processing #############

load("C:/Users/Sharon/Desktop/Chapman Fall 2018/Independent Study/Time_Magazine_faces/resized_SaveFace/timeface_meta.RData")

images = read.csv("C:/Users/Sharon/Desktop/Chapman Fall 2018/Independent Study/Time_Magazine_faces/resized_SaveFace/test.csv")
images$image = as.character(images$FileName)
data = merge(images,dfbest, by='image')
data = within(data, rm('FileName','ID','year','month','day','page','photo','angle'))
data$Group[data$Year==1927] = '1920s'
data$Group[data$Year==1934] = '1930s'
data$Group[data$Year==1942] = '1940s'
data$Group[data$Year==1958] = '1950s'
data$Group[data$Year<1970 & data$Year >=1960] = '1960s'
data$Group[data$Year<1980 & data$Year >=1970] = '1970s'
data$Group[data$Year<1990 & data$Year >=1980] = '1980s'
data$Group[data$Year==1991] = '1990s'
data$Group[data$Year==2001] = '2000s'
data$Group[data$Year==2012] = '2010s'

#histogram of data
hist(data$Year, breaks = 30)

#ggplot gender
gender = table(data$Group,data$gender)
gender.long = melt(gender, value.name = 'nGender')
gender.long$Year = gender.long$Var1
gender.long$Gender = gender.long$Var2
gender.long = within(gender.long, rm('Var1','Var2'))
ggplot(gender.long, aes(x=Year, y=nGender, fill=Gender)) +
  geom_bar(stat='identity')

#ggplot gender percent
gender = as.data.frame.matrix(table(data$Group,data$gender))
Year = rownames(gender)
gender = cbind(Year,gender)
gender$per_f = gender$female/(gender$female + gender$male + gender$unknown)
gender$per_m = gender$male/(gender$female + gender$male + gender$unknown)
gender$per_u = gender$unknown/(gender$female + gender$male + gender$unknown)
gender_percent = within(gender, rm('female','male','unknown'))
gender_percent.long = melt(gender_percent, id.var = 'Year', variable.name = 'Gender', value.name = 'pGender')
ggplot(gender_percent.long, aes(x=Year, y=pGender, fill=Gender)) +
  geom_bar(stat='identity')
gender_percent.long = gender_percent.long[gender_percent.long$Gender == 'per_f',]
ggplot(gender_percent.long, aes(x=Year, y=pGender)) + geom_line(aes(group = Gender)) + geom_point(aes(group = Gender))


#ggplot smile percent
smile = as.data.frame.matrix(table(data$Group,data$smile))
Year = rownames(smile)
smile = cbind(Year,smile)
smile$per_0 = smile$`0`/(smile$`0` + smile$`1`)
smile$per_1 = smile$`1`/(smile$`0` + smile$`1`)
smile_percent = within(smile, rm('0','1'))
smile_percent.long = melt(smile_percent, id.var = 'Year', variable.name = 'Smile', value.name = 'pSmile')
ggplot(smile_percent.long, aes(x=Year, y=pSmile, fill=Smile)) +
  geom_bar(stat='identity')
smile_percent.long = smile_percent.long[smile_percent.long$Smile == 'per_1',]
ggplot(smile_percent.long, aes(x=Year, y=pSmile)) + geom_line(aes(group = Smile)) + geom_point(aes(group = Smile))

#Gender & Smile
male <- c(0,.185,.318,.344,.365,.388,.373,.422,.25,.541)
female <- c(0.6,.429,.33,.602,.667,.525,.447,.562,.417)
gender_smile <- as.data.frame(cbind(Year,female,male))
gender_smile.long = melt(gender_smile, id.var = 'Year', variable.name = 'Gender_Smile', value.name = 'pGender_Smile')
ggplot(gender_smile.long, aes(x=Year, y=pGender_Smile)) + geom_line(aes(group = Gender_Smile,col=Gender_Smile)) + geom_point(aes(group = Gender_Smile,col=Gender_Smile))





data_ad = data[data$category == 'ad',]

#ggplot gender percent
gender = as.data.frame.matrix(table(data_ad$Group,data_ad$gender))
Year = rownames(gender)
gender = cbind(Year,gender)
gender$per_f = gender$female/(gender$female + gender$male + gender$unknown)
gender$per_m = gender$male/(gender$female + gender$male + gender$unknown)
gender$per_u = gender$unknown/(gender$female + gender$male + gender$unknown)
gender_percent = within(gender, rm('female','male','unknown'))
gender_percent.long = melt(gender_percent, id.var = 'Year', variable.name = 'Gender', value.name = 'pGender')
ggplot(gender_percent.long, aes(x=Year, y=pGender, fill=Gender)) +
  geom_bar(stat='identity')
gender_percent.long = gender_percent.long[gender_percent.long$Gender == 'per_f',]
ggplot(gender_percent.long, aes(x=Year, y=pGender)) + geom_line(aes(group = Gender)) + geom_point(aes(group = Gender))

#ggplot smile percent
smile = as.data.frame.matrix(table(data_ad$Group,data_ad$smile))
Year = rownames(smile)
smile = cbind(Year,smile)
smile$per_0 = smile$`0`/(smile$`0` + smile$`1`)
smile$per_1 = smile$`1`/(smile$`0` + smile$`1`)
smile_percent = within(smile, rm('0','1'))
smile_percent.long = melt(smile_percent, id.var = 'Year', variable.name = 'Smile', value.name = 'pSmile')
ggplot(smile_percent.long, aes(x=Year, y=pSmile, fill=Smile)) +
  geom_bar(stat='identity')
smile_percent.long = smile_percent.long[smile_percent.long$Smile == 'per_1',]
ggplot(smile_percent.long, aes(x=Year, y=pSmile)) + geom_line(aes(group = Smile)) + geom_point(aes(group = Smile))


#Gender_Smile
Year <- c('1920s','1930s','1940s','1950s','1960s','1970s','1980s','1990s','2000s','2010s')
male <- c(0,.286,.571,.571,.533,.716,.575,.667,.429,.455)
female <- c(0,.5,0,.5,.6,.816,.643,.667,.8,.667)
gender_smile <- as.data.frame(cbind(Year,female,male))
gender_smile.long = melt(gender_smile, id.var = 'Year', variable.name = 'Gender_Smile', value.name = 'pGender_Smile')
ggplot(gender_smile.long, aes(x=Year, y=pGender_Smile)) + geom_line(aes(group = Gender_Smile,col=Gender_Smile)) + geom_point(aes(group = Gender_Smile,col=Gender_Smile))






data_feature = data[data$category == 'feature',]

#ggplot gender percent
gender = as.data.frame.matrix(table(data_feature$Group,data_feature$gender))
Year = rownames(gender)
gender = cbind(Year,gender)
gender$per_f = gender$female/(gender$female + gender$male + gender$unknown)
gender$per_m = gender$male/(gender$female + gender$male + gender$unknown)
gender$per_u = gender$unknown/(gender$female + gender$male + gender$unknown)
gender_percent = within(gender, rm('female','male','unknown'))
gender_percent.long = melt(gender_percent, id.var = 'Year', variable.name = 'Gender', value.name = 'pGender')
ggplot(gender_percent.long, aes(x=Year, y=pGender, fill=Gender)) +
  geom_bar(stat='identity')
gender_percent.long = gender_percent.long[gender_percent.long$Gender == 'per_f',]
ggplot(gender_percent.long, aes(x=Year, y=pGender)) + geom_line(aes(group = Gender)) + geom_point(aes(group = Gender))

#ggplot smile percent
smile = as.data.frame.matrix(table(data_feature$Group,data_feature$smile))
Year = rownames(smile)
smile = cbind(Year,smile)
smile$per_0 = smile$`0`/(smile$`0` + smile$`1`)
smile$per_1 = smile$`1`/(smile$`0` + smile$`1`)
smile_percent = within(smile, rm('0','1'))
smile_percent.long = melt(smile_percent, id.var = 'Year', variable.name = 'Smile', value.name = 'pSmile')
ggplot(smile_percent.long, aes(x=Year, y=pSmile, fill=Smile)) +
  geom_bar(stat='identity')
smile_percent.long = smile_percent.long[smile_percent.long$Smile == 'per_1',]
ggplot(smile_percent.long, aes(x=Year, y=pSmile)) + geom_line(aes(group = Smile)) + geom_point(aes(group = Smile))

#Gender & Smile
male <- c(0,.176,.154,.304,.325,.295,.332,.391,.238,.591)
female <- c(0,.667,.4,.33,.607,.583,.453,.407,.40,.353)
gender_smile <- as.data.frame(cbind(Year,female,male))
gender_smile.long = melt(gender_smile, id.var = 'Year', variable.name = 'Gender_Smile', value.name = 'pGender_Smile')
ggplot(gender_smile.long, aes(x=Year, y=pGender_Smile)) + geom_line(aes(group = Gender_Smile,col=Gender_Smile)) + geom_point(aes(group = Gender_Smile,col=Gender_Smile))
