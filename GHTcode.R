#####MATCH GOOGLE HEALTH TRENDS - EPIDEMIOLOGICAL DATA######

# --------------------------------------------------------
#'  Author: Daniel Romero-Alvarez, Alex ###, Qays #####
#                
#'  Script to:
#'  Match google health trends data with weekly case incidence for any state
#'  Example starts with Brazilian data 

# --------------------------------------------------------


#####LIBRARIES#####
library (plyr) #transforming lists on dataframes
library(datetimeutils) #changing_dates

#####IMPORTANT DETAILS######

#' This code needs google health trends data and also 
#' epidemiological data that matches the google health trends week 
#' data division. You can follow the code weekly_BR_cases in order 
#' to arrange databases. 
#' All the google health trends data has been adquired and added for 
#' countries and states in the corresponding dropbox folder. 
#' 
#' 
#' We need to collect the following data: 
#' - Number of useful terms. 
#' - Adjusted R squared per model.  
#' - Plot of covid-19 incidence + regression line 
#' - Date of first case per model... 
#' 
#Formatting daily case data
library (plyr) #transforming lists on dataframes
library(datetimeutils) #changing_dates

#FOR TRANSFORMING DAILY CASE DATA TO WEEKLY CASE DATA
BR = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/data 1-28-2021/Central African Republic.csv', header = T)


BR1 <- data.frame('date' = BR$date,
                  'Cumulative Cases' = BR$Cumulative.Cases,
                  'New Cases' = BR$New.Cases)
pop1 <- data.frame('pop' = BR$pop)

BR1$date = gsub (BR1$date, pattern ='/',replacement = '-')
BR1$date = as.Date(BR1$date,format="%m-%d-%Y")

BR1$date = as.Date(BR1$date)


BR1 = BR1[BR1$date > '2020-02-01'& BR1$date < '2021-01-25',] 
tail(BR1)
#subset by important columns: 
names (BR1)
BR1 = BR1[,c(1, 2, 3)]



#change name of BR1 column name
colnames(BR1)[colnames(BR1) == "date"] <- "data"

#Create a new dataframe based on weekly cumulative cases

BR_comp = list() #open empty list 


vct= seq (1,length(BR1$data), 7)

for (i in seq (1,length(BR1$data), 7)){ #loop each 7 days till the end of the DF
  rr = BR1[(BR1$data > BR1$data[i] & BR1$data <= BR1$data[i+7]),] #subset DF above first row 
  cs = colSums(rr[,2:3]) #sum cases for this subset
  df =  t(as.data.frame (cs)) #transform in dataframe and transpose
  cs2 = cbind (data = as.character(BR1$data[i+7]), df) #add the name to the dataframe
  BR_comp[[length(BR_comp)+1]] = cs2 #add to the list 
}



BR_rw = ldply(BR_comp, data.frame) #transform list to dataframe
BR_week = rbind (BR1[1,1:3], BR_rw) #add first row previously ignored 
BR_week = BR_week[-(length(BR_week$data)),] #eliminate last row with NA if needed
BR_week$data = as.Date(BR_week$data) #define first column as class dates 
#add population column
BR_week$pop=rep(pop1[1,"pop"],length(BR_week$New.Cases))


write.csv (BR_week, 'C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/Weekly 1-28-2021/SouthAfrica1.csv', row.names = F)

#formatting GHT data
AL1 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/GHTdata 1-28-2021/ZA_ZA.csv')
AL1$date = as.Date(AL1$date) #convert columns to actual dates
AL1 = AL1[AL1$date > '2020-02-01'& AL1$date < '2021-01-29',] 

#usful words: 
AL2 = AL1[,colSums(AL1 !=0) >0] #this selects those columns that have information 
names (AL2)


#important words: 

def_tms = AL2[,2:length(AL2)] #only terms dataset

def_tms1 <- data.frame('COVID19' = def_tms$COVID19, 
                       'coronavirus' = def_tms$coronavirus,
                       'coronavirus symptoms' = def_tms$coronavirus.symptoms, 
                       'pandemic' = def_tms$pandemic)
average_tm = apply(def_tms1, 2, FUN = mean)

median_tm = apply(def_tms1, 2, FUN= median) #obtaining a vector with the medians
bp = names(median_tm)#names for boxplot

#reading data already processed by week
AL_br1 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/Weekly 1-28-2021/SouthAfrica1.csv')

# #Objects with information of the week of the first case 
ot = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/data 1-28-2021/SouthAfrica.csv')
ot <- data.frame('date' = ot$date,
                 'Cumulative Cases' = ot$Cumulative.Cases,
                 'New Cases' = ot$New.Cases)
ot$date = gsub (ot$date, pattern ='/',replacement = '-')
ot$date = as.Date(ot$date,format="%m-%d-%Y")
ot$date = as.Date(ot$date) #defining as dates

f_c = ot$New.Cases[ot$New.Cases>0][1] #number of first cases in the original state database
f_c1 = AL_br1$New.Cases[AL_br1$New.Cases>0][1] #number of first cases in the weekly state database

ff_1 = as.Date(ot[which(ot$New.Cases == f_c)[1], 1]) #obtain the date through index and subsetting table 1, 8 represents column with dates in this table
ff_2 = as.Date(AL_br1[which(AL_br1$New.Cases == f_c1)[1], 1]) #obtain the date through index and subsetting table 2, 4 represents column with dates in this table

#calculate incidence per state adding a new column to the brazilian datafram: inci: 
AL_br1$inci = AL_br1$New.Cases/AL_br1$pop

#regression using all available terms per state: 

bt = paste('AL2', '$', bp , sep = '') #VECTOR SUBSETTING THE COLUMNS ACCORDING TO THE MENTIONED WORDS


AL1_form1 = formula(c(paste ('AL_br1$inci~'), 
                      paste(bt, collapse = ' + '))) #formula for the regression

AL1_model = lm(AL1_form1) #regression 

AL1_rq = summary(AL1_model)$adj.r.squared #collect adj. r squared


tiff("South Africa.tiff", units="in", width=6, height=5, res=300)

#plot incidence
plot (AL_br1$inci, main = bquote(AF ~ - ~ South ~ Africa ~ Republic ~ Adjusted ~ R^2 == .(round(AL1_rq,4))),
      ylab = 'Weekly incidence', xlab = '', xaxt ='n',
      type = 'l', pch = 19, cex = 0.3, cex.axis = 1.3, cex.main = 1.5, cex.lab = 1.4)
#regression line 
lines (predict (AL1_model), col = 'blue', type ='b', pch = 19, cex = 0.3) 
#x-axis

axis (1, at=c(1,5,9,14,18,23,27,31,36,40,45,49),
      labels = c('Feb.', 'Mar.', 'Apr.', 'May', 'Jun.','Jul.','Aug.','Sep.','Oct.','Nov.','Dec.','Jan'), cex.axis = 1, las=2)

text (30, quantile(AL_br1$inci, 0.95),
      labels = paste ('First case = ', '\n\ ', ff_1, sep = ''), 
      cex = 0.8, pos = '2', col= 'red', srt = 0)

#adding legend
legend('topleft', legend=c("Observed", "Model"),
       col=c("black", 'blue'), pch = c(NA,19,NA), lty = c(1, 1, 1), cex=0.8) 
dev.off()

####SUMMARY MODELS R2 plots#####
#READ THE BRAZILIAN CODE THAT IS FULLY COMMENTED, THE OTHER ONES ARE A REPETITION OF THIS ONE...

#BRAZIL: 
br = read.table ('./alex_qays_res1/GHTBrazilResultsFull.txt', sep = '\t', header = T) #reading the table, 

#' NOTICE: that you should make sure to 
#' eliminate weird characters such as Amapá, 
#' you should elimiante the accent...manually in the file...

head (br) #checking the first 6 lines 

plot (br$Adjusted.R2, ylab = 'Adj. R2', xaxt = 'n', xlab = '', main = 'Brazil', type = 'h') #creating the plot and vertical lines
points (br$Adjusted.R2, pch = 16) #adding the points
axis (1, at= (1:length(br$States)),labels = br$States, las= 2, cex.axis = 0.6) #adding the x axis with the names of the states or countries...
abline (h = 0.6, lty= 2) #adding the threshold line, in this case, 0.6!

#AFRICAN COUNTRIES deacreasing R2 plot: 
af = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/ght results 1-28-2021/r2plot.csv')
ac = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/R2 plot/r2sorted.csv')
af = data.frame(af)

head (af)

bp_countries = af[,1:2] #only countries and r2 values

bp2 = bp_countries[order(bp_countries$Adjusted.R2, decreasing = T),] #ordering the dataframe with the r2 values from high to low 
bp = row.names (bp2$ID) #obtaining the ordered names for the plot

y1 <- expression(Adjusted ~ R^2)
x1 <- expression(Adjusted ~ R^2 ~ Results)

tiff("r2plot.tiff", units="in", width=6, height=5, res=300)

par(mfrow=c(1,1))
par(mar=c(4,5,3,1)+.1)
plot (bp2$Adjusted.R2, ylab = y1, xaxt = 'n', xlab = 'African Countries', main = x1, type = 'h')
points (bp2$Adjusted.R2, pch = 16)
axis (1, at= (1:length(bp2$ID)),labels = bp2$ID, las= 2, cex.axis = 0.5)
abline (h = 0.6, lty= 2) 
dev.off()

#####VOLATILITY INDEX######

#Example with ALGERIA

#working directory 
setwd ('/Users/daniel/Documents/CORONAVIRUS/brazil')

#read data
alg = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/Weekly 1-28-2021/Zimbabwe.csv')

#I AM USING THE CASES FOR THIS EXAMPLE BUT YOU NEED TO PUT HERE THE INCIDENCE
alg$inci = alg$New.Cases/alg$pop


#normalization of the data
alg_mean = mean (alg$inci) #calculate the mean
alg_sd = sd(alg$inci) #calculate the standard deviation 

alg_norm = (alg$inci - alg_mean)/alg_sd #normalization for Algeria

#comparing histograms
dev.new()
par(mfrow = c(2,1))
hist(alg_norm) #normalized data
hist(alg$inci) #original data 

length (alg_norm) #total amount of rows 

#volatility index loop 
do2 = list () #empty list 
for (d in 1:26){ #loop to total-1 number of rows 
  t1 = abs(alg_norm[d]-alg_norm[d+1]) #absolute difference between 2 consecutive weeks 
  do2[[length(do2)+1]] = t1 #add to the list
}

do3 = unlist (do2) #total of 25 observations
alg_v = mean (do3) #calculate the mean which is the volatility score for that country

plot (alg_norm, main = paste ('Volatility = ', round (alg_v,3), sep = ''), type = 'l') #low volatility

#Example with CAMEROON

#working directory 
setwd ('/Users/daniel/Documents/CORONAVIRUS/brazil')

#read data
camer = read.csv ('./alex_qays_volatility/Weekly/Cameroon.csv')

#I AM USING THE CASES FOR THIS EXAMPLE BUT YOU NEED TO PUT HERE THE INCIDENCE

#normalization of the data
camer_mean = mean (camer$New.Cases) #calculate the mean
camer_sd = sd(camer$New.Cases) #calculate the standard deviation 

camer_norm = (camer$New.Cases - camer_mean)/camer_sd #normalization for camereria

#comparing histograms
dev.new()
par(mfrow = c(2,1))
hist(camer_norm) #normalized data
hist(camer$New.Cases) #original data 

length (camer_norm) #total amount of rows 

#volatility index loop 
do2 = list () #empty list 
for (d in 1:26){ #loop to total-1 number of rows 
  t1 = abs(camer_norm[d]-camer_norm[d+1]) #absolute difference between 2 consecutive weeks 
  do2[[length(do2)+1]] = t1 #add to the list
}

do3 = unlist (do2) #total of 25 observations
camer_v = mean (do3) #calculate the mean which is the volatility score for that country

plot (camer_norm, main = paste ('Volatility = ', round (camer_v,3), sep = ''), type = 'l') #high volatility

#Compare plot from Angola and Cameroon to see the difference between low and high volatility! 


# Collect the volatility score for each country and then create a regression in the form: 
# 
# r_squares ~ volatility scores
# 
# Record the new r_square


#####ELIMINATING NAs######
#this is used to create dataframes without NAs for regression analysis 
#fake dataframe 
a = sample (1:100, 50)
b = c(1:25, rep (NA, 25))
c = c (1:50)

#dataframe 
dff = data.frame (a, b, c)
dim(dff)

#eliminate rows with NAs
dff2 = dff[rowSums(is.na (dff))!=1, ] #If NAs are present in more than one column is better to use <1 instead of !=1 because a lot of them are going to be greater thatn 1... 
dim(dff2)


##############3 best/worst plots with model and regression line###################
#pickout the 6 countries by hand

AL1 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/GHTdata/TZ_TZ.csv')
AL2 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/GHTdata/TN_TN.csv')
AL3 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/GHTdata/BF_BF.csv')
AL4 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/GHTdata/SC_SC.csv')
AL5 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/GHTdata/BJ_BJ.csv')
AL6 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/GHTdata/NA_NA.csv')

AL1$date = as.Date(AL1$date) #convert columns to actual dates
AL1 = AL1[AL1$date > '2020-02-01'& AL1$date < '2020-08-03',] 

AL2$date = as.Date(AL2$date) #convert columns to actual dates
AL2 = AL2[AL2$date > '2020-02-01'& AL2$date < '2020-08-03',] 

AL3$date = as.Date(AL3$date) #convert columns to actual dates
AL3 = AL3[AL3$date > '2020-02-01'& AL3$date < '2020-08-03',] 

AL4$date = as.Date(AL4$date) #convert columns to actual dates
AL4 = AL4[AL4$date > '2020-02-01'& AL4$date < '2020-08-03',] 

AL5$date = as.Date(AL5$date) #convert columns to actual dates
AL5 = AL5[AL5$date > '2020-02-01'& AL5$date < '2020-08-03',] 

AL6$date = as.Date(AL6$date) #convert columns to actual dates
AL6 = AL6[AL6$date > '2020-02-01'& AL6$date < '2020-08-03',] 

#usful words: 
AL1 = AL1[,colSums(AL1 !=0) >0] #this selects those columns that have information 
AL2 = AL2[,colSums(AL2 !=0) >0]
AL3 = AL3[,colSums(AL3 !=0) >0] 
AL4 = AL4[,colSums(AL4 !=0) >0]  
AL5 = AL5[,colSums(AL5 !=0) >0]  
AL6 = AL6[,colSums(AL6 !=0) >0]  




#important words: 

def_tms1 = AL1[,2:length(AL1)] #only terms dataset
def_tms2 = AL2[,2:length(AL2)] 
def_tms3 = AL3[,2:length(AL3)] 
def_tms4 = AL4[,2:length(AL4)] 
def_tms5 = AL5[,2:length(AL5)] 
def_tms6 = AL6[,2:length(AL6)] 

#4 predefined terms
def_tms1 <- data.frame('COVID19' = def_tms1$COVID19, 
                       'coronavirus' = def_tms1$coronavirus,
                       'coronavirus symptoms' = def_tms1$coronavirus.symptoms, 
                       'pandemic' = def_tms1$pandemic)
def_tms2 <- data.frame('COVID19' = def_tms2$COVID19, 
                       'coronavirus' = def_tms2$coronavirus,
                       'coronavirus symptoms' = def_tms2$coronavirus.symptoms, 
                       'pandemic' = def_tms2$pandemic)
def_tms3 <- data.frame('COVID19' = def_tms3$COVID19, 
                       'coronavirus' = def_tms3$coronavirus,
                       'coronavirus symptoms' = def_tms3$coronavirus.symptoms, 
                       'pandemic' = def_tms3$pandemic)
def_tms4 <- data.frame('COVID19' = def_tms4$COVID19, 
                       'coronavirus' = def_tms4$coronavirus,
                       'coronavirus symptoms' = def_tms4$coronavirus.symptoms, 
                       'pandemic' = def_tms4$pandemic)
def_tms5 <- data.frame('COVID19' = def_tms5$COVID19, 
                       'coronavirus' = def_tms5$coronavirus,
                       'coronavirus symptoms' = def_tms5$coronavirus.symptoms, 
                       'pandemic' = def_tms5$pandemic)
def_tms6 <- data.frame('COVID19' = def_tms6$COVID19, 
                       'coronavirus' = def_tms6$coronavirus,
                       'coronavirus symptoms' = def_tms6$coronavirus.symptoms, 
                       'pandemic' = def_tms6$pandemic)

median_tm1 = apply(def_tms1, 2, FUN= median) #obtaining a vector with the medians
bp1 = names(median_tm1)#names for boxplot
bp1 = bp1[order(median_tm1, decreasing = T)] #order names highest to lowest

median_tm2 = apply(def_tms2, 2, FUN= median) 
bp2 = names(median_tm2)#names for boxplot
bp2 = bp2[order(median_tm2, decreasing = T)]

median_tm3 = apply(def_tms3, 2, FUN= median) 
bp3 = names(median_tm3)#names for boxplot
bp3 = bp3[order(median_tm3, decreasing = T)] 

median_tm4 = apply(def_tms4, 2, FUN= median)
bp4 = names(median_tm4)#names for boxplot
bp4 = bp4[order(median_tm4, decreasing = T)] 

median_tm5 = apply(def_tms5, 2, FUN= median) 
bp5 = names(median_tm5)#names for boxplot
bp5 = bp5[order(median_tm5, decreasing = T)] 

median_tm6 = apply(def_tms6, 2, FUN= median) 
bp6 = names(median_tm6)#names for boxplot
bp6 = bp6[order(median_tm6, decreasing = T)] 

#reading data already processed by week
AL_br1 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/Weekly/AF_Tanzania.csv')
AL_br2 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/Weekly/AF_Tunisia.csv')
AL_br3 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/Weekly/Burkina Faso.csv')
AL_br4 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/Weekly/AF_Seychelles.csv')
AL_br5 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/Weekly/Benin.csv')
AL_br6 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/Weekly/AF_Namibia.csv')

# #remove last two weeks of data for nigerian states
# n<-dim(AL2)[1]
# AL2<-AL2[1:(n-2),]

length(AL1$date) == length(AL_br1$data) #notice that the column with dates in the brazialian file is called data not date
length(AL2$date) == length(AL_br2$data)
length(AL3$date) == length(AL_br3$data)
length(AL4$date) == length(AL_br4$data)
length(AL5$date) == length(AL_br5$data)
length(AL6$date) == length(AL_br6$data)
#both dataframes have the same length, thus we can create plots

#defining date column 
AL_br1$data = as.Date (AL_br1$data)
AL_br2$data = as.Date (AL_br2$data)
AL_br3$data = as.Date (AL_br3$data)
AL_br4$data = as.Date (AL_br4$data)
AL_br5$data = as.Date (AL_br5$data)
AL_br6$data = as.Date (AL_br6$data)

#linear regresion

#Objects with information of the week of the first case 
ot1 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/data 8-9-2020/Tanzania.csv')
ot1 <- data.frame('date' = ot1$date,
                  'Cumulative Cases' = ot1$Cumulative.Cases,
                  'New Cases' = ot1$New.Cases)
ot1$date = gsub (ot1$date, pattern ='/',replacement = '-')
ot1$date = as.Date(ot1$date,format="%m-%d-%Y")
ot1$date = as.Date(ot1$date) #defining as dates

f_cc1 = ot1$New.Cases[ot1$New.Cases>0][1] #number of first cases in the original state database
f_c1 = AL_br1$New.Cases[AL_br1$New.Cases>0][1] #number of first cases in the weekly state database

ff_1 = as.Date(ot1[which(ot1$New.Cases == f_cc1)[1], 1]) #obtain the date through index and subsetting table 1, 8 represents column with dates in this table
ff__1 = as.Date(AL_br1[which(AL_br1$New.Cases == f_c1)[1], 1]) #obtain the date through index and subsetting table 2, 4 represents column with dates in this table
#
ot2 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/data 8-9-2020/Tunisia.csv')
ot2 <- data.frame('date' = ot2$date,
                  'Cumulative Cases' = ot2$Cumulative.Cases,
                  'New Cases' = ot2$New.Cases)
ot2$date = gsub (ot2$date, pattern ='/',replacement = '-')
ot2$date = as.Date(ot2$date,format="%m-%d-%Y")
ot2$date = as.Date(ot2$date) #defining as dates

f_cc2 = ot2$New.Cases[ot2$New.Cases>0][1] #number of first cases in the original state database
f_c2 = AL_br2$New.Cases[AL_br2$New.Cases>0][1] #number of first cases in the weekly state database

ff_2 = as.Date(ot2[which(ot2$New.Cases == f_cc2)[1], 1]) #obtain the date through index and subsetting table 1, 8 represents column with dates in this table
ff__2 = as.Date(AL_br2[which(AL_br2$New.Cases == f_c2)[1], 1]) #obtain the date through index and subsetting table 2, 4 represents column with dates in this table
#
ot3 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/data 8-9-2020/Burkina Faso.csv')
ot3 <- data.frame('date' = ot3$date,
                  'Cumulative Cases' = ot3$Cumulative.Cases,
                  'New Cases' = ot3$New.Cases)
#some of the dataframes already have proper dates
# ot3$date = gsub (ot3$date, pattern ='/',replacement = '-')
# ot3$date = as.Date(ot3$date,format="%m-%d-%Y")
# ot3$date = as.Date(ot3$date) #defining as dates

f_cc3 = ot3$New.Cases[ot3$New.Cases>0][1] #number of first cases in the original state database
f_c3 = AL_br3$New.Cases[AL_br3$New.Cases>0][1] #number of first cases in the weekly state database

ff_3 = as.Date(ot3[which(ot3$New.Cases == f_cc3)[1], 1]) #obtain the date through index and subsetting table 1, 8 represents column with dates in this table
ff__3 = as.Date(AL_br3[which(AL_br3$New.Cases == f_c3)[1], 1]) #obtain the date through index and subsetting table 2, 4 represents column with dates in this table
#
ot4 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/data 8-9-2020/Seychelles.csv')
ot4 <- data.frame('date' = ot4$date,
                  'Cumulative Cases' = ot4$Cumulative.Cases,
                  'New Cases' = ot4$New.Cases)
ot4$date = gsub (ot4$date, pattern ='/',replacement = '-')
ot4$date = as.Date(ot4$date,format="%m-%d-%Y")
ot4$date = as.Date(ot4$date) #defining as dates

f_cc4 = ot4$New.Cases[ot4$New.Cases>0][1] #number of first cases in the original state database
f_c4 = AL_br4$New.Cases[AL_br4$New.Cases>0][1] #number of first cases in the weekly state database

ff_4 = as.Date(ot1[which(ot1$New.Cases == f_cc4)[1], 1]) #obtain the date through index and subsetting table 1, 8 represents column with dates in this table
ff__4 = as.Date(AL_br1[which(AL_br1$New.Cases == f_c4)[1], 1]) #obtain the date through index and subsetting table 2, 4 represents column with dates in this table
#
ot5 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/data 8-9-2020/Benin.csv')
ot5 <- data.frame('date' = ot5$date,
                  'Cumulative Cases' = ot5$Cumulative.Cases,
                  'New Cases' = ot5$New.Cases)
ot5$date = gsub (ot5$date, pattern ='/',replacement = '-')
ot5$date = as.Date(ot5$date,format="%m-%d-%Y")
ot5$date = as.Date(ot5$date) #defining as dates

f_cc5 = ot5$New.Cases[ot5$New.Cases>0][1] #number of first cases in the original state database
f_c5 = AL_br5$New.Cases[AL_br5$New.Cases>0][1] #number of first cases in the weekly state database

ff_5 = as.Date(ot5[which(ot5$New.Cases == f_cc5)[1], 1]) #obtain the date through index and subsetting table 1, 8 represents column with dates in this table
ff__5 = as.Date(AL_br5[which(AL_br5$New.Cases == f_c5)[1], 1]) #obtain the date through index and subsetting table 2, 4 represents column with dates in this table
#
ot6 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/data 8-9-2020/Namibia.csv')
ot6 <- data.frame('date' = ot6$date,
                  'Cumulative Cases' = ot6$Cumulative.Cases,
                  'New Cases' = ot6$New.Cases)
ot6$date = gsub (ot6$date, pattern ='/',replacement = '-')
ot6$date = as.Date(ot6$date,format="%m-%d-%Y")
ot6$date = as.Date(ot6$date) #defining as dates

f_cc6 = ot6$New.Cases[ot6$New.Cases>0][1] #number of first cases in the original state database
f_c6 = AL_br6$New.Cases[AL_br6$New.Cases>0][1] #number of first cases in the weekly state database

ff_6 = as.Date(ot1[which(ot6$New.Cases == f_cc6)[1], 1]) #obtain the date through index and subsetting table 1, 8 represents column with dates in this table
ff__6 = as.Date(AL_br6[which(AL_br6$New.Cases == f_c6)[1], 1]) #obtain the date through index and subsetting table 2, 4 represents column with dates in this table
#
#calculate incidence per state adding a new column to the brazilian datafram: inci: 
AL_br1$inci = AL_br1$New.Cases/AL_br1$pop
AL_br2$inci = AL_br2$New.Cases/AL_br2$pop
AL_br3$inci = AL_br3$New.Cases/AL_br3$pop
AL_br4$inci = AL_br4$New.Cases/AL_br4$pop
AL_br5$inci = AL_br5$New.Cases/AL_br5$pop
AL_br6$inci = AL_br6$New.Cases/AL_br6$pop

bt1 = paste('AL1', '$', bp1 , sep = '') #VECTOR SUBSETTING THE COLUMNS ACCORDING TO THE MENTIONED WORDS
bt2 = paste('AL2', '$', bp2 , sep = '')
bt3 = paste('AL3', '$', bp3 , sep = '')
bt4 = paste('AL4', '$', bp4 , sep = '') 
bt5 = paste('AL5', '$', bp5 , sep = '') 
bt6 = paste('AL6', '$', bp6 , sep = '') 


AL1_form1 = formula(c(paste ('AL_br1$inci~'), 
                      paste(bt1, collapse = ' + '))) #formula for the regression
AL1_form2 = formula(c(paste ('AL_br2$inci~'), 
                      paste(bt2, collapse = ' + '))) 
AL1_form3 = formula(c(paste ('AL_br3$inci~'), 
                      paste(bt3, collapse = ' + '))) 
AL1_form4 = formula(c(paste ('AL_br4$inci~'), 
                      paste(bt4, collapse = ' + '))) 
AL1_form5 = formula(c(paste ('AL_br5$inci~'), 
                      paste(bt5, collapse = ' + '))) 
AL1_form6 = formula(c(paste ('AL_br6$inci~'), 
                      paste(bt6, collapse = ' + '))) 

AL1_model1 = lm(AL1_form1) #regression 
AL1_model2 = lm(AL1_form2) #regression 
AL1_model3 = lm(AL1_form3) #regression 
AL1_model4 = lm(AL1_form4) #regression 
AL1_model5 = lm(AL1_form5) #regression 
AL1_model6 = lm(AL1_form6) #regression 

AL1_rq1 = summary(AL1_model1)$adj.r.squared #collect adj. r squared
AL1_rq2 = summary(AL1_model2)$adj.r.squared 
AL1_rq3 = summary(AL1_model3)$adj.r.squared 
AL1_rq4 = summary(AL1_model4)$adj.r.squared 
AL1_rq5 = summary(AL1_model5)$adj.r.squared 
AL1_rq6 = summary(AL1_model6)$adj.r.squared 

setwd('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/ght results 8-9-2020')

#plot all 6 countries in one pane
tiff("test.png", units="in", width=10, height=5, res=300)
par(mfrow=c(2,3))
#plot incidence
plot (AL_br1$inci, main = paste('Tanzania', '\n\ Adj. R squared =', round(AL1_rq1,4)), 
      ylab = 'Weekly incidence', xlab = '', xaxt ='n',
      type = 'l', pch = 19, cex = 0.3, cex.axis = 1.3, cex.main = 1.5, cex.lab = 1.4) 
#regression line 
lines (predict (AL1_model1), col = 'blue', type ='b', pch = 19, cex = 0.3) 
#x-axis
axis (1, at=c(1,6,10,15,19), 
      labels = c('Feb', 'March', 'April', 'May', 'June'), cex.axis = 1.3)
#adding first case
#abline (v = f_c2, col = 'red', lty = 2)

text (6, quantile(AL_br1$inci, 0.95),
      labels = paste ('First case = ', '\n\ ', ff_1, sep = ''), 
      cex = 0.8, pos = '1', col= 'red', srt = 0)

#plot incidence
plot (AL_br2$inci, main = paste('Tunisia', '\n\ Adj. R squared =', round(AL1_rq2,4)), 
      ylab = 'Weekly incidence', xlab = '', xaxt ='n',
      type = 'l', pch = 19, cex = 0.3, cex.axis = 1.3, cex.main = 1.5, cex.lab = 1.4) 
#regression line 
lines (predict (AL1_model2), col = 'blue', type ='b', pch = 19, cex = 0.3) 
#x-axis
axis (1, at=c(1,6,10,15,19), 
      labels = c('Feb', 'March', 'April', 'May', 'June'), cex.axis = 1.3)
#adding first case
#abline (v = f_c2, col = 'red', lty = 2)

text (20, quantile(AL_br2$inci, 0.95),
      labels = paste ('First case = ', '\n\ ', ff_2, sep = ''), 
      cex = 0.8, pos = '1', col= 'red', srt = 0)
#plot incidence
plot (AL_br3$inci, main = paste('Burkina Faso', '\n\ Adj. R squared =', round(AL1_rq3,4)), 
      ylab = 'Weekly incidence', xlab = '', xaxt ='n',
      type = 'l', pch = 19, cex = 0.3, cex.axis = 1.3, cex.main = 1.5, cex.lab = 1.4) 
#regression line 
lines (predict (AL1_model3), col = 'blue', type ='b', pch = 19, cex = 0.3) 
#x-axis
axis (1, at=c(1,6,10,15,19), 
      labels = c('Feb', 'March', 'April', 'May', 'June'), cex.axis = 1.3)

text (20, quantile(AL_br3$inci, 0.95),
      labels = paste ('First case = ', '\n\ ', ff__3, sep = ''), 
      cex = 0.8, pos = '1', col= 'red', srt = 0)
#plot incidence
plot (AL_br4$inci, main = paste('Seychelles', '\n\ Adj. R squared =', round(AL1_rq4,4)), 
      ylab = 'Weekly incidence', xlab = '', xaxt ='n',
      type = 'l', pch = 19, cex = 0.3, cex.axis = 1.3, cex.main = 1.5, cex.lab = 1.4) 
#regression line 
lines (predict (AL1_model4), col = 'blue', type ='b', pch = 19, cex = 0.3) 
#x-axis
axis (1, at=c(1,6,10,15,19), 
      labels = c('Feb', 'March', 'April', 'May', 'June'), cex.axis = 1.3)

text (7, quantile(AL_br4$inci, 0.95),
      labels = paste ('First case = ', '\n\ ', ff_4, sep = ''), 
      cex = 0.8, pos = '3', col= 'red', srt = 0)
#plot incidence
plot (AL_br5$inci, main = paste('Benin', '\n\ Adj. R squared =', round(AL1_rq5,4)), 
      ylab = 'Weekly incidence', xlab = '', xaxt ='n',
      type = 'l', pch = 19, cex = 0.3, cex.axis = 1.3, cex.main = 1.5, cex.lab = 1.4) 
#regression line 
lines (predict (AL1_model5), col = 'blue', type ='b', pch = 19, cex = 0.3) 
#x-axis
axis (1, at=c(1,6,10,15,19), 
      labels = c('Feb', 'March', 'April', 'May', 'June'), cex.axis = 1.3)

text (7, quantile(AL_br5$inci, 0.95),
      labels = paste ('First case = ', '\n\ ', ff__5, sep = ''), 
      cex = 0.8, pos = '1', col= 'red', srt = 0)
#plot incidence
plot (AL_br6$inci, main = paste('Namibia', '\n\ Adj. R squared =', round(AL1_rq6,4)), 
      ylab = 'Weekly incidence', xlab = '', xaxt ='n',
      type = 'l', pch = 19, cex = 0.3, cex.axis = 1.3, cex.main = 1.5, cex.lab = 1.4) 
#regression line 
lines (predict (AL1_model6), col = 'blue', type ='b', pch = 19, cex = 0.3) 
#x-axis
axis (1, at=c(1,6,10,15,19), 
      labels = c('Feb', 'March', 'April', 'May', 'June'), cex.axis = 1.3)

text (7, quantile(AL_br6$inci, 0.95),
      labels = paste ('First case = ', '\n\ ', ff_6, sep = ''), 
      cex = 0.8, pos = '1', col= 'red', srt = 0)
dev.off()
#####avg weekly deaths for the four time periods######
list1 = list.files('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/data 1-28-2021/', full.names = T)
library(plyr)
#creates empty dataframe
df = list()
#AL = read.csv('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/data 1-28-2021/Algeria.csv')
for (i in list1){
  
  AL = read.csv (i)
  AL$caseinci = AL$New.Cases/AL$pop
  AL$deathinci = AL$New.Death/AL$pop
  obj1 = mean(AL[12:100,7])
  obj2 = mean(AL[101:192,7])
  obj3 = mean(AL[193:284,7])
  obj4 = mean(AL[285:373,7])
  cl = c(obj1,obj2,obj3,obj4)
  df[[length(df)+1]] = cl
}

finalobj = t(data.frame(df))
finalobj = data.frame(finalobj)
#change column names for the three time periods
names(finalobj)[names(finalobj) == "X1"] <- "Feb.2.to.April.30"
names(finalobj)[names(finalobj) == "X2"] <- "May.1.to.July.31"
names(finalobj)[names(finalobj) == "X3"] <- "Aug.1.to.Oct.31"
names(finalobj)[names(finalobj) == "X4"] <- "Nov.1.to.Jan.28"
View(finalobj)
write.csv (finalobj, 'C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/avgcasesanddeath/avgcases_1-28-2021.csv', row.names = F )

########day of first case graph############
#FIGURE 1
fc = read.csv('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/ght results 1-28-2021/dofc.csv')
#ac = read.csv('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/avgcasesanddeath/avgcases1sorted.csv')
fc1 <- data.frame(Country = fc$Country,
                  First = fc$Day.of.First.Case)
fc1$First <- as.Date(fc1$First, "%m/%d/%Y")
# plot(Day.of.First.Case ~ Country, fc1, xaxt = "n", type = "l")
# axis(1, fc1$Day.of.First.Case, format(fc1$Day.of.First.Case, "%b %d"), cex.axis = .7)
colnames(fc1) = c('Country', 'Day.of.the.First.Case')

library(ggrepel)
library( ggplot2 )

p <- ggplot( fc1, aes(x=Country, y=Day.of.the.First.Case) ) + geom_point() + 
  scale_y_date( date_labels = "%b %d", date_breaks = "1 week") +
  theme(axis.text.x=element_blank()) +
  labs( y="Date", x="Country")
p + geom_text_repel(aes(label=fc$ID), size=2) + theme(legend.position = "None")   # text

plot1 = p + geom_label_repel(aes(label=fc$ID), size=2)  + theme(legend.position = "None")   # label
ggsave('dofcgraph1.tiff', plot=plot1, width=6, height=5,units='in', dpi=300)

######MAPS IN R BASED ON DATAFRAMES#####
#we didnt end up using these, but in case anyone would like to replicate the maps in a simple way:
#required packages
library(sp)
library(raster)
library(maps)
library(rgeos)
library (rgdal)
library (maptools)
library(mapdata)
library(utils)
library(gridExtra)


#Setting the working directory 
#setwd('/Users/daniel/Documents/CORONAVIRUS/brazil/_MANUSCRIPT')

#reading databases (with official country ID and corrected by population (incidence), top line is case data, bottom is death data)
cas = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/avgcasesanddeath/avgcases_1-28-2021_samescale.csv')
cas1 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/avgcasesanddeath/avgdeath_1-28-2021_samescale.csv')
#fc1 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/drafts/Supplementary Table 2 - Terms that had information_AF.csv')
fc1 = read.csv ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/dofcredo1.csv')

# head (cas) #checking the first 6 rows
# head (death)

#reading the shapefiles
#writeOGR(amr_sh3, layer= 'shape_3', dsn = 'shape_3', driver = 'ESRI Shapefile') #create shape files
#download the world in countries from: https://www.naturalearthdata.com/downloads/50m-cultural-vectors/

#read the shapefile: 
wrd = readOGR ('C:/Users/bigfo/OneDrive/Desktop/School/Research/Covid19/Africa Country Data/Combined1/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp')
WGS84 = crs(wrd) #define the projection 

names(wrd@data) #check the name of the columns
unique (wrd@data$CONTINENT) #check the names of the column called CONTIINENT 
unique (wrd@data$NAME)

#add the two missing countries and remove the disputed territories (somaliland and western sahara)
coun1 = subset(wrd, NAME == 'Seychelles')
coun2 = subset(wrd, NAME == 'Mauritius')
afr1 = subset (wrd, CONTINENT == 'Africa') #subset by continent name
rbind(afr1,coun1) -> afr1
rbind(afr1,coun2) -> afr1
row.names.remove <- c("193","207")
afr1[!row.names(afr1@data) %in% row.names.remove,] -> afr1
View(afr1@data)
View(wrd@data)
#plot (afr1) #plot the map 
afr1@data$SOV_A3[39] <- 'SSD'
names (afr1@data) #review the names of the attribute table of the subset object 

#Now the plan is to make the incidence dataframe match with the spatial polygons dataframe

length (afr1@data$NAME) #checking the length of both dataframes
length(fc1$Country) 

sort(afr1@data$SOV_A3)#sorting the country names by alphabetical order
afr1@data$NAME
#View(afr1@data)
or1 = afr1@data$SOV_A3 #defining objects to compare the names, from spatial polygon 
or2 = fc1$ID #defining objects to compare names, from the incidence dataframe
or1 == or2 #comparing both objects...a lot of differences...

#' because of that we decided to MANUALLY add a column in the incidence dataframe
#' with the three letter SOV_A3 column code from the natural earth shapefile
#after checking that everything matches: 
#adding a column from the incidence dataframe in the spatial polygon dataframe for cases: 

afr1@data$FDC = cas$FirstC

afr1@data$SDC = cas$SecondC#replace by the correct name of the column 

afr1@data$TDC = cas$ThirdC

afr1@data$FoDC = cas$FourthC

afr1@data$Colors = cas$Colors

#for death

afr1@data$FDD = cas1$FirstD

afr1@data$SDD = cas1$SecondD#replace by the correct name of the column 

afr1@data$TDD = cas1$ThirdD

afr1@data$FoDD = cas1$FourthD

afr1@data$Colors = cas1$Colors

afr1@data$DOFC = fc1$num

afr1@data$DOFC1 = fc1$Num.first.case.1

#Creating the color ramp: 
#rampcolor: 
gradient_color = colorRampPalette(c("white", "blue", "blue3")) #for cases
gradient_color1 = colorRampPalette(c("white", "orange", "red2")) #for deaths
#select desired colors, 
#check: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
#check: https://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3

cbw = gradient_color(30) #select a number of available colors for the map
cbw1 = gradient_color1(30)
tiff("avgcaseanddeath.tiff", units="in", width=10, height=5, res=300)
grid.arrange(spplot(afr1, "FIRST_DATE_cases", col.regions = cbw, at = seq(0,2,.08),main=list(label="February 2 to March 31",cex=1)) , 
             spplot(afr1, "SECOND_DATE_cases", col.regions = cbw, at = seq(0,30,1),main=list(label="April 1 to May 31",cex=1)), 
             spplot(afr1, "THIRD_DATE_cases", col.regions = cbw, at = seq(0,140,5),main=list(label="June 1 to August 2",cex=1)),
             spplot(afr1, "FIRST_DATE_death", col.regions = cbw1, at = seq(0,.1,.005),main=list(label="February 2 to March 31",cex=1)),
             spplot(afr1, "SECOND_DATE_death", col.regions = cbw1, at = seq(0,1,.05),main=list(label="April 1 to May 31",cex=1)),
             spplot(afr1, "THIRD_DATE_death", col.regions = cbw1, at = seq(0,2.30,.08),main=list(label="June 1 to August 2",cex=1)),
             ncol=3)

#Plot the figures: 
# spplot(afr1, "FIRST_DATE",
#        col.regions = cbw, at = seq(0,2e-5,.000001),main=list(label="February 2 to March 31",cex=1))
# # #Plot the figures: 
# # spplot(afr1, "FIRST_DATE",
# #        col.regions = cbw, cuts = 10, scale = (draw = T),main=list(label="February 2 to March 31",cex=1))
# #February 2 to March 31
# #April 1 to May 31
# #June 1 to August 2

#write shapefile with the added information: 
writeOGR(afr1, layer= 'afr1', dsn = 'afr123', driver = 'ESRI Shapefile')
dev.off()
