library(dplyr)
library(tidyr)
library(stringr)
library(gdata)
library(ggplot2)
library(choroplethr)
library(R6)
library(plotly)

#Load data
data1<-read.xls("oilgascounty.xls", sheet=1)
head(data1)
dim(data1)

#delete the cols that without data(NAs)
data2 <- data1[(colSums(is.na(data1)) != nrow(data1))]
head(data2)
dim(data2)

#delete the cols that only have one value
data3<-Filter(function(x)(length(unique(x))>1), data2)
head(data3)
dim(data3)

# delete the cols that have the same value

data4<-t(unique(t(data3)))
data4<-data.frame(data4)
dim(data4)

#get rid of comma style format(thousands seperator)
data5<-{
  data4 %>%
    mutate_each(funs(as.character(.)), oil2000:gas2011) %>%
    mutate_each(funs(gsub(",", "", .)), oil2000:gas2011) %>%
    mutate_each(funs(as.numeric(.)), oil2000:gas2011)}
head(data5)
dim(data5)

#factor
data5$Urban_Influence_2013<-as.factor(data5$Urban_Influence_2013)
data5$Rural_Urban_Continuum_Code_2013<-as.factor(data5$Rural_Urban_Continuum_Code_2013)
data5$oil_change_group <- as.factor(data5$oil_change_group) 
data5$gas_change_group <- as.factor(data5$gas_change_group)
data5$oil_gas_change_group <- as.factor(data5$oil_gas_change_group)


data5$County_Name <- gsub("County", "", data5$County_Name)


#clean the rows that annual gross withdrawals of curde oil and natural gas =0
#create a subset "value", which only contains data of oil and gas withdrawals. 
nums <- sapply(data5, is.numeric)
value <- data5[,nums]

x=NULL
for(i in 1:nrow(value)){
if(sum(value[i,])==0){x=c(x,i)}}

data6<-data5[-x,]
head(data6)
dim(data6)

#data frame (tidy data)
#oil= subset that does not contain gas data
#gas= subset that does not contain oil data
oil<- data.frame(subset(data6, select = -c(gas2000:gas2011)))
gas<- data.frame(subset(data6, select = -c(oil2000:oil2011)))

oil1 <- oil %>% 
  gather(Year, oil, oil2000:oil2011)%>%
  mutate(Year = gsub("oil", "", Year))%>%
  arrange(County_Name, Year)

gas1<- gas%>%
  gather(Year, gas, gas2000:gas2011)%>%
  mutate(Year = gsub("gas", "", Year))%>%
  arrange(County_Name, Year)

#merge two data frame, get tidy data
data_clean<-merge(gas1,oil1)


###Analysis

#summary 
summary(data_clean$oil)
summary(data_clean$gas)
#remove outlier
#reference: https://www.r-bloggers.com/identify-describe-plot-and-remove-the-outliers-from-the-dataset/
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1)
  m2 <- mean(var_name, na.rm = T)
  dt[as.character(substitute(var))] <- invisible(var_name)
  assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
  return(invisible(dt))
}

no_outlier<-data_clean
no_outlier<-outlierKD(no_outlier, oil)
no_outlier<-outlierKD(no_outlier, gas)
no_outlier[no_outlier == 0 & is.numeric(no_outlier)] <- NA



###Visualization

#US map
oilvalue<-value[,1:(length(value)/2)]
gasvalue<-value[,(length(value)/2+1):length(value)]
oilsum<-data.frame(rowSums(oilvalue))
gassum<-data.frame(rowSums(gasvalue))
colnames(oilsum)[1]<-"value"
colnames(gassum)[1]<-"value"


df <- data1
aa<-data.frame(subset(df,select = c(1,3,4,8)))
colnames(aa)[1]<-'region'
oilsum<-cbind(aa,oilsum)
gassum<-cbind(aa,gassum)

#Plot the oil production distribution across the US
oilcho = CountyChoropleth$new(oilsum)
oilcho$title = "Oil Production across US"
oilcho$ggplot_scale = scale_fill_brewer(name="Barrel Produced", palette=2, drop=FALSE)
oilcho$render()

#Plot the gas prodcution distribution 
gascho = CountyChoropleth$new(gassum)
gascho$title = "Gas Production across US"
gascho$ggplot_scale = scale_fill_brewer(name="Barrel Produced", palette=3, drop=FALSE)
gascho$render()


#Plot the oil production based on the Metro, Micro, and Noncore statues
plot_ly(data = data_clean, x = ~Year , y = ~oil, color = ~Metro_Micro_Noncore_2013)
plot_ly(data = no_outlier, x = ~Year , y = ~oil, color = ~Metro_Micro_Noncore_2013)

plot_ly(data = data_clean, x = ~Year , y = ~gas, color = ~Metro_Micro_Noncore_2013)
plot_ly(data = no_outlier, x = ~Year , y = ~gas, color = ~Metro_Micro_Noncore_2013)


#Plot the oil and gas production based on Rural Urban Continuum Code 2013
#
plot_ly(data=no_outlier, y = ~oil, color = ~Rural_Urban_Continuum_Code_2013, type = "box")
plot_ly(data=no_outlier, y = ~gas, color = ~Rural_Urban_Continuum_Code_2013, type = "box")

plot_ly(data=no_outlier, y = ~oil, color = ~Metro_Micro_Noncore_2013, type = "box")
plot_ly(data=no_outlier, y = ~gas, color = ~Metro_Micro_Noncore_2013, type = "box")


#scatter plot (x=year, y=sum(value))
#oil hyperbola trend, gas increasing trend
op<-data.frame(colSums(oilvalue))
op$oil<-op$colSums.oilvalue.
op$Year<- c(2000:2011)
og<-data.frame(colSums(gasvalue))
og$gas<-og$colSums.gasvalue.
og$Year<- c(2000:2011)
plot_ly(op, x=~Year, y=~oil, type = "scatter")
plot_ly(og, x=~Year, y=~gas, type = "scatter")



#which state produce the most?
plot_ly(data_clean, x=~ Stabr, y=~ oil)
plot_ly(data_clean, x=~ Stabr, y=~ gas)






##Reference
#http://indicatorsidaho.org/DrawRegion.aspx?IndicatorID=36&RegionID=16049
    