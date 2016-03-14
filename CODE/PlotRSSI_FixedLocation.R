#Author: Ramanathan Subramanian
#Last Modified: 14th March '16

#Clear workspace
rm(list = ls(all.names = TRUE))

#Clear console
cat("\014")

#Load Packages
library(dplyr)
library(ggmap)
library(R.devices)

#Paramater Settings
decimation_factor=1;

#Set Working directory
(WD <- getwd())
if (!is.null(WD)) setwd(WD)

#Load the multiplot()
source("multiplot.R")

#Retrieve RF data from CSV file
radio_data <- read.csv("../DATA/FixedLocation/HynesCenter.csv", header=TRUE,sep =" ")
if(any(names(radio_data)=="row.names")){radio_data<-select(radio_data,-row.names,-name,-location)} else {{radio_data<-select(radio_data,-name,-location)}}
if(!any(names(radio_data)=="type") && ncol(radio_data)>11) {cnames <- c("latitude", "longitude", "time", "speed", "type", "rssi_avg", "rssi_var", "cellid", "cqi", "ssnr", "throughput_down", "throughput_up", "delayCon", "delay", "pdr") 
                                                            colnames(radio_data) <- cnames} else if(!any(names(radio_data)=="type") && ncol(radio_data)==21) {cnames <- c("latitude", "longitude", "time", "speed", "type", "rssi_avg", "rssi_var", "cellid", "cqi", "ssnr", "throughput_down", "throughput_up", "delayCon", "delay", "pdr", "X", "X.1", "numWiFiOpen", "X.2", "numWiFiFreeChannels", "X.3")
                                                                                                                                                              colnames(radio_data) <- cnames} else if(!any(names(radio_data)=="type") && ncol(radio_data)==10) {cnames <- c("latitude", "longitude", "time", "speed", "type", "rssi_avg", "rssi_var", "cellid", "cqi", "ssnr")
                                                                                                                                                                                                                                                                colnames(radio_data) <- cnames}

#Retain only columns of interest
radio_data<-select(radio_data,1:7)

#Omit rows which have NA entries
radio_data<-na.omit(radio_data)

#Remove rows which have rssi_avg == 0
c <- radio_data['rssi_avg']!= 0
radio_data <- radio_data[c,]

#Subsample rows
b<-seq(1,nrow(radio_data),decimation_factor)
radio_data<-radio_data[b,]

#Elapsed Time in seconds
radio_data[,'time_elapsed_sec'] <- (radio_data['time']-radio_data['time'][1,1])/1000

#Determine real-time
real_time <- function(x){as.POSIXct(x, origin="1970-01-01", tz="EST")}
radio_data['time']<-lapply(3600+radio_data['time']/1000,real_time)

#Date
date <- strsplit(as.character(radio_data['time'][1,])," ")[[1]][1]

#Start Time
start_time <- strsplit(as.character(radio_data['time'][1,])," ")[[1]][2]

#End Time
end_time <- strsplit(as.character(radio_data['time'][nrow(radio_data),])," ")[[1]][2]

d<-(1:nrow(radio_data))
k<-d[c][b]

#Graphs
p1 <- ggplot(radio_data, aes(x = time_elapsed_sec, y = rssi_avg))+geom_line(color="firebrick")+ggtitle('Average Radio Signal Strength')+labs(x="Seconds", y="Average(RSSI)")
p2 <- ggplot(radio_data, aes(x = time_elapsed_sec, y = rssi_var))+geom_point()+ggtitle('Variability in Radio Signal Strength')+labs(x="Seconds", y="Variance(RSSI)")
p3 <- ggplot(radio_data, aes(x = time_elapsed_sec, y = rssi_avg))+geom_point(aes(size= rssi_var, color=rssi_var))+geom_line(color="firebrick")+theme(legend.position="none")+ggtitle('Average Radio Signal Strength')+labs(x="Seconds", y="Average(RSSI)")
myplot <- multiplot(p1, p2, p3, cols = 3)
