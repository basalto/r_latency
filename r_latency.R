library(ggplot2)
library(RMySQL)
library(dplyr)
library(plyr)
library(scales)
library(gridExtra)


setwd("D:\\Development\\R\\scripts\\r_latency")
#setwd("/home/rjdinis/development/R/r_latency")

source("plotUserLatency.R")

# Import latency data from App-Server database (Linux)
m <- dbDriver("MySQL")
rmysql.settingsfile <- "D:\\Users\\rjdinis\\my.cnf"
#rmysql.settingsfile <- "~/.cnf"
con <- dbConnect(m, default.file=rmysql.settingsfile, group="latency", dbName="raspProbe")
dbSendQuery(con, "USE raspProbe")

# Import latency data from App-Server database (windows)
m <- dbDriver("MySQL")
rmysql.settingsfile <- "D:\\Users\\rjdinis\\my.cnf"
con <- dbConnect(m, default.file=rmysql.settingsfile, group="latency", dbName="raspProbe")
dbSendQuery(con, "USE raspProbe")

# Import latency data from aws database (windows)
m <- dbDriver("MySQL")
rmysql.settingsfile <- "D:\\Users\\rjdinis\\my.cnf"
con <- dbConnect(m, default.file=rmysql.settingsfile, group="aws", dbName="myProbe")
dbSendQuery(con, "USE myProbe")

rs = dbSendQuery(con, "select * from view_latency_1min")
latency.1min = fetch(rs, n=-1)

rs = dbSendQuery(con, "select * from view_latency_5min")
latency.5min = fetch(rs, n=-1)

rs = dbSendQuery(con, "select * from view_latency_15min")
latency.15min = fetch(rs, n=-1)

rs = dbSendQuery(con, "select * from view_latency_30min")
latency.30min = fetch(rs, n=-1)

rs = dbSendQuery(con, "select * from view_latency_60min")
latency.60min = fetch(rs, n=-1)

dbClearResult(rs)
dbDisconnect(con)


# Format data imported from database
latency.1min$Host      <- as.factor(latency.1min$Host)
latency.1min$Coverage  <- as.factor(latency.1min$Coverage)
latency.1min$Load      <- as.factor(latency.1min$Load)
latency.1min$Timestamp <- as.POSIXlt(latency.1min$Timestamp, tz="GMT")
latency.1min$Hour = factor(format(latency.1min$Timestamp, format="%H"))

latency.5min$Host      <- as.factor(latency.5min$Host)
latency.5min$Timestamp <- as.POSIXct(latency.5min$Timestamp, tz="GMT")
latency.5min$Hour = factor(format(latency.5min$Timestamp, format="%H"))

latency.15min$Host <- as.factor(latency.15min$Host)
latency.15min$Timestamp <- as.POSIXct(latency.15min$Timestamp, tz="GMT")
latency.15min$Hour = factor(format(latency.15min$Timestamp, format="%H"))

latency.30min$Host <- as.factor(latency.30min$Host)
latency.30min$Timestamp <- as.POSIXct(latency.30min$Timestamp, tz="GMT")
latency.30min$Hour = factor(format(latency.30min$Timestamp, format="%H"))

latency.60min$Host <- as.factor(latency.60min$Host)
latency.60min$Timestamp <- as.POSIXct(latency.60min$Timestamp, tz="GMT")
latency.60min$Hour = factor(format(latency.60min$Timestamp, format="%H"))


# Set chart limits
last.n.days <- as.numeric("1")
time_end    <- Sys.time() - 0*24*60*60
time_start  <- time_end - last.n.days*24*60*60
lim_start   <- strptime(time_start, "%Y-%m-%d %H:%M:%S")
lim_end     <- strptime(time_end, "%Y-%m-%d %H:%M:%S")
xlim <- as.POSIXct(c(lim_start, lim_end), origin="1970-01-01", tz="GMT")


# Print charts
chartDir <- paste0(getwd(),"\\charts\\")
plotUserLatency(chartDir, c("351935575160"), 500)
plotUserLatency(chartDir, c("351930478639"), 500)
plotUserLatency(chartDir, c("351934766450"), 500)
plotUserLatency(chartDir, c("351932370687"), 500)
