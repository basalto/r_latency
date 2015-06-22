library(ggplot2)
library(RMySQL)
library(dplyr)
library(plyr)
library(scales)
library(gridExtra)

setwd("D:\\Development\\R\\scripts\\latency")


# Import latency data from database
con <- dbConnect(RMySQL::MySQL(), user="root", password="404306516da", dbname="raspProbe", host="62.169.65.146")

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


# Create subsets for host
myHost <- c("351938722727")
latency.1min.host  <- subset(latency.1min, Host==myHost)
latency.5min.host  <- subset(latency.5min, Host==myHost)
latency.15min.host <- subset(latency.15min, Host==myHost)
latency.30min.host <- subset(latency.30min, Host==myHost)
latency.60min.host <- subset(latency.60min, Host==myHost)


# Set chart limits
last.n.days <- as.numeric("5")
time_end    <- Sys.time() - 0*24*60*60
time_start  <- time_end - last.n.days*24*60*60
lim_start   <- strptime(time_start, "%Y-%m-%d %H:%M:%S")
lim_end     <- strptime(time_end, "%Y-%m-%d %H:%M:%S")
xlim <- as.POSIXct(c(lim_start, lim_end), origin="1970-01-01", tz="GMT")


# Line chart of one host
c.1min <- ggplot(latency.1min.host, aes(x=Timestamp, y=RTT)) + geom_line(colour="red") +
  scale_y_continuous(breaks=seq(0, 2000, 500), limits=c(0, 2000)) +
  scale_x_datetime(breaks = date_breaks("3 hour"), minor_breaks=date_breaks("1 hour"), labels=date_format("%d-%m %H:%M"), limits=xlim) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x="", y="RTT (ms)", title="Average (1 min aggregation)") + 
  theme(legend.position='none', plot.title=element_text(size = rel(2)), axis.text.x=element_text(size = rel(0.9)))

c.5min <- ggplot(latency.5min.host, aes(x=Timestamp, y=RTT)) + geom_line(colour="red") +
  scale_y_continuous(breaks=seq(0, 2000, 500), limits=c(0, 2000)) +
  scale_x_datetime(breaks = date_breaks("3 hour"), minor_breaks=date_breaks("1 hour"), labels=date_format("%d-%m %H:%M"), limits=xlim) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x="", y="RTT (ms)", title="Average (5 min aggregation)") + 
  theme(legend.position='none', plot.title=element_text(size = rel(2)), axis.text.x=element_text(size = rel(0.9)))

c.15min <- ggplot(latency.15min.host, aes(x=Timestamp, y=RTT)) + geom_line(colour="red") +
  scale_y_continuous(breaks=seq(0, 2000, 500), limits=c(0, 2000)) +
  scale_x_datetime(breaks = date_breaks("3 hour"), minor_breaks=date_breaks("1 hour"), labels=date_format("%d-%m %H:%M"), limits=xlim) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x="", y="RTT (ms)", title="Average (15 min aggregation)") + 
  theme(legend.position='none', plot.title=element_text(size = rel(2)), axis.text.x=element_text(size = rel(0.9)))

c.30min <- ggplot(latency.30min.host, aes(x=Timestamp, y=RTT)) + geom_line(colour="red") +
  scale_y_continuous(breaks=seq(0, 2000, 500), limits=c(0, 2000)) +
  scale_x_datetime(breaks = date_breaks("3 hour"), minor_breaks=date_breaks("1 hour"), labels=date_format("%d-%m %H:%M"), limits=xlim) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x="", y="RTT (ms)", title="Average (30 min aggregation)") + 
  theme(legend.position='none', plot.title=element_text(size = rel(2)), axis.text.x=element_text(size = rel(0.9)))

c.60min <- ggplot(latency.60min.host, aes(x=Timestamp, y=RTT)) + geom_line(colour="red") +
  scale_y_continuous(breaks=seq(0, 2000, 500), limits=c(0, 2000)) +
  scale_x_datetime(breaks = date_breaks("3 hour"), minor_breaks=date_breaks("1 hour"), labels=date_format("%d-%m %H:%M"), limits=xlim) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(x="", y="RTT (ms)", title="Average (60 min aggregation)") + 
  theme(legend.position='none', plot.title=element_text(size = rel(2)), axis.text.x=element_text(size = rel(0.9)))

print(c.1min)
print(c.5min)
print(c.15min)
print(c.30min)
print(c.60min)
ggsave(c.1min, file=paste0("RTT_agg1_",myHost,"_1min.jpeg"),  width=12, height=7, dpi=100)
ggsave(c.5min, file=paste0("RTT_agg2_",myHost,"_5min.jpeg"),  width=12, height=7, dpi=100)
ggsave(c.15min,file=paste0("RTT_agg3_",myHost,"_15min.jpeg"), width=12, height=7, dpi=100)
ggsave(c.30min,file=paste0("RTT_agg4_",myHost,"_30min.jpeg"), width=12, height=7, dpi=100)
ggsave(c.60min,file=paste0("RTT_agg5_",myHost,"_60min.jpeg"), width=12, height=7, dpi=100)


# line chart of one host by Hour
latency.by.Hour = ddply(latency.1min, "Hour", summarize, RTT=mean(RTT))
c.hour <- ggplot(latency.by.Hour,aes(x=Hour, y=RTT)) + geom_bar(stat="identity") + geom_smooth(aes(group=1)) + 
  xlab('Hour of day') + ylab('mean RTT (ms)')
print(c.hour)
ggsave(c.hour,file=paste0("RTT_byHour_",myHost,".jpeg"), width=20, height=7)


charts <- arrangeGrob(c.1min, c.5min, c.15min, c.30min, c.60min, c.hour, ncol=3, nrow=2)
print(charts)
ggsave(charts,file=paste0("RTT_check_",myHost,".jpeg"), width=20, height=10, dpi=100)


# END 

ggplot(latency,aes(x=Hour, y=RTT)) + geom_boxplot() + 
  xlab('hr of day') + ylab('freq')


latency_edge <- subset(latency.1min, Coverage=="EDGE")
latency_center <- subset(latency.1min, Coverage=="CENTER")
latency_fo <- subset(latency.1min, Coverage=="FO")
latency_351938722727 <- subset(latency.1min, Host=="351938722727")
latency_351930474974 <- subset(latency.1min, Host=="351930474974")



ggplot(latency.1min, aes(x=RTT)) + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "green", high = "red") + facet_wrap(Load ~ Host, ncol=2)
ggplot(latency, aes(x=Timestamp, y=RTT, colour=Host)) + geom_line() + xlab("") + ylab("RTT (ms)") + facet_wrap(~ Host, ncol=2)
ggplot(latency.1min, aes(x=Timestamp, y=RTT, colour=Host)) + geom_point() + facet_wrap(Load ~ Host, ncol=3)
ggplot(latency.1min, aes(x=Timestamp, y=RTT, colour=Host)) + geom_point() + facet_grid(~ Coverage, scales = "free")

ggplot(latency, aes(x=Timestamp, y=RTT)) + geom_jitter(aes(colour = Host), size=0.3)
ggplot(latency_5min, aes(x=Timestamp, y=RTT)) + geom_jitter(aes(colour = Host), size=0.3)



ggplot(latency, aes(x=Timestamp, y=RTT)) + geom_jitter(aes(colour = Host), size=0.3)
ggplot(latency, aes(x=Timestamp, y=RTT)) + geom_jitter(aes(colour = Host), size=0.3) + facet_wrap(Load ~ Host, ncol=3)
ggplot(latency, aes(x=Timestamp, y=RTT)) + geom_jitter(aes(colour = Host), size=0.3) + facet_wrap(Load ~ Coverage, ncol=2) 
ggplot(latency, aes(x=Timestamp, y=RTT)) + geom_jitter(aes(colour = Host), size=0.3) + facet_grid(Load ~ Host) 



ggplot(latency_351938722727, aes(x=Timestamp, y=RTT)) + geom_point(aes(colour = "red"), size=2) + theme(legend.position='none')
ggplot(latency_5min_351938722727, aes(x=Timestamp, y=RTT)) + geom_point(aes(colour = "red"), size=2) + theme(legend.position='none')
ggplot(latency_15min_351938722727, aes(x=Timestamp, y=RTT)) + geom_point(aes(colour = "red"), size=2) + theme(legend.position='none')

#ggplot(latency_351938722727, aes(x=Timestamp, y=RTT)) + geom_bar(stat = "identity") + xlab("") + ylab("RTT (ms)") + theme(legend.position='none')
#ggplot(latency_5min_351938722727, aes(x=Timestamp, y=RTT)) + geom_line() + xlab("") + ylab("RTT (ms)") + theme(legend.position='none')
#ggplot(latency_15min_351938722727, aes(x=Timestamp, y=RTT)) + geom_line() + xlab("") + ylab("RTT (ms)") + theme(legend.position='none')

#ggplot(latency_351930474974, aes(x=Timestamp, y=RTT)) + geom_jitter(aes(colour = Host), size=0.5)

factor(latency$Timestamp)
head(table(factor(latency$Host)))
head(table(factor(latency_5min$Host)))
head(table(factor(latency_15min$Host)))

      