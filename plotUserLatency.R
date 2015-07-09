plotUserLatency <- function(dir, msisdn, ylimit) {
  # Create subsets for host
  latency.1min.host  <- subset(latency.1min, Host==msisdn)
  latency.5min.host  <- subset(latency.5min, Host==msisdn)
  latency.15min.host <- subset(latency.15min, Host==msisdn)
  latency.30min.host <- subset(latency.30min, Host==msisdn)
  latency.60min.host <- subset(latency.60min, Host==msisdn)
  
  # Line chart of one host
  c.1min <- ggplot(latency.1min.host, aes(x=Timestamp, y=RTT)) + geom_line(colour="red") +
    scale_y_continuous(breaks=seq(0, ylimit, ylimit/5), limits=c(0, ylimit)) +
    scale_x_datetime(breaks = date_breaks("3 hour"), minor_breaks=date_breaks("1 hour"), labels=date_format("%d-%m %H:%M"), limits=xlim) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
    labs(x="", y="RTT (ms)", title="Average (1 min aggregation)") + 
    theme(legend.position='none', plot.title=element_text(size = rel(2)), axis.text.x=element_text(size = rel(0.9)))
  
  c.5min <- ggplot(latency.5min.host, aes(x=Timestamp, y=RTT)) + geom_line(colour="red") +
    scale_y_continuous(breaks=seq(0, ylimit, ylimit/5), limits=c(0, ylimit)) +
    scale_x_datetime(breaks = date_breaks("3 hour"), minor_breaks=date_breaks("1 hour"), labels=date_format("%d-%m %H:%M"), limits=xlim) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
    labs(x="", y="RTT (ms)", title="Average (5 min aggregation)") + 
    theme(legend.position='none', plot.title=element_text(size = rel(2)), axis.text.x=element_text(size = rel(0.9)))
  
  c.15min <- ggplot(latency.15min.host, aes(x=Timestamp, y=RTT)) + geom_line(colour="red") +
    scale_y_continuous(breaks=seq(0, ylimit, ylimit/5), limits=c(0, ylimit)) +
    scale_x_datetime(breaks = date_breaks("3 hour"), minor_breaks=date_breaks("1 hour"), labels=date_format("%d-%m %H:%M"), limits=xlim) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
    labs(x="", y="RTT (ms)", title="Average (15 min aggregation)") + 
    theme(legend.position='none', plot.title=element_text(size = rel(2)), axis.text.x=element_text(size = rel(0.9)))
  
  c.30min <- ggplot(latency.30min.host, aes(x=Timestamp, y=RTT)) + geom_line(colour="red") +
    scale_y_continuous(breaks=seq(0, ylimit, ylimit/5), limits=c(0, ylimit)) +
    scale_x_datetime(breaks = date_breaks("3 hour"), minor_breaks=date_breaks("1 hour"), labels=date_format("%d-%m %H:%M"), limits=xlim) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
    labs(x="", y="RTT (ms)", title="Average (30 min aggregation)") + 
    theme(legend.position='none', plot.title=element_text(size = rel(2)), axis.text.x=element_text(size = rel(0.9)))
  
  c.60min <- ggplot(latency.60min.host, aes(x=Timestamp, y=RTT)) + geom_line(colour="red") +
    scale_y_continuous(breaks=seq(0, ylimit, ylimit/5), limits=c(0, ylimit)) +
    scale_x_datetime(breaks = date_breaks("3 hour"), minor_breaks=date_breaks("1 hour"), labels=date_format("%d-%m %H:%M"), limits=xlim) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
    labs(x="", y="RTT (ms)", title="Average (60 min aggregation)") + 
    theme(legend.position='none', plot.title=element_text(size = rel(2)), axis.text.x=element_text(size = rel(0.9)))
  
  # line chart of one host by Hour
  latency.by.Hour = ddply(latency.1min, "Hour", summarize, RTT=mean(RTT))
  c.hour <- ggplot(latency.by.Hour,aes(x=Hour, y=RTT)) + geom_bar(stat="identity") + geom_smooth(aes(group=1)) + 
    xlab('Hour of day') + ylab('mean RTT (ms)')
  
  ggsave(c.1min, file=paste0(dir,"RTT_agg1_",msisdn,"_1min.jpeg"),  width=12, height=7, dpi=100)
  ggsave(c.5min, file=paste0(dir,"RTT_agg2_",msisdn,"_5min.jpeg"),  width=12, height=7, dpi=100)
  ggsave(c.15min,file=paste0(dir,"RTT_agg3_",msisdn,"_15min.jpeg"), width=12, height=7, dpi=100)
  ggsave(c.30min,file=paste0(dir,"RTT_agg4_",msisdn,"_30min.jpeg"), width=12, height=7, dpi=100)
  ggsave(c.60min,file=paste0(dir,"RTT_agg5_",msisdn,"_60min.jpeg"), width=12, height=7, dpi=100)
  ggsave(c.hour, file=paste0(dir,"RTT_byHour_",msisdn,".jpeg"), width=20, height=7)
  
  charts <- arrangeGrob(c.1min, c.5min, c.15min, c.60min, ncol=2, nrow=2)
  print(charts)
}