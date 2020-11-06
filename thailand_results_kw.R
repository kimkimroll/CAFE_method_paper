
setwd("//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Kim/R/cafe_paper")

thaidata <- read.csv("thailand_data_hw.csv", header=TRUE, na.strings=c("","NA"))
thaidata$result1<-as.character(thaidata$result1)
thaidata$result1[grep("Negative", allg$result1)]<- 'NEG'
allg<-as.data.frame(thaidata)



#make r_result column and remove rows with no results
allg$r_result <-paste(allg$result1, allg$result2, allg$result3, sep="_")
allg$r_result<-as.character(allg$r_result)
allg$r_result[allg$r_result == "SL1_SL3_NPEV"] <- 'SL1 & SL3'
allg$r_result[allg$r_result == "SL1_SL3_NA"] <- 'SL1 & SL3'
allg$r_result[allg$r_result == "SL1_NPEV_NA"] <- 'SL1'
allg$r_result[allg$r_result == "SL1_NPEV_NEV"] <- 'SL1'
allg$r_result[allg$r_result == "SL1_NA_NA"] <- 'SL1'
allg$r_result[allg$r_result == "SL3_NPEV_NA"] <- 'SL3'
allg$r_result[allg$r_result == "SL3_NPEV_NEV"] <- 'SL3'
allg$r_result[allg$r_result == "SL3_NA_NA"] <- 'SL3'
allg$r_result[allg$r_result == "NA_NA_NA"] <- 'NA'
allg$r_result[allg$r_result == "NPEV_NA_NA"] <- 'NPEV'
allg$r_result[allg$r_result == "NPEV_NEV_NA"] <- 'NPEV'
allg$r_result[allg$r_result == "NEG_NA_NA"] <- 'NEG'
allg$r_result[grep("SL3 Discordant", allg$r_result)]<- 'VDPV3'
allg$r_result[grep("SL1 Discordant", allg$r_result)]<- 'VDPV1'
allg$r_result<-as.character(allg$r_result)  
allg <- allg[allg$r_result != "NA", ]
allg$r_result<-as.factor(allg$r_result)  

#make collection month and year columns
#allg[, "year"] <- format(allg[,"collection_date"], "%Y")
#allg[, "month"] <- format(allg[,"collection_date"], "%m")

#allg$collection_date <- as.character(allg$collection_date)
#allg$collection_date <- as.Date(allg$collection_date, format = "%m-%d-%Y")

#filter CAFE paper dates (beginning-Aug2019)
#allg <- allg[allg$collection_date <= as.Date("2019-08-31"),]

#filter only SL1 and SL1 Discordant
sl1 <- (allg[allg$r_result == "SL1" | allg$r_result == "VDPV1",])
sl1$r_result<-as.character(sl1$r_result) 
sl1$r_result[sl1$r_result == "SL1"] <- 'SL'
sl1$r_result[sl1$r_result == "VDPV1"] <- 'VDPV'
sl1$collection_date <- as.Date(sl1$collection_date, format = "%m/%d/%y")
sl3 <- (allg[allg$r_result == "SL3" | allg$r_result == "VDPV3",])
sl3$r_result<-as.character(sl3$r_result) 
sl3$r_result[sl3$r_result == "SL3"] <- 'SL'
sl3$r_result[sl3$r_result == "VDPV3"] <- 'VDPV'
sl3$collection_date <- as.Date(sl3$collection_date, format = "%m/%d/%y")
#combine SL1 and SL3 to same axis line
allg$r_result<-as.character(allg$r_result)
allg$r_result[allg$r_result == "SL1"] <- 'SL'
allg$r_result[allg$r_result == "SL3"] <- 'SL'
allg$r_result[allg$r_result == "VDPV1"] <- 'VDPV'
allg$r_result[allg$r_result == "VDPV3"] <- 'VDPV'

#collection_dates

allg$collection_date <- as.Date(allg$collection_date, format = "%m/%d/%y")


#graph
library(ggplot2)

resultcolors<-c(" " = "grey",
                "NEG" = "grey20",
                "NA" = "grey",
                "NPEV" = "blue", 
                #"SL1" = "light blue", 
                #"SL3" = "light blue", 
                "SL" = "light blue", 
                "SL1 & SL3" = "gold",
                "SL2" = "red",
                "VDPV" = "red" 
                #"VDPV3" = "red"
                )



methodcolors<-c("Filtration" = "black",
                "two_phase" = "grey")   

methodlabels<-c("Filtration" = "Filtration",
                "two_phase" = "Two-Phase")  
methodsize<-c("Filtration" = "1.5",
              "two_phase" = "1") 
methodsize<-as.numeric(methodsize)  
result_order <- c('NEG', 'NPEV', 'SL', 'SL1 & SL3', 'VDPV')

kthai<-ggplot(allg, aes(x=collection_date, y=factor(r_result, level=result_order), 
                        group=method, 
                        fill=r_result), na.rm=TRUE) +
  geom_line(aes(x=collection_date, y=factor(r_result, level=result_order), colour=method, size=method), na.rm=TRUE) +
  geom_point(shape = 21, size=4.5, color="white", show.legend=FALSE)+
  geom_point(data = sl1, aes(x = collection_date), shape = 49, size = 2.5, show.legend=FALSE)+
  geom_point(data = sl3, aes(x = collection_date), shape = 51, size = 2.5, show.legend=FALSE)+
  geom_hline(aes(yintercept="NPEV"), linetype="solid", 
             color = "blue", alpha=0.25, size=0.75)+
  scale_x_date(date_labels = "%b %Y", breaks="1 month", name='Collection Date')+
  scale_y_discrete(name = "Result", limits = c('NEG', 'NPEV', 'SL', 'SL1 & SL3', 'VDPV'))+
  scale_colour_manual(name='Method', values = methodcolors, labels = methodlabels) +
  scale_fill_manual(values = resultcolors) +
  scale_size_manual(values=methodsize, guide = FALSE)+
  #scale_shape_manual(values = c('SL1'), shape = 49) +
  theme_bw()+
  theme( 
    axis.text = element_text( size = 20 ),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size = 9, angle = 0),
    axis.title = element_text( size = 14),
    strip.text.x = element_text( size = 12),
    strip.text.y = element_text( size = 12, angle =0),
    strip.background.x = element_rect(size=.2),
    legend.text = element_text( size = 14, angle =0),
    legend.title = element_text( size = 14, angle =0),
    legend.position="bottom",
    panel.border = element_rect(colour="black"),
    panel.spacing = unit(0, "mm"),
    strip.background = element_rect(fill = "white", colour="black")
    )  +
  guides(color=guide_legend(ncol=1))+
  facet_grid(city+site~., space="free", scales="free_y") +labs(caption = paste(Sys.time()))

kthai

#save PNG graph to Environmental Surveillance folder
setwd("//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Kim/R/cafe_paper")





ggsave("Thailand_hanen.png", dpi = 500, height = 7, width = 11, units = "in")



#END SCRIPT##########################################################################
