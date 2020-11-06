


setwd("//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Kim/R/cafe_paper")

#read in data from Environmental Surveillance spreadsheet
setwd("//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Environmental/R_analysis")

library(xlsx)
eolddata <- read.xlsx("allsamples_envsdata.xlsx", sheetName = "envsdata_2016-2019", header=TRUE)
enewdata <- read.xlsx("allsamples_envsdata.xlsx", sheetName = "2020_envs data", header=TRUE)

#filter only guatemala samples
names(eolddata)[names(eolddata) == "pH"] <- "collection_pH"
gtmold<-(eolddata[eolddata$country == "GTM",])
gtmnew<-(enewdata[enewdata$country == "GTM",])

#combine old (2019 or past) with new (2020) data
allg <- merge(gtmold, gtmnew, by.x = c("country", "city", "site", "site_name", "lat", "long", "sample_id", "dash", "collection_date", "collection_time", "method", "processor1", "processor2", "collection_temp", "collection_pH", "volume_filtered", "conc_factor", "processing_date", "report_date", "result1", "result2", "result3"),
              by.y = c("country", "city", "site", "site_name", "lat", "long", "sample_id", "dash", "collection_date", "collection_time", "method", "processor1", "processor2", "collection_temp", "collection_pH", "volume_filtered", "conc_factor", "processing_date", "report_date", "result1", "result2", "result3"),
              all.x=TRUE, all.y=TRUE)

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
allg[, "year"] <- format(allg[,"collection_date"], "%Y")
allg[, "month"] <- format(allg[,"collection_date"], "%m")

#filter CAFE paper dates (beginning-Aug2019)
allg <- allg[allg$collection_date <= as.Date("2019-08-31"),]
allg$collection_date 

#filter only SL1 and SL1 Discordant
sl1 <- (allg[allg$r_result == "SL1" | allg$r_result == "VDPV1",])
sl1$r_result<-as.character(sl1$r_result) 
sl1$r_result[sl1$r_result == "SL1"] <- 'SL'
sl1$r_result[sl1$r_result == "VDPV1"] <- 'VDPV'
sl3 <- (allg[allg$r_result == "SL3" | allg$r_result == "VDPV3",])
sl3$r_result<-as.character(sl3$r_result) 
sl3$r_result[sl3$r_result == "SL3"] <- 'SL'
sl3$r_result[sl3$r_result == "VDPV3"] <- 'VDPV'

#combine SL1 and SL3 to same axis line
allg$r_result<-as.character(allg$r_result)
allg$r_result[allg$r_result == "SL1"] <- 'SL'
allg$r_result[allg$r_result == "SL3"] <- 'SL'
allg$r_result[allg$r_result == "VDPV1"] <- 'VDPV'
allg$r_result[allg$r_result == "VDPV3"] <- 'VDPV'

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

result_order <- c('NEG', 'NPEV', 'SL', 'SL1 & SL3', 'VDPV')

kgtml<-ggplot(allg, aes(x=collection_date, y=factor(r_result, level=result_order), 
                        group=method, 
                        fill=r_result), na.rm=TRUE) +
  geom_line(aes(x=collection_date, y=factor(r_result, level=result_order), colour=method), size=1, na.rm=TRUE) +
  geom_point(shape = 21, size=4.5, color="white", show.legend=FALSE)+
  geom_point(data = sl1, aes(x = collection_date), shape = 49, size = 2.5, show.legend=FALSE)+
  geom_point(data = sl3, aes(x = collection_date), shape = 51, size = 2.5, show.legend=FALSE)+
  geom_hline(aes(yintercept="NPEV"), linetype="solid", 
             color = "blue", alpha=0.25, size=0.75)+
  scale_x_date(date_labels = "%b\n%Y", 
               date_breaks="1 month", 
               date_minor_breaks = "1 month",
               limits = c(as.Date("2018-10-31"), as.Date("2019-08-14")),
               #breaks = allg$collection_month,
               #breaks = unique(allg$collection_date
                               #, fromLast = FALSE
                               #),
               name='Collection Date'
               #expand = c(0,0)
               )+
  scale_y_discrete(name = "Result", limits = c('NEG', 'NPEV', 'SL', 'SL1 & SL3', 'VDPV'))+
  scale_colour_manual(name='Method', values = methodcolors, labels = methodlabels) +
  scale_fill_manual(values = resultcolors) +
  #scale_shape_manual(values = c('SL1'), shape = 49) +
  theme_bw()+
  theme( 
    axis.text = element_text( size = 20 ),
    axis.text.x = element_text(size = 10, angle = 0),
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
  facet_grid(city+site~., space="free", 
             scales="free_y") +labs(caption = paste(Sys.time()))

kgtml

#save PNG graph to Environmental Surveillance folder
setwd("//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Kim/R/cafe_paper")




ggsave("GTM_hanen_dates.png", dpi = 500, height = 8, width = 10, units = "in")



#END SCRIPT##########################################################################
