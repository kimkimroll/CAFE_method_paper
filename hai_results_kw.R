


setwd("//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Environmental/R_analysis")

library(xlsx)
eolddata <- read.xlsx("allsamples_envsdata.xlsx", sheetName = "envsdata_2016-2019", header=TRUE)
enewdata <- read.xlsx("allsamples_envsdata.xlsx", sheetName = "2020_envs data", header=TRUE)

#filter only guatemala samples
names(eolddata)[names(eolddata) == "pH"] <- "collection_pH"
haiold<-(eolddata[eolddata$country == "HAI",])
hainew<-(enewdata[enewdata$country == "HAI",])

#combine old (2019 or past) with new (2020) data
allg <- merge(haiold, hainew, by.x = c("country", "city", "site", "site_name", "lat", "long", "sample_id", "dash", "collection_date", "collection_time", "method", "processor1", "processor2", "collection_temp", "collection_pH", "volume_filtered", "conc_factor", "processing_date", "report_date", "result1", "result2", "result3"),
              by.y = c("country", "city", "site", "site_name", "lat", "long", "sample_id", "dash", "collection_date", "collection_time", "method", "processor1", "processor2", "collection_temp", "collection_pH", "volume_filtered", "conc_factor", "processing_date", "report_date", "result1", "result2", "result3"),
              all.x=TRUE, all.y=TRUE)

#make collection month and year columns
allg[, "year"] <- format(allg[,"collection_date"], "%Y")
allg[, "month"] <- format(allg[,"collection_date"], "%m")


#filter March 2017-current/Angie's paper(March2016-Feb2017)
#allg$collection_year <- as.numeric(allg$collection_year)

#allg <- allg[allg$collection_date >= as.Date("2017-03-01"),]

#allg <- allg[!(allg$collection_year == "2017"  & allg$collection_month == "January"), ]
#allg <- allg[!(allg$collection_year == "2017"  & allg$collection_month == "February"), ]

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
allg$r_result[allg$r_result == "NEV_NA_NA"] <- 'NEG'
allg$r_result[grep("SL3 Discordant", allg$r_result)]<- 'VDPV3'
allg$r_result[grep("SL1 Discordant", allg$r_result)]<- 'VDPV1'
allg$r_result<-as.character(allg$r_result)  
allg <- allg[allg$r_result != "NA", ]
allg$r_result<-as.factor(allg$r_result)  

#filter only current sites
#allg<-(allg[allg$site == "BRA" | allg$site == "PET" | allg$site == "AMA" | allg$site == "BNF" | allg$site == "BDC"| allg$site == "RRD" | allg$site == "RPA" | allg$site == "CMB" | allg$site == "IMP" | allg$site == "CRC",])

#filter only Filtration and two-phase results
allg<-(allg[allg$method == "Filtration" | allg$method == "two_phase",])


#filter CAFE paper dates (dec2017-Aug2019)
allg <- allg[allg$collection_date >= as.Date("2017-12-01"),]
allg <- allg[allg$collection_date <= as.Date("2019-06-30"),]



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

#remove NA's
allg<-allg[!is.na(allg$site), ]
sl1<-sl1[!is.na(sl1$site), ]
sl3<-sl3[!is.na(sl3$site), ]

#graph
library(ggplot2)

resultcolors<-c(" " = "grey",
                "NEG" = "grey20",
                "NA" = "grey",
                "NPEV" = "blue", 
                "SL" = "light blue", 
                #"SL3" = "light blue", 
                "SL1 & SL3" = "gold",
                "SL2" = "red",
                #"VDPV1" = "red", 
                "VDPV" = "red")

methodcolors<-c("Filtration" = "black",
                "two_phase" = "grey")   

methodlabels<-c("Filtration" = "Filtration",
                "two_phase" = "Two-Phase")  

result_order <- c('NEG', 'NPEV', 'SL3', 'SL1', 'SL1 & SL3'
                  #'VDPV3', 'VDPV1'
                  )

khail<-ggplot(allg, aes(x=collection_date, y=factor(r_result, level=result_order), group=method, fill=r_result), na.rm=TRUE) +
  geom_line(aes(x=collection_date, y=factor(r_result, level=result_order), colour=method), size=1, na.rm=TRUE) +
  geom_point(shape = 21, size=4.5, color="white", show.legend=FALSE, na.rm=TRUE)+
  geom_point(data = sl1, aes(x = collection_date), shape = 49, size = 2.5, show.legend=FALSE, na.rm=TRUE)+
  geom_point(data = sl3, aes(x = collection_date), shape = 51, size = 2.5, show.legend=FALSE, na.rm=TRUE)+
  geom_hline(aes(yintercept="NPEV"), linetype="solid", 
             color = "blue", alpha=0.25, size=0.75, na.rm=TRUE)+
  scale_x_date(date_labels = "%b\n%Y", breaks="1 month", name='Collection Date',
               limits = c(as.Date("2017-12-01"), as.Date("2019-06-28")),
               expand =  expand_scale(add = 2)
              )+
  scale_y_discrete(name = "Result", limits = c('NEG', 'NPEV', 'SL', 'SL1 & SL3'))+
  scale_colour_manual(name='Method', values = methodcolors, labels = methodlabels) +
  scale_fill_manual(values = resultcolors) +
  theme_bw()+
  theme( 
    axis.text = element_text( size = 20 ),
    axis.text.x = element_text(size = 14, angle = 0),
    axis.text.y = element_text(size = 9, angle = 0),
    axis.title = element_text( size = 14),
    strip.text.x = element_text( size = 12),
    strip.text.y = element_text( size = 12, angle =0, lineheight = 20),
    strip.background.x = element_rect(size=.2),
    legend.text = element_text( size = 14, angle =0),
    legend.title = element_text( size = 14, angle =0),
    legend.position="bottom",
    panel.border = element_rect(colour="black"),
    panel.spacing = unit(0, "mm"),
    strip.background = element_rect(fill="white", colour="black"))  +
  guides(color=guide_legend(ncol=1))+
  facet_grid(city+site~.,
             #space="free", 
             #scales="free_y"
             ) +labs(caption = paste(Sys.time()))

khail

setwd("//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Kim/R/cafe_paper")

ggsave("HAI_hanen_ev_dates.png", dpi = 500, height = 9, width = 12, units = "in")


