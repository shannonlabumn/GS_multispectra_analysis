## loading packages

library(tidyverse)
library(corrplot)
library(BGLR)
library(ggbiplot)

## external scripts
source("./code/specCval.R")# cross validation using the TRN/TST partition
source("./code/fitGSmodel.R")# GS Model fit for each stage with kernels and returns accuracy results(within test set)


(start.time <- Sys.time())
## STAGE 7 ALL TIMEPOINT COMBINED
print("starting stage 7 modeling: all timepoint combined")

s7 <- modfit(pheno = "./output/Y1.chipstraits.rdata", ETAs = "./data/ETA.chips.S7.rda", stage = 7, class = "chips" )

## STAGE 8 selected TIMEPOINT with genetic algorithm
print("starting stage 8 modeling: selected timepoint combined")
s8 <- modfit(pheno = "./output/Y1.chipstraits.rdata", ETAs = "./data/ETA.chips.S8.rda", stage = 8, class = "chips" )

## STAGE 9 selected TIMEPOINT with simulation anealing
print("starting stage 9 modeling: selected timepoint combined")
s9 <- modfit(pheno = "./output/Y1.chipstraits.rdata", ETAs = "./data/ETA.chips.S9.rda", stage = 9, class = "chips" )

## STAGE 10 selected TIMEPOINT with genetic algorithm
print("starting stage 10 modeling: selected timepoint combined")
s10 <- modfit(pheno = "./output/Y1.chipstraits.rdata", ETAs = "./data/ETA.chips.S10.rda", stage = 10, class = "chips" )

(end.time <- Sys.time())
(end.time - start.time)


Gall.cor <- rbind(s7,s8,s9,s10)
write.csv(Gall.cor,"./output/SelvarpredAstageschips.csv",quote = F, row.names = F)
plotall <- sel_c[-c(121:240),]  %>% # sel_c %>%filter(stage==c("7","9","10"))
  mutate(traits = factor(key, levels = c("yield")), stage=factor(stage,levels=c("7","9","10"), 
                                                                                  labels=c("combined stages 66 vars",
                                                                                           "SA selected 26 vars","SF selected 46 vars")),
         model = factor(model, levels = unique(model))) %>% 
  ggplot(aes(x = model, y = PA, fill = model)) +
  geom_boxplot( stat = "boxplot",  outlier.colour = "red", outlier.shape = 16,
                outlier.size = 0.5, na.rm=TRUE) +
  labs(x= bquote("Relationship Matrix"), y= "Prediction Ability") + theme_bw() + facet_grid( ~ stage) +
  theme(axis.text = element_text(face = "bold",size = 10), axis.title = element_text(face = "bold", size=14), legend.title = element_text(face = "bold", size=12),legend.text = element_text(face = "bold", size=8), strip.text = element_text(size=16,face="bold"))
#G1.cor <- rbind(modelG1$yield$cval,modelG1$sg$cval,modelG1$roundness$cval)
ggsave("./output/Selvarstage_allmodelschips.png",height=10.5, width=18.5, units="in", dpi=300)
