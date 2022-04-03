rm(list=ls())

library(ggplot2); library(rworldmap); library(maps); library(ggmap); library(ggthemes); library(ggpubr)

setwd("C:/Users/amorg/Documents/PhD/Chapter_1/Chapter1_Analysis")
review <- read.csv("C:/Users/amorg/Documents/PhD/Chapter_1/Chapter1_Analysis/New_List_noresidue_data.csv")


data.frame(table(review$Country_of_Origin))
data.frame(table(review$Modelled_Country))

#####Publication Date Analysis#####
yearscount <- data.frame(table(review$Publication_Date))
tmp <- data.frame(years = seq(2000, 2021, 1), Freq=0)

yearscount2 <- merge(tmp,yearscount, by.x="years", by.y="Var1", all=TRUE)
tmp <-NULL; yearscount2$Freq.x <- NULL
yearscount2[is.na(yearscount2$Freq.y),]$Freq.y <- 0
yearscount2$cum <- cumsum(yearscount2$Freq)

freq_model <- lm(cum ~ years, yearscount2)
summary(freq_model)

p_years_cum <- ggplot(yearscount2, aes(x = years, y = cum)) + 
  geom_line(aes(y = predict(freq_model, yearscount2), x = years), colour = "purple", size = 1.2, alpha = 0.5, lty = 2) + 
  geom_line(size = 1.3) + theme_bw() +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 30)) + 
  labs(y="Cumulative Publication Frequency", x = "Year") + 
  theme(plot.title = element_text(size=18), legend.text=element_text(size=15), axis.text=element_text(size=15), 
        axis.title.y=element_text(size=15), axis.title.x= element_text(size=15), plot.margin = unit(c(1,1,1,1), "cm"),
        legend.position="bottom") 


# Risk Anal + Dyn Regression ----------------------------------------------

review_rdyn <- review[c(3,22,23)]
review_rdyn_data <- data.frame(matrix(nrow= 0, ncol = 2))

for(i in 1:nrow(review_rdyn)) {
  tmp <- data.frame("year" = review_rdyn[i,1],
                    "model" = sapply(1:2, function(x) review_rdyn[i,(1+x)])
  )
  review_rdyn_data <- rbind(review_rdyn_data, tmp)
}

review_rdyn_data <- review_rdyn_data[!(review_rdyn_data$model == ""),]

table(review_rdyn_data)

#Risk Anal
yearscount_anal <- data.frame(table(review_rdyn_data[review_rdyn_data$model == "Risk Assessment",]$year))
tmp <- data.frame(years = seq(2000, 2021, 1), Freq=0)

yearscount2_anal <- merge(tmp,yearscount_anal, by.x="years", by.y="Var1", all=TRUE)
yearscount2_anal$Freq.x <- NULL
yearscount2_anal[is.na(yearscount2_anal$Freq.y),]$Freq.y <- 0
yearscount2_anal$cum <- cumsum(yearscount2_anal$Freq)

freq_model_anal <- lm(cum ~ years, yearscount2_anal)
summary(freq_model_anal)

data_model_anal <- data.frame("pred" = predict(freq_model_anal, yearscount2_anal),
                             "years" = yearscount2_anal$years)

#Dynamic Model
yearscount_dyn <- data.frame(table(review_rdyn_data[review_rdyn_data$model == "Dynamic Epidemiological Model",]$year))

yearscount_dyn <- merge(tmp, yearscount_dyn, by.x="years", by.y="Var1", all=TRUE)
tmp <-NULL; yearscount_dyn$Freq.x <- NULL
yearscount_dyn[is.na(yearscount_dyn$Freq.y),]$Freq.y <- 0
yearscount_dyn$cum <- cumsum(yearscount_dyn$Freq)

freq_model_dyn <- lm(cum ~ years, yearscount_dyn)
summary(freq_model_dyn)

data_model_dyn <- data.frame("pred" = predict(freq_model_dyn, yearscount_dyn),
                             "years" = yearscount_dyn$years)

#Plotting

anal_data <- yearscount2_anal; anal_data$group <- "Risk Assessment"
dyn_data <- yearscount_dyn; dyn_data$group <- "Dynamic Epidemiological Model"

comb_data <- rbind(anal_data,dyn_data)

supp_time <- ggplot(comb_data, aes(y = cum, x = years, color = group)) + geom_line(size = 1.2, alpha = 0.5) + 
  geom_line(data = data_model_anal, aes(y = pred, x = years), colour = "purple", size = 1.2, alpha = 0.5, lty = 2, inherit.aes = F) +
  geom_line(data = data_model_dyn, aes(y = pred, x = years), colour = "purple", size = 1.2, alpha = 0.5, lty = 2, inherit.aes = F) + 
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 21)) + 
  labs(y="Cumulative Publication Frequency", x = "Year", color = "") + 
  theme(plot.title = element_text(size=18), legend.text=element_text(size=15), axis.text=element_text(size=15), 
        axis.title.y=element_text(size=15), axis.title.x= element_text(size=15), plot.margin = unit(c(1,1,1,1), "cm"),
        legend.position="bottom") 

ggsave(supp_time, filename = "risk_dyn_time.png", dpi = 300, type = "cairo", width = 10, height = 8, units = "in",
       path = "C:/Users/amorg/Documents/PhD/Chapter_1/Figures")


#####Country Analysis#######
country <- data.frame(table(review$Country_of_Origin))
country$lat <- c(50.5039, 56.1304, 35.8617, 56.2639, 46.2276, 51.1657, 52.1326, 
                 -30.5595, 55.3781, 37.0902)
country$lon <- c(4.4699, -106.3468, 104.1954, 9.5018, 2.2137, 10.4515, 5.2913, 
                 22.9375, -3.4360, -95.7129)

world <- map_data("world")

p_world <- ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="gray80")+
  borders("world", colour = "gray87")+ theme_bw() +
  coord_cartesian(ylim = c(-50, 90)) +
  geom_point(data=country, aes(x=lon, y=lat, size = Freq), colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(2, 20), 
                        breaks = c(1, 3, 5, 10, 14)) + 
  labs(size = 'Study Origin Frequency', x = NULL, y = NULL) + 
  theme(legend.text=element_text(size=15), legend.title=element_text(size=15), plot.margin = unit(c(1,1,1,1), "cm"),
        axis.text.x = element_blank(),axis.text.y = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position = "bottom") 
  
# Comb Plot - Frequency and Map ---------------------------------------------------------------

p_year_world <- ggarrange(p_years_cum, p_world, labels = c("A", "B"), widths = c(0.4, 0.6), font.label = list(size = 20))
p_year_world <- ggarrange(p_years_cum, p_world, labels = c("A", "B"), widths = c(0.4, 0.6), ncol = 1, nrow = 2, font.label = list(size = 20))

p_year_world <- ggarrange(p_years_cum, supp_time, p_world,
                          labels = c("A", "B", "C"), 
                          widths = c(0.4, 0.6), ncol = 1, nrow = 3, font.label = list(size = 20))


#ggsave(p_year_world, filename = "pub_freq_world.png", dpi = 300, type = "cairo", width = 13, height = 6, units = "in",
#       path = "C:/Users/amorg/Documents/PhD/Chapter_1/Figures")
#ggsave(p_year_world, filename = "pub_freq_world.png", dpi = 300, type = "cairo", width = 8, height = 10, units = "in",
#       path = "C:/Users/amorg/Documents/PhD/Chapter_1/Figures")

ggsave(p_year_world, filename = "pub_freq_world_split.png", dpi = 300, type = "cairo", width = 8, height = 14, units = "in",
              path = "C:/Users/amorg/Documents/PhD/Chapter_1/Figures")

# Drug/Bug Combo Analysis ----------------------------------------------------------

#Non-Normalised 
drug_bug <- review[,6:13]

bac_modelled <- c(review$Bacteria_Modelled_1, review$Bacteria_Modelled_2, review$Bacteria_Modelled_3, review$Bacteria_Modelled_4) 
bac.frame <- data.frame(table(bac_modelled))[-1,]

antibiotic_modelled <- c(review$Drug_Class_1, review$Drug_Class_2, review$Drug_Class_3, review$Drug_Class_4)
drug_frame <- data.frame(table(antibiotic_modelled))

drug_bug_data <- data.frame(matrix(nrow= 0, ncol = 2))

for(z in 1:4){
  for(i in 1:nrow(drug_bug)) {
    tmp <- data.frame("bug" = drug_bug[i,z],
                      "drugs" = sapply(0:3, function(x) drug_bug[i,(5+x)])
               )
    drug_bug_data <- rbind(drug_bug_data, tmp)
  }
}

drug_bug_data <- drug_bug_data[!(drug_bug_data$drugs == "" | drug_bug_data$bug == ""),]

bac_res_data <- data.frame(proportions(table(drug_bug_data), 1))

for(i in 1:length(unique(bac_res_data$bug))) {
  bac_res_data[bac_res_data$bug == unique(bac_res_data$bug)[i],]$Freq <- bac_res_data[bac_res_data$bug == unique(bac_res_data$bug)[i],]$Freq * 
    bac.frame[bac.frame$bac_modelled == unique(bac.frame$bac_modelled)[i],]$Freq
}

bac_res_data[bac_res_data$bug == unique(bac_res_data$bug)[2],]

bac_res_data$drugs <- as.character(bac_res_data$drugs)

for(i in 1:length(names(table(bac_res_data$drugs)))){
  bac_res_data$drugs[which(bac_res_data$drugs == drug_frame$antibiotic_modelled[-1][i])] <- paste0(drug_frame$antibiotic_modelled[-1][i], " (n = ", 
                                                                                                 drug_frame$Freq[-1][[i]], "/27)")
}


#Normalised 

bac_res_norm <- data.frame(proportions(table(drug_bug_data), 1))

bac_res_norm$drugs <- factor(bac_res_norm$drugs, levels = c(as.character(unique(bac_res_norm$drugs)[-4]), "Not Specified"))
bac_res_norm$bug <- factor(bac_res_norm$bug, levels = c(as.character(unique(bac_res_norm$bug)[-4]), "Not Specified"))

p_drug_norm <- ggplot(bac_res_norm, aes(fill = drugs, x =  bug, y = Freq)) +
  geom_col(color = "black",position= "stack") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) + theme_bw() + 
  labs(fill = "Antibiotic Resistance", y = "Proportion", x = NULL) + 
  theme(legend.text=element_text(size=15), legend.title=element_text(size=15), plot.margin = unit(c(1,1,1,1), "cm"),
        axis.text = element_text(size=15), axis.title = element_text(size=15),legend.position = "right")

bac_res_data$drugs <- factor(bac_res_norm$drugs, levels = c(as.character(unique(bac_res_norm$drugs)[-4]), "Not Specified"))
bac_res_data$bug <- factor(bac_res_norm$bug, levels = c(as.character(unique(bac_res_norm$bug)[-4]), "Not Specified"))

p_drug_bug <- ggplot(bac_res_data, aes(fill = drugs, x =  bug, y = Freq)) +
  geom_col(color = "black",position= "stack") + 
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,10, by = 2), limits = c(0, 10)) + theme_bw() + 
  labs(fill = "Antibiotic Resistance", y = "Frequency", x = NULL) + 
  theme(legend.text=element_text(size=15), legend.title=element_text(size=15), plot.margin = unit(c(1,1,1,1), "cm"),
        axis.text = element_text(size=15), axis.title = element_text(size=15),legend.position = "right")



names(table(drug_bug_data))


#Plotting 

p_drug <- ggarrange(p_drug_bug, p_drug_norm, ncol = 1, nrow = 2, labels = c("A", "B"), common.legend = TRUE, legend = "bottom",
                    font.label = c(size = 20))

ggsave(p_drug, filename = "p_drug.png", dpi = 300, width = 11, height = 14, units = "in",
       path = "C:/Users/amorg/Documents/PhD/Chapter_1/Figures")


# Model Structure ---------------------------------------------------------
table(review$Type_Model_1)
table(review$Type_Model_2)
table(review$Compart_Model_Structure)

review[review$Type_Model_1 == "Dynamic Epidemiological Model"]

review[review$Sens_Anal == "Sensitivity analysis conducted"]
#Risk Assessment 
mod_struct <- review[,35:39]
colSums(mod_struct)

# Populations and Pathways Modelled ----------------------------------------------------

#Populations
table(review$Food_Pop_Modelled)
table(review$Hum_Pop_Modelled)
table(review$Livestock_Pop_Modelled)
table(review$Env_Pop_Modelled)


#Pathways

path_mod <- c(review[,28], review[,29]) 

table(path_mod)


# Data Used ---------------------------------------------------------------

table(review$Sec_Data_Source_Surv)
table(review$Sec_Data_Source_Epi)
table(review$Sec_Data_Source_Experiment)
table(review$Data_Expert_Opin)

# Colonisation and Curtailment --------------------------------------------

table(review$AMR_Col_Hum)
table(review$AMR_Col_Anim)
table(c(review$AMR_Col_Food_1, review$AMR_Col_Food_2))
table(review$AMR_Col_Env)
table(review$Antibiot_Consum_Hum)
table(review$Antibiot_Consum_Anim)
table(review$Curtail_Mod_Hum_Res)
table(review$Curtail_Mod_Hum_Sens)
table(review$Curtail_Mod_Live_Res)
table(review$Curtail_Mod_Live_Sens)

# Sensitivity and Validation ----------------------------------------------

table(review$Uncert_Anal)
table(review$Sens_Anal)
table(review$Validation)
