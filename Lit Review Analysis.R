rm(list=ls())

library(ggplot2)
library(rworldmap)
library(maps)
library(ggmap)

windows.options(antialias = "cleartype")
setwd("//csce.datastore.ed.ac.uk/csce/biology/users/s1678248/PhD/Lit Reviews")
review <- read.csv("Review Sheet v8 Import Version.csv")
review <- review[-c(39:45),]
review <- review[,-c(45:150)]

#####Publication Date Analysis#####
yearscount <- data.frame(table(review$Publication.Date))

tmp <- data.frame(years = seq(1988, 2018, 1), Freq=0)

yearscount2 <- merge(tmp,yearscount, by.x="years", by.y="Var1", all=TRUE)
tmp <-NULL
yearscount2$Freq.x <- NULL

yearscount2[is.na(yearscount2$Freq.y),]$Freq.y <- 0

yearscount2$cum <- cumsum(yearscount2$Freq)
plot(yearscount2$years,yearscount2$cum, type = "s", xlim = c(1988,2018), ylab = "Cumulative Frequency", xlab = "Year of Publication", lwd = 2)

#####Country Analysis#######
country <- table(review$Country[1:38])
country <- country[-1]
country <- data.frame(country)
country$lat <- c(50.732328, 55.827498, 32.741081, 55.598920, 46.982259, 22.481635, 36.555374, 
                 52.094809, 23.447768, -30.467904, 52.651572, 38.905036)
country$lon <- c(4.722129, -101.605061, 105.569908, 11.606962, 2.568195, 79.567508, 
                 127.862090, 5.822146, 45.285775, 24.158074, -1.026508, -100.442332 )

world <- map_data("world")

ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="gray80")+
  borders("world", colour = "gray87")+ theme_map() +
  coord_cartesian(ylim = c(-50, 90)) +
  geom_point(data=country, aes(x=lon, y=lat, size = Freq), colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(2, 20), 
                        breaks = c(1, 5, 10, 15, 23)) + 
  labs(size = 'Study Origin Frequency', x = NULL, y = NULL)
  

#pie(country,names(country))

pie = ggplot(country, aes(x="", y=Freq, fill=Var1)) + geom_bar(stat="identity", width=1)
pie = pie + coord_polar("y", start=0) + geom_text(aes(label=Freq), position = position_stack(vjust = 0.5))
pie = pie + scale_fill_brewer(palette="Set3")
pie
#####Sensitivity Analysis####
sens <- table(review$Has.a.sensitivity.analysis.been.conducted.)
sens <- sens[-1]
prop.table(sens)

######Analysis for Livestock to Human Pathway Modelled#####

pathway <- data.frame(table(review$Livestock.to.Human.Transmission.Pathway.Modelled))
mergepath <- data.frame(table(review$Livestock.to.Human.Transmission.Pathway.Modelled..Secondary.))
new <- merge(pathway,mergepath, by.x="Var1", by.y="Var1", all=TRUE)
new <- new[!(new$Var1=="N/A" | (new$Freq.x == 0 & new$Freq.y == 0)),]
new[is.na(new)] <- 0
new$Combcol <- new$Freq.x + new$Freq.y

#####Populations Modelled#####

poptable <- data.frame(table(review$Livestock.Species.Modelled))
poptable1 <- data.frame(table(review$Livestock.Species.Modelled...Secondary))
poptable2 <- data.frame(table(review$Livestock.Species.Modelled...Tertiary))
poptable <- merge(poptable, poptable1, by.x = "Var1", by.y = "Var1", all = TRUE)
poptable <- merge(poptable, poptable2, by.x = "Var1", by.y = "Var1", all = TRUE)
poptable[is.na(poptable)] <- 0
poptable$comb <- poptable$Freq.x + poptable$Freq.y + poptable$Freq
poptable <- poptable[-c(1,9),]

#####Drug+Bugs Modelled #####

drug <- table(review$Drug.Class.Modelled)
drug1 <- table(review$Drug.Class.Modelled...Secondary)
drug <- merge(drug,drug1, by.x="Var1", by.y="Var1", all=TRUE)
drug[is.na(drug)] <- 0
drug$comb <- drug$Freq.x + drug$Freq.y
drug <- drug[-c(1,10),]

bacteria <- data.frame(table(review$Hazard.Bacteria.spp.))
bacteria1 <- data.frame(table(review$Hazard.Bacteria.spp....Secondary))
bacteria2 <- data.frame(table(review$Hazard.Bacteria.spp....Tertiary))
bacteria <- merge(bacteria,bacteria1, by.x="Var1", by.y="Var1", all=TRUE)
bacteria <- merge(bacteria,bacteria2, by.x="Var1", by.y="Var1", all=TRUE)
bacteria[is.na(bacteria)] <- 0
bacteria$comb <- bacteria$Freq.x + bacteria$Freq.y + bacteria$Freq 
bacteria <- bacteria[-c(1,10),]

##### Data Used ####

dataused <- data.frame(table(review$Data.Source.Used))
dataused1 <- data.frame(table(review$Data.Source.Used...Secondary))
dataused2 <- data.frame(table(review$Data.Source.Used...Tertiary))
dataused <- merge(dataused, dataused1, by.x="Var1", by.y="Var1", all=TRUE)
dataused <- merge(dataused, dataused2, by.x="Var1", by.y="Var1", all=TRUE)
dataused[is.na(dataused)] <- 0
dataused$comb <- dataused$Freq.x + dataused$Freq.y + dataused$Freq 
dataused <- dataused[-c(1,7),]
