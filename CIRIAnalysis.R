# [PHYSINT] Physical Integrity Rights Index: This is an additive index 
# constructed from the Torture, Extrajudicial Killing, Political Imprisonment, and Disappearance indicators.
#   Value: It ranges from 0 (no government respect for these 
#   four rights) to 8 (full government respect for these four rights).

# [DISAP] Disappearance: Disappearances are cases in which people
# have disappeared, political motivation appears likely, and the
# victims have not been found.
#   Value: 0  - indicates that disappearances have occurred
#   frequently in a given year; 1 - indicates that disappearances
#   occasionally occurred; 2 - indicates that disappearances
#   did not occur in a given year.

# [KILL] Extrajudicial Killing: Extrajudicial killings are killings
# by government officials without due process of law.
#   Value: 0 - indicates that extrajudicial killings were practiced 
#   frequently in a given year; 1 - indicates that extrajudicial 
#   killings were practiced occasionally; 2 - indicates that such 
#   killings did not occur in a given year.

#Import Packages
library(corrplot)
library(vcd)
library(corrgram)
library(reshape)
library(ggplot2)
setwd('Desktop/humanrights')

# Importing the CIRI dataset
ciri <- read.csv("CIRI_raw_data.csv")

#### Data Cleaning for CIRI #####
#Remove useless columns
ciri <- ciri[, c(1:2, 9:ncol(ciri))]

#Change numeric variables to factors
# for (col in 2:ncol(ciri)) {
#   ciri[,col] <- as.factor(ciri[,col])
# }

#Remove rows with NAs
ciri <- ciri[complete.cases(ciri),]

#Write cleaned data to CSV
# write.csv(ciri, file="truncData.csv");

#### Visualizing Correlations ####
#See correlations
dataC <- ciri[,3:(dim(ciri)[2])]
corr <- cor(dataC)
corrplot(corr, method="pie")

# Importing the U.S. Overseas Loans and Grants dataset
usgrants <- read.csv("Total_Economic_and_Military_Assistance_1946-2014.csv")

#### Data Cleaning for Military Grants ####
usgrants$Obligations..Constant.Dollars. <- as.numeric(gsub(",","",as.character(usgrants$Obligations..Constant.Dollars.)))

#Start Fixing Issues w/ Naming
levels(usgrants$Country)[levels(usgrants$Country) == "Burma (Myanmar)"] <- "Burma"
levels(usgrants$Country)[levels(usgrants$Country) == "Micronesia (Federated States of)"] <- "Micronesia, Federated States of"
levels(usgrants$Country)[levels(usgrants$Country) == "Bahamas, The"] <- "Bahamas"
levels(usgrants$Country)[levels(usgrants$Country) == "Korea, Republic of"] <- "Korea, South"
levels(usgrants$Country)[levels(usgrants$Country) == "Serbia"] <- "Serbia and Montenegro"
levels(usgrants$Country)[levels(usgrants$Country) == "Trinidad & Tobago"] <- "Trinidad and Tobago"
levels(usgrants$Country)[levels(usgrants$Country) == "Macedonia, Former Yugoslav Republic"] <- "Macedonia"
levels(usgrants$Country)[levels(usgrants$Country) == "St. Vincent and Grenadines"] <- "Saint Vincent and the Grenadines"
levels(usgrants$Country)[levels(usgrants$Country) == "St. Vincent and Grenadines"] <- "Saint Vincent and the Grenadines"
#Merge the Chinas and Congos

#Subset to 2004
usgrants0414 <- usgrants[usgrants$Fiscal.Year >= 2004, ] 
military0414 <- usgrants0414[usgrants0414$Assistance.Category == "Military",]
militarySum0414 <- aggregate(Obligations..Constant.Dollars. ~ Country, military0414, sum)

#Get the Countries that don't match between the datasets
setDiff <- unique(ciri$CTRY)[!unique(ciri$CTRY) %in% unique(militarySum0414$Country)]

#Sort the military spending data
sortedMilitary <- militarySum0414[order(-militarySum0414$Obligations..Constant.Dollars.),]

#Take the top n rows
n = 20
sortedSubset = sortedMilitary[1:n,]

#Plot the Physical Rights Index]
## This ended up not being very helpful since it's
## hard to see what is going in the graph
# ciriSet = NULL
# for(country in sortedSubset$Country) {
#   ciriPRI = ciri[ciri$CTRY == country,]
#   ciriSet = rbind(ciriSet, ciriPRI)
# }
# ggplot(data = ciriSet, aes(x=ciriSet$YEAR, y=ciriSet$PHYSINT)) + geom_line(aes(colour=ciriSet$CTRY))

# Plot the Summative Index
# This generally shows that there isn't much relation
# between human rights and funding
ciriSet = NULL
for(country in sortedSubset$Country) {
  ciriPRI = ciri[ciri$CTRY == country,]
  ciriSet = rbind(ciriSet, ciriPRI)
}
ggplot(data = ciriSet, aes(x=ciriSet$YEAR, y=50*((ciriSet$PHYSINT/8)+(ciriSet$NEW_EMPINX/14)))) + geom_line(aes(colour=ciriSet$CTRY))

#Add new human rights score to the dataset
ciriSet$Summary <- 50*((ciriSet$PHYSINT/8)+(ciriSet$NEW_EMPINX/14))

#Aggregate data with Human Rights Scores and Money
allAggregate <- aggregate(ciriSet$Summary, list(Country=ciriSet$CTRY), FUN=median)
names(allAggregate)[names(allAggregate)=="x"] <- "Median.Human.Rights.Summary"
sds <- aggregate(ciriSet$Summary, list(Country=ciriSet$CTRY), FUN=sd)
allAggregate$SD <- sds$x
allAggregate <- merge(allAggregate, sortedSubset, by = intersect(names(allAggregate), names(sortedSubset)))

#Sort the aggregated data
sortedAggregate <- allAggregate[order(-allAggregate$Obligations..Constant.Dollars.),]

