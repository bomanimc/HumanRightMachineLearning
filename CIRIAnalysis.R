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




