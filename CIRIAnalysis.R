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

# Importing the CIRI dataset
ciri <- read.csv("CIRI_raw_data.csv")

#### Data Cleaning #####
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

# Importing the U.S. Overseas Loans and Grants
usgrants <- read.csv("Total_Economic_and_Military_Assistance_1946-2014.csv")

usgrants0414 <- usgrants[usgrants$Fiscal.Year >= 2004, ] 



