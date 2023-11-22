library(tidyverse)
library(dplyr)

al16 <- read.csv("~/al16.csv")
al17 <- read.csv("~/al17.csv")
al20 <- read.csv("~/al20.csv")
al22 <- read.csv("~/al22.csv")

al16$PREC_nospace <- gsub(" ", "", al16$id)
al16$PREC_nospace <- gsub("\\d+", "", al16$PREC_nospace)
al17$PREC_nospace <- gsub(" ", "", al17$id)
al17$PREC_nospace <- gsub("\\d+", "", al17$PREC_nospace)
al20$PREC_nospace <- gsub(" ", "", al20$id)
al20$PREC_nospace <- gsub("\\d+", "", al20$PREC_nospace)
al22$PREC_nospace <- gsub(" ", "", al22$id)
al22$PREC_nospace <- gsub("\\d+", "", al22$PREC_nospace)
##Should have 107 variables, 8944 observations in total, but in general, it should be 2070 + 225 additional observations

df_list <- list(al16, al17, al20, al22)
merged <- df_list %>% reduce(full_join, by = "PREC_nospace")

all2 <- merge(al16, al17, by = "PREC_nospace", all = TRUE)
all3 <- merge(al20, al22, by = "PREC_nospace", all = T)

all4 <- merge(all2, all3, by = "PREC_ nospace", all = T)

all2 <- subset(all2, select = -c(id.x, id.y))


alltry2 <- all2 %>%
  group_by(PREC_nospace) %>%
  summarise_all(funs(sort(.)[1]))

alltry2$PREC_nospace[duplicated(alltry2$PREC_nospace)] #detect duplicates

group_list <- as.factor(rep("group", nrow(all2)))

alltry2$PREC_nospace <- gsub("_", "", alltry2$PREC_nospace)
alltry2$PREC_nospace <- gsub("-\\d+", "", alltry2$PREC_nospace)

##Now for editing of the other dataframe
all3 <- subset(all3, select = -c(id.x, id.y))
all3 <- subset(all3, select = -c(year))

alltry3 <- all3 %>%
  group_by(PREC_nospace) %>%
  summarise_all(funs(sort(.)[1]))

alltry3$PREC_nospace <- gsub("_", "", alltry3$PREC_nospace)
alltry3$PREC_nospace <- gsub("-\\d+", "", alltry3$PREC_nospace)

alltry3$PREC_nospace[duplicated(alltry3$PREC_nospace)] #detect duplicates


##test merge for all years
all5 <- merge(alltry2, alltry3, by = "PREC_nospace", all = T)
all5 <- subset(all5, select = -c(X.x))

##Hardcoding and general cleaning
colnames(all5)[colnames(all5) == 'PREC_nospace'] <- 'id'

#Autauga needs to have the numbers removed!
all5$id <- gsub("AUTAUGA---100TRINITYMETHODIST", "AUTAUGA---TRINITYMETHODIST",
           gsub("AUTAUGA---100TRINITYMETHODIST", "AUTAUGA---TRINITYMETHODIST",
           gsub("AUTAUGA---10JONESCOMMUNITYCENTER", "AUTAUGA---JONESCOMMUNITYCENTER",              
           gsub("AUTAUGA---110CENTRALALELECTRIC", "AUTAUGA---CENTRALALELECTRIC",                  
           gsub("AUTAUGA---140AUTAUGAVILLEVFD", "AUTAUGA---AUTAUGAVILLEVFD",                      
           gsub("AUTAUGA---150PRATTMONTBAPTISTCHURCH", "AUTAUGA---PRATTMONTBAPTISTCHURCH",              
           gsub("AUTAUGA---160DOSTERCOMMUNITYCENTER", "AUTAUGA---DOSTERCOMMUNITYCENTER",               
           gsub("AUTAUGA---170CAMELLIABAPTISTCHURCH", "AUTAUGA---CAMELLIABAPTISTCHURCH",                
           gsub("AUTAUGA---180OLDKINGSTONVFD", "AUTAUGA---OLDKINGSTONVFD",                      
           gsub("AUTAUGA---20HERITAGEBAPTISTCHURCH", "AUTAUGA---HERITAGEBAPTISTCHURCH",         
           gsub("AUTAUGA---220UPPERKINGSTON", "AUTAUGA---UPPERKINGSTON",                       
           gsub("AUTAUGA---30BILLINGSLEYCOMMUNITYCENTER", "AUTAUGA---BILLINGSLEYCOMMUNITYCENTER",             
           gsub("AUTAUGA---40EMEMORIALCHRISTIAN", "AUTAUGA---EMEMORIALCHRISTIAN",                  
           gsub("AUTAUGA---50WHITECITYVFD", "AUTAUGA---WHITECITYVFD",                          
           gsub("AUTAUGA---60MARBURYMIDDLESCHOOL", "AUTAUGA---MARBURYMIDDLESCHOOL",                  
           gsub("AUTAUGA---70BOONE'SCHAPEL", "AUTAUGA---BOONE'SCHAPEL",                       
           gsub("AUTAUGA---80INDEPENDENCEVFD", "AUTAUGA---INDEPENDENCEVFD",                      
           gsub("AUTAUGA---90BOOTHVFD", "AUTAUGA---BOOTHVFD", all5$id))))))))))))))))))

#Same with Jefferson
all5$id[grep("JEFFERSON", all5$id)] <- gsub("PREC[0-9]+-", "PREC-", all5$id[grep("JEFFERSON", all5$id)])

#Montgomery
all5$id[grep("MONTGOMERY", all5$id)] <- gsub("MONTGOMERY---[0-9]+", "MONTGOMERY---", all5$id[grep("MONTGOMERY", all5$id)])

#Tuscaloosa
all5$id <- gsub("TUSCALOOSA---STATION#CARROLLSCRK", "TUSCALOOSA---STATION#2CARROLLSCRK",
           gsub("TUSCALOOSA---STATION2CARROLLSCREEK", "TUSCALOOSA---STATION#2CARROLLSCRK", all5$id))

##attempts to aggregate
all5_test <- all5
all5_test <- head(all5_test, -1)
all5_test[] <- lapply(all5_test, function(x) {
  if(is.factor(x)) as.numeric(as.character(x))
  else x
})


all5_test <- all5_test %>% #combine all of the ids with the exact name, summing up the votes
  group_by(id) %>%
  summarize_all(~sum(., na.rm = T))

all5_test <- sapply(all5_test, class)
