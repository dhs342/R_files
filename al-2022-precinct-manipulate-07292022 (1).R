library(dplyr)
library(tidyverse)
library(tidyr)
library(stringr)
library(lubridate)
library(rio)
library(data.table)
library(readxl)


pth <- "/Users/danie/Downloads/2022 Primary Precinct Results"
setwd(pth)

fl.list <- list.files(path = pth)

dta.all <- list()

for(i in 1:length(fl.list)){
  hold <- read_excel(fl.list[i])
  hold.sub <- hold[hold$'Contest Title' %in% c("UNITED STATES SENATOR",
                                             "REGISTERED VOTERS - TOTAL"),]
  J <- dim(hold.sub)[1]
  for(j in 1:J){
    hold.sub$Contest[j] <- gsub(" ","",paste(hold.sub$Contest.Title[j],hold.sub$Party[j],hold.sub$Candidate[j],collapse="."),fixed=T)
  }
  hold.sub$Contest.Title <- hold.sub$Party <- hold.sub$Candidate <- NULL
  clnames <- hold.sub$Contest
  hold.sub$Contest <- NULL
  precinct.names <- colnames(hold.sub)
  hold.transposed <- transpose(hold.sub)
  colnames(hold.transposed) <- clnames
  rownames(hold.transposed) <- precinct.names
  dta.all[[i]] <- hold.transposed
}
output <- dta.all

#output <- lapply(dta.all, t)

#output <- lapply(output, function(x){
 # if(any(class(x)=="matrix")) as.data.frame(x) 
  #else x
 # })

## Create the column for Autauga, and then repeat for each dataframe.
output[[1]] <- output[[1]] %>%
  mutate(County = "Autauga", FIPS = "1001")
output[[1]] <- output[[1]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[2]] <- output[[2]] %>%
  mutate(County = "Baldwin", FIPS = "1003")
output[[2]] <- output[[2]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[3]] <- output[[3]] %>%
  mutate(County = "Barbour", FIPS = "1005")
output[[3]] <- output[[3]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[4]] <- output[[4]] %>%
  mutate(County = "Bibb", FIPS = "1007")
output[[4]] <- output[[4]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[5]] <- output[[5]] %>%
  mutate(County = "Blount", FIPS = "1009")
output[[5]] <- output[[5]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[6]] <- output[[6]] %>%
  mutate(County = "Bullock", FIPS = "1011")
output[[6]] <- output[[6]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[7]] <- output[[7]] %>%
  mutate(County = "Butler", FIPS = "1013")
output[[7]] <- output[[7]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[8]] <- output[[8]] %>%
  mutate(County = "Calhoun", FIPS = "1015")
output[[8]] <- output[[8]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[9]] <- output[[9]] %>%
  mutate(County = "Chambers", FIPS = "1017")
output[[9]] <- output[[9]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[10]] <- output[[10]] %>%
  mutate(County = "Cherokee", FIPS = "1019")
output[[10]] <- output[[10]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[11]] <- output[[11]] %>%
  mutate(County = "Chilton", FIPS = "1021")
output[[11]] <- output[[11]] %>% relocate(County, FIPS, .before = DEMWillBoyd)                                

output[[12]] <- output[[12]] %>%
  mutate(County = "Choctaw", FIPS = "1023")
output[[12]] <- output[[12]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[13]] <- output[[13]] %>%
  mutate(County = "Clarke", FIPS = "1025")
output[[13]] <- output[[13]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[14]] <- output[[14]] %>%
  mutate(County = "Clay", FIPS = "1027")
output[[14]] <- output[[14]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[15]] <- output[[15]] %>%
  mutate(County = "Cleburne", FIPS = "1029")
output[[15]] <- output[[15]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[16]] <- output[[16]] %>%
  mutate(County = "Coffee", FIPS = "1031")
output[[16]] <- output[[16]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[17]] <- output[[17]] %>%
  mutate(County = "Colbert", FIPS = "1033")
output[[17]] <- output[[17]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[18]] <- output[[18]] %>%
  mutate(County = "Conecuh", FIPS = "1035")
output[[18]] <- output[[18]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[19]] <- output[[19]] %>%
  mutate(County = "Coosa", FIPS = "1037")
output[[19]] <- output[[19]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[20]] <- output[[20]] %>%
  mutate(County = "Covington", FIPS = "1039")
output[[20]] <- output[[20]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[21]] <- output[[21]] %>%
  mutate(County = "Crenshaw", FIPS = "1041")
output[[21]] <- output[[21]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[22]] <- output[[22]] %>%
  mutate(County = "Cullman", FIPS = "1043")
output[[22]] <- output[[22]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[23]] <- output[[23]] %>%
  mutate(County = "Dale", FIPS = "1045")
output[[23]] <- output[[23]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[24]] <- output[[24]] %>%
  mutate(County = "Dallas", FIPS = "1047")
output[[24]] <- output[[24]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[25]] <- output[[25]] %>%
  mutate(County = "Dekalb", FIPS = "1049")
output[[25]] <- output[[25]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[26]] <- output[[26]] %>%
  mutate(County = "Elmore", FIPS = "1051")
output[[26]] <- output[[26]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[27]] <- output[[27]] %>%
  mutate(County = "Escambia", FIPS = "1053")
output[[27]] <- output[[27]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[28]] <- output[[28]] %>%
  mutate(County = "Etowah", FIPS = "1055")
output[[28]] <- output[[28]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[29]] <- output[[29]] %>%
  mutate(County = "Fayette", FIPS = "1057")
output[[29]] <- output[[29]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[30]] <- output[[30]] %>%
  mutate(County = "Franklin", FIPS = "1059")
output[[30]] <- output[[30]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[31]] <- output[[31]] %>%
  mutate(County = "Geneva", FIPS = "1061")
output[[31]] <- output[[31]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[32]] <- output[[32]] %>%
  mutate(County = "Greene", FIPS = "1063")
output[[32]] <- output[[32]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[33]] <- output[[33]] %>%
  mutate(County = "Hale", FIPS = "1065")
output[[33]] <- output[[33]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[34]] <- output[[34]] %>%
  mutate(County = "Henry", FIPS = "1067")
output[[34]] <- output[[34]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[35]] <- output[[35]] %>%
  mutate(County = "Houston", FIPS = "1069")
output[[35]] <- output[[35]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[36]] <- output[[36]] %>%
  mutate(County = "Jackson", FIPS = "1071")
output[[36]] <- output[[36]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[37]] <- output[[37]] %>%
  mutate(County = "Jefferson", FIPS = "1073")
output[[37]] <- output[[37]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[38]] <- output[[38]] %>%
  mutate(County = "Lamar", FIPS = "1075")
output[[38]] <- output[[38]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[39]] <- output[[39]] %>%
  mutate(County = "Lauderdale", FIPS = "1077")
output[[39]] <- output[[39]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[40]] <- output[[40]] %>%
  mutate(County = "Lawrence", FIPS = "1079")
output[[40]] <- output[[40]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[41]] <- output[[41]] %>%
  mutate(County = "Lee", FIPS = "1081")
output[[41]] <- output[[41]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[42]] <- output[[42]] %>%
  mutate(County = "Limestone", FIPS = "1083")
output[[42]] <- output[[42]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[43]] <- output[[43]] %>%
  mutate(County = "Lowndes", FIPS = "1085")
output[[43]] <- output[[43]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[44]] <- output[[44]] %>%
  mutate(County = "Macon", FIPS = "1087")
output[[44]] <- output[[44]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[45]] <- output[[45]] %>%
  mutate(County = "Madison", FIPS = "1089")
output[[45]] <- output[[45]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[46]] <- output[[46]] %>%
  mutate(County = "Marengo", FIPS = "1091")
output[[46]] <- output[[46]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[47]] <- output[[47]] %>%
  mutate(County = "Marion", FIPS = "1093")
output[[47]] <- output[[47]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[48]] <- output[[48]] %>%
  mutate(County = "Marshall", FIPS = "1095")
output[[48]] <- output[[48]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[49]] <- output[[49]] %>%
  mutate(County = "Mobile", FIPS = "1097")
output[[49]] <- output[[49]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[50]] <- output[[50]] %>%
  mutate(County = "Monroe", FIPS = "1099")
output[[50]] <- output[[50]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[51]] <- output[[51]] %>%
  mutate(County = "Montgomery", FIPS = "1101")
output[[51]] <- output[[51]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[52]] <- output[[52]] %>%
  mutate(County = "Morgan", FIPS = "1103")
output[[52]] <- output[[52]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[53]] <- output[[53]] %>%
  mutate(County = "Perry", FIPS = "1105")
output[[53]] <- output[[53]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[54]] <- output[[54]] %>%
  mutate(County = "Pickens", FIPS = "1107")
output[[54]] <- output[[54]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[55]] <- output[[55]] %>%
  mutate(County = "Pike", FIPS = "1109")
output[[55]] <- output[[55]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[56]] <- output[[56]] %>%
  mutate(County = "Randolph", FIPS = "1111")
output[[56]] <- output[[56]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[57]] <- output[[57]] %>%
  mutate(County = "Russell", FIPS = "1113")
output[[57]] <- output[[57]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[58]] <- output[[58]] %>%
  mutate(County = "Shelby", FIPS = "1115")
output[[58]] <- output[[58]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[59]] <- output[[59]] %>%
  mutate(County = "StClair", FIPS = "1117")
output[[59]] <- output[[59]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[60]] <- output[[60]] %>%
  mutate(County = "Sumter", FIPS = "1119")
output[[60]] <- output[[60]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[61]] <- output[[61]] %>%
  mutate(County = "Talladega", FIPS = "1121")
output[[61]] <- output[[61]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[62]] <- output[[62]] %>%
  mutate(County = "Tallapoosa", FIPS = "1123")
output[[62]] <- output[[62]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[63]] <- output[[63]] %>%
  mutate(County = "Tuscaloosa", FIPS = "1125")
output[[63]] <- output[[63]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[64]] <- output[[64]] %>%
  mutate(County = "Walker", FIPS = "1127")
output[[64]] <- output[[64]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[65]] <- output[[65]] %>%
  mutate(County = "Washington", FIPS = "1129")
output[[65]] <- output[[65]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[66]] <- output[[66]] %>%
  mutate(County = "Wilcox", FIPS = "1131")
output[[66]] <- output[[66]] %>% relocate(County, FIPS, .before = DEMWillBoyd)

output[[67]] <- output[[67]] %>%
  mutate(County = "Winston", FIPS = "1133")
output[[67]] <- output[[67]] %>% relocate(County, FIPS, .before = DEMWillBoyd)


output <- lapply(output, function(x) x[-1,] ) #delete contest title since we know what they are

output <- Map(cbind, output, text_name = lapply(output, rownames))

test2 <- bind_rows(output)
output_merge <- test2
output_merge$Precinct <- rownames(output_merge)
output_merge$text_name <- NULL #remove the original name for rownames command
rownames(output_merge) <- 1:nrow(output_merge) #create row numbers instead of having the rownames as locations


## Now to begin cleanup, first by combining columns
output_merge <- unite(output_merge, FIPS_County, c(FIPS, County), sep = " ")
output_merge <- unite(output_merge, Precinct, c(FIPS_County, Precinct), sep = ": ")

## Move the precinct column to the first position
output_merge <- output_merge %>%
  select("Precinct", everything())

##Remove the FIPS numbers and capitalize everything since the formatting with them looks awkward
output_merge$Precinct <- gsub('[0-9. ]', '', output_merge$Precinct)
output_merge$Precinct <- toupper(output_merge$Precinct)
output_merge$Precinct <- gsub(':', '---', output_merge$Precinct)
output_merge$id <- output_merge$Precinct
output_merge$Precinct <- NULL
output_merge <- output_merge %>%
  select("id", everything())

##Now for the fun part - cleaning up the precinct names
##First, start standardizing shared elements across each precinct

output_merge$id <- gsub('CoOMM_CTR_', 'COMMUNITY CENTER',
                   gsub('COMM_CTR', ' COMMUNITY CENTER', 
                   gsub('COMMCTR', ' COMMUNITY CENTER', output_merge$id)))
output_merge$id <- gsub('COMMUNITY CENTER_', 'COMMUNITY CENTER', output_merge$id)
output_merge$id <- gsub('VFD', ' VFD', output_merge$id)
output_merge$id <- gsub('VFD#', ' VFD', output_merge$id)
output_merge$id <- gsub('BAPTIST', ' BAPTIST', output_merge$id)
output_merge$id <- gsub('CHRISTIANCH', ' CHRISTIAN CHURCH', output_merge$id)
output_merge$id <- gsub('COMM_COLLEGE', ' COMM COLLEGE', output_merge$id)
output_merge$id <- gsub('UMC', ' UNITED METHODIST', output_merge$id)
output_merge$id <- gsub('METHODIST', ' METHODIST', output_merge$id)
output_merge$id <- gsub('CIVICCTR', ' CIVIC CENTER', 
                   gsub('CIVICCTR_', ' CIVIC CENTER', output_merge$id))
output_merge$id <- gsub('CITYHALL', ' CITY HALL', output_merge$id)
output_merge$id <- gsub('MIDDLESCH', ' MIDDLE SCHOOL', output_merge$id)
output_merge$id <- gsub('SR_CTR_', ' SENIOR CENTER', output_merge$id)
output_merge$id <- gsub('_', '', output_merge$id)
output_merge$id <- gsub('UNITED  METHODIST', 'UNITED METHODIST', output_merge$id)
output_merge$id <- gsub('  VFD', ' VFD', output_merge$id)
output_merge$id <- gsub('COMMD-', 'COMMUNITY CENTER', output_merge$id)
output_merge$id <- gsub('PINERIDGECOMMUNITY CENTER', 'PINE RIDGE COMMUNITY CENTER', output_merge$id)
output_merge$id <- gsub('PREC-FAITHCHAPELCHRIS', 'FAITH CHAPEL CHRI', output_merge$id)
output_merge$id <- gsub("BROWNS-WHITE'SCHAPELCH", "BROWNS-WHITE'S CHAPEL CENTER", output_merge$id)                   
output_merge$id <- gsub("FAIRHOPECIRCLECH", "FAIRHOPE CIRCLE CHURCH", output_merge$id)
output_merge$id <- gsub("THREECIRCLECHMIDTOWN", "THREE CIRCLE CHURCH MIDTOWN", output_merge$id)
output_merge$id <- gsub("CHAPEL", " CHAPEL", output_merge$id)
output_merge$id <- gsub("BROMLEYCROOSROADS", "BROMLEYCROOSROADS", output_merge$id)
output_merge$id <- gsub("CROSSROADS", " CROSSROADS", output_merge$id)
output_merge$id <- gsub("CROSSROADSCHOFCHRIST", "CROSSROADS CHURCH OF CHRIST", output_merge$id)
output_merge$id <- gsub("BONSECOURMORG CHAPEL", "BON SECOUR MORGANS CHAPEL", output_merge$id)

##I realize that I did this in the previous section, but now for some hardcoding
##AUTAUGA
output_merge$id <- gsub("UPPERKINGSTON", "UPPER KINGSTON", output_merge$id)
output_merge$id <- gsub("CENTRALALELECTRIC", "CENTRAL AL ELECTRIC", output_merge$id)
output_merge$id <- gsub("WHITECITY", "WHITE CITY", output_merge$id)
output_merge$id <- gsub("OLDKINGSTON", "OLD KINGSTON", output_merge$id)
output_merge$id <- gsub("CENTRALAL", "CENTRAL AL", output_merge$id)

##BALDWIN
output_merge$id <- gsub("BROMLEYCROOSROADS", "BROMLEY CROSSROADS", output_merge$id)
output_merge$id <- gsub("BAYMINETTE", "BAY MINETTE", output_merge$id)
output_merge$id <- gsub("CLRSPRINGS", "CLEAR SPRINGS", output_merge$id)
output_merge$id <- gsub(" CROSSROADSDURANT", "DURANT CROSSROADS CHURCH", output_merge$id)
output_merge$id <- gsub("DOUGLASVILLEBOYKIN", "DOUGLASVILLE BOYKIN", output_merge$id)
output_merge$id <- gsub("ELSANORBETHALBAPT", "ELSANOR BETHEL BAPTIST CHURCH", output_merge$id)
output_merge$id <- gsub("FAIRHOPEMARSHILLCH", "MARS HILL CHURCH", output_merge$id)
output_merge$id <- gsub("FIRSTFAIRHOPEBAPT", "FAIRHOPE AVE BAPTIST CHURCH", output_merge$id)
output_merge$id <- gsub("FOLEYUNITED METHODIST", "FOLEY UNITED METHODIST", output_merge$id)
output_merge$id <- gsub("FTMORGAN VFD", "FT_ MORGAN VFD", output_merge$id)
output_merge$id <- gsub("GRAHAMCREEKINTERPRETIVE", "GRAHAM CREEK INTERPRETIVE", output_merge$id)
output_merge$id <- gsub("GULFSHORES CIVIC CENTER", "GULF SHORES CIVIC CENTER", output_merge$id)
output_merge$id <- gsub("GULFSHORESCULTURAL", "GULF SHORES CULTURAL", output_merge$id)
output_merge$id <- gsub("HARRIETTCOCKRELLCTR", "HARRIETT COCKRELL CENTER", output_merge$id)
output_merge$id <- gsub("HOMESTEADVILLAGE", "HOMESTEAD VILLAGE", output_merge$id)
output_merge$id <- gsub("LILLIANCOMMCLUB", "LILLIAN COMMUNITY CLUB", output_merge$id)
output_merge$id <- gsub("LITTLERIVER VFD", "LITTLE RIVER VFD", output_merge$id)
output_merge$id <- gsub("MAGSPGSWESLEYANCH", "MAGNOLIA SPGS WESLEYAN CHURCH", output_merge$id)
output_merge$id <- gsub("MARLOW/FISHRIVER VFD", "MARLOW/FISH RIVER VFD", output_merge$id)
output_merge$id <- gsub("NEWLIFEASSEMBLYOFGOD", "NEW LIFE ASSEMBLY OF GOD", output_merge$id)
output_merge$id <- gsub("ORANGEBEACH", "ORANGE BEACH", output_merge$id)
output_merge$id <- gsub("PERDIDOBEACH", "PERDIDO BEACH", output_merge$id)
output_merge$id <- gsub("PTCLEARSTFRANCIS", "PT CLEAR ST FRANCIS CHURCH", output_merge$id)
output_merge$id <- gsub("ROBERTSDALEPZKHALL", "ROBERTSDALE PZK HALL", output_merge$id)
output_merge$id <- gsub("SEMINOLEFIREHOUSE", "SEMINOLE FIREHOUSE", output_merge$id)
output_merge$id <- gsub("SPANISHFORTCHCHRIST", "SPANISH FT CHURCH OF CHRIST", output_merge$id)
output_merge$id <- gsub("SPANISHFORT", "SPANISH FORT", output_merge$id)
output_merge$id <- gsub("STMARGARETSCOTLAND", "ST MARGARET SCOTLAND", output_merge$id)
output_merge$id <- gsub("STPAUL'SEPISCOPALCH", "ST PAUL'S EPISCOPAL CHURCH", output_merge$id)
output_merge$id <- gsub("STOCKTONCIVICBLDG", "STOCKTON CIVIC BUILDING", output_merge$id)
output_merge$id <- gsub("WHITEHOUSEFORK", "WHITE HOUSE FORK", output_merge$id)

##BARBOUR
output_merge$id <- gsub("BAKERHILLTOWNHALL", "BAKER HILL TOWN HALL", output_merge$id)
output_merge$id <- gsub("BEVILLCTR", "BEVILL CENTER", output_merge$id)
output_merge$id <- gsub("BLUESPRINGS", "BLUE SPRINGS",
                   gsub("CLAYTONCOURTHOUSE", 'CLAYTON COURTHOUSE',
                   gsub("EUFAULAWATERBOARD", "EUFALA WATERBOARD",
                   gsub("MCCOOSCHOOL", "MCCOO SCHOOL",
                   gsub("MTANDREWS", "MT ANDREWS",
                   gsub("SANFORDAVEGYM", "SANFORD AVE GYM", output_merge$id))))))

##BIBB
output_merge$id <- gsub("ALTERNATIVESCHOOL", "ALTERNATIVE SCHOOL",
                   gsub("CENTREVILLEROCKBLDG", "CENTREVILLE ROCK BUILDING",
                   gsub("EOLINEFIREDEPT", "EOLINE FIRE DEPT",
                   gsub("GREENPONDFIREDEPT", "GREENPOND FIRE DEPT",
                   gsub("NATIONALGUARD", "NATIONAL GUARD ARMORY",
                   gsub("SIXMILE", "SIX MILE", output_merge$id))))))

##BLOUNT
output_merge$id <- gsub("ALLGOODTOWNHALL", "ALLGOOD TOWN HALL",
                   gsub("BLOUNTSPRINGS", "BLOUNT SPRINGS",
                   gsub("HOLLYSPRINGS", "HOLLY SPRINGS",
                   gsub("LOCUSTFORK", "LOCUST FORK",
                   gsub("MTHIGH", "MT_HIGH",
                   gsub("MURPHREEVALLEY", "MURPHREE VALLEY",
                   gsub("PINEMOUNTAIN", "PINE MOUNTAIN", output_merge$id)))))))

##BULLOCK
output_merge$id <- gsub("ALMERIA", "ALMERIA ",
                   gsub("CORNERSTONERECCTR", "CORNERSTONE REC_ CTR",
                   gsub("OLDMERRITTSCHOOL", "OLD MERRITT SCHOOL",
                   gsub("PEROTEVOTINGBLDG", "PEROTE VOTING BUILDING",
                   gsub("RICHARDBSTONECOMPLEX", "RICHARD B STONE COMPLEX",
                   gsub("UNIONSPRINGSRECCTR", "UNION SPRINGS REC_ CTR", output_merge$id))))))

##BUTLER
output_merge$id <- gsub("AMERICANLEGIONBLDG", "AMERICAN LEGION BUILDING",
                   gsub("BOLLINGVTGHOUSE", "BOLLING-VOTING HOUSE",
                   gsub("CENTERHILLVTGHOUSE", "CENTER HILL-VOTING HOUSE",
                   gsub("CHAPMANVTGHOUSE", "CHAPMAN VOTING HOUSE",
                   gsub("FORESTHOMEVTGHOUSE", "FOREST HOME-VOTING HOUSE",
                   gsub("FRIENDSHIPOLDSCHHOUSE", "FRIENDSHIP OLD SCHOOL",
                   gsub("GEORGIANANUTRITIONCTR", "GEORGIANA NUTRITION CENTER",
                   gsub("HARRISON'SVTGHOUSE", "HARISON'S-VOTING HOUSE",
                   gsub("HEALTHDEPT", "HEALTH DEPT",
                   gsub("INDUSTRYOLDSCHHOUSE", "INDUSTRY OLD SCHOOL HOUSE",
                   gsub("MANNINGHAMVTGHOUSE", "MANNINGHAM-VOTING HOUSE",
                   gsub("MIDWAY&DAMASCUS", "MIDWAY & DAMASCUS",
                   gsub("NEWMCLAINSVTGHOUSE", "NEW MCLAINS-VOTING HOUSE",
                   gsub("PROVIDENCEVTGHOUSE", "PROVIDENCE-VOTING HOUSE",
                   gsub("ROCKYCREEKVTGHOUSE", "ROCKY CREEK-VOTING HOUSE",
                   gsub("STEINER'SSTORE", "STEINER'S STORE", output_merge$id))))))))))))))))

##CALHOUN
output_merge$id <- gsub("STUN METHODIST-WEAVER", "1ST UNITED METHODIST - WEAVER",
                   gsub("APHOLLINGSWORTHCOMM", "A_P_ HOLLINGSWORTH COMMUNITY CENTER",
                   gsub("ALEXANDRIACIVITANCLUB", "ALEXANDRIA CIVITAN CLUB",
                   gsub("ANGELFIRESTATION", "ANGEL FIRE STATION",
                   gsub("ANNISTONCARVERCTR", "ANNISTON CARVER CENTER",
                   gsub("ANNISTONGOLDENSPRINGS", "ANNISTON GOLDEN SPRINGS",
                   gsub("ANNISTONMEETINGCTR", "ANNISTON MEETING CENTER",
                   gsub("ANNISTONTHANKFULBAPT", "ANNISTON THANKFUL BAPTIST CHURCH",
                   gsub("ASBERRYBAPT", "ASBERRY BAPTIST CHURCH",
                   gsub("BETTAVIEWCHOFCHRIST", "BETTA VIEW CHURCH OF CHRIST",
                   gsub("BOILINGSPRINGSBAPT", "BOILING SPRINGS BAPTIST CHURCH",
                   gsub("CALHOUNCOHWYDEPT", "CALHOUN CO_ HWY DEPT",
                   gsub("FRIENDSHIPBAPT", "FRIENDSHIP BAPTIST CHURCH",
                   gsub("GLENWOODMEADOWSMTGRM", "GLENWOOD MEADOWS MTG_ RM_",
                   gsub("GOD'SCOVENANTMINISTRY", "GOD'S COVENANT MINISTRY",
                   gsub("HARVESTCHOFGOD", "HARVEST CHURCH OF GOD",
                   gsub("HOBSONCITY- CITY HALL", "HOBSON CITY - CITY HALL",
                   gsub("HOPECOMMCH", "HOPE COMM CHURCH",
                   gsub("JACKSONVILLESTBAPT", "JACKSONVILLE 1ST BAPTIST CHURCH",
                   gsub("JACKSONVILLESAFETYCOMP", "JACKSONVILLE SAFETY COMP",
                   gsub("JACKSONVILLEWESTSIDECH", "JACKSONVILLE WESTSIDE CHURCH",
                   gsub("LEATHERWOODRDCIV", "LEATHERWOOD RD CIV_",
                   gsub("MENTALHEALTHCTR", "MENTAL HEALTH CENTER",
                   gsub("MTVIEWBAPT", "MT_VIEW BAPTIST CHURCH",
                   gsub("OAKGROVEBAPT", "OAK GROVE BAPTIST CHURCH",
                   gsub("OHATCHEESTBAPT", "OHATCHEE 1ST BAPTIST CHURCH",
                   gsub("OXFORDCHEAHACLUBHOUSE", "OXFORD CHEAHA CLUBHOUSE",
                   gsub("OXFORDLAKEPK CIVIC CENTER", "OXFORD LAKE PARK CIV_ CENTER",
                   gsub("OXFORDPUBLICLIBRARY", "OXFORD PUBLIC LIBRARY",
                   gsub("PIEDMONTFIRESTATION", "PIEDMONT FIRE STATION",
                   gsub("PIEDMONTRECBLDG", "PIEDMONT REC_ BUILDLING",
                   gsub("SHIGHLAND", "S_HIGHLAND",
                   gsub("SAKSHIGHSCHOOL", "SAKS HIGH SCHOOL",
                   gsub("TRINITYMISSIONARYBAPT", "TRINITY MISSIONARY BAPTIST CHURCH",
                   gsub("WPARKHEIGHTSBAPT", "W PARK HEIGHTS BAPTIST CHURCH",
                   gsub("WHITEPLAINSSTBAPT", "WHITE PLAINS 1ST BAPTIST CHURCH",
                   gsub("WHITEPLAINS VFD", "WHITE PLAINS VFD", output_merge$id)))))))))))))))))))))))))))))))))))))

##CHAMBERS
output_merge$id <- gsub("MTOLIVE", "MT_OLIVE COMMUNITY CLUBHOUSE",
                    gsub("RIDGEGROVE", "RIDGE GROVE",
                    gsub("SPARKLINGSPRINGS", "SPARKLING SPRINGS",
                    gsub("STANDINGROCK", "STANDING ROCK",
                    gsub("UNIONHILL", "UNION HILL",
                    gsub("VALLEYSPORTSPLEX", "VALLEY SPORTSPLEX",
                    gsub("WESTCHAMBERS", "WEST CHAMBERS", output_merge$id)))))))

##CHEROKEE
output_merge$id <- gsub("BEREACHURCHOFCHRIST", "BEREA CHURCH OF CHRIST",
                   gsub("BROOMTOWNFIREDEPT", "BROOMTOWN FIRE DEPT",
                   gsub("CEDARBLUFFSTBAPT", "CEDAR BLUFF 1ST BAPTIST CHURCH",
                   gsub("DANIELS CHAPELFELLOWSHIP", "DANIELS CHAPEL",
                   gsub("LEESBURGFEMABLDG", "LEESBURG FEMA BUILDING",
                   gsub("MACEDONIABAPTFELLOWSHIP", "MACEDONIA BAPTIST CHURCH",
                   gsub("MTCALVARYBAPTCHURCH", "MOUNT CALVARY BAPTIST CHURCH",
                   gsub("MTWEISNERFIREHALL", "MOUNT WEISNER FIRE HALL",
                   gsub("MTWEISNER VFD", "MOUNT WEISNER VFD",
                   gsub("NEWHOPE", "NEW HOPE",
                   gsub("ROCKRUNCHURCH", "ROCK RUN CHURCH",
                   gsub("SANDROCKTOWNHALL", "SAND ROCK TOWN HALL",
                   gsub("SPRINGGARDENFIRESTATION", "SPRING GARDEN FIRE STATION",
                   gsub("THEROC", "THE ROC",
                   gsub("THEVALLEYCHURCH", "THE VALLEY CHURCH",
                   gsub("CHAPELFIREDEPT", "CHAPEL FIRE DEPT",
                   gsub("UNITYMISSIONARYBAPT", "UNITY MISSIONARY BAPTIST CHURCH", output_merge$id)))))))))))))))))


##test merge
write.csv(output_merge, file = "al22.csv")

output_merge2 <- subset(output_merge, select = - c(2, 3, 5, 7, 9, 11))

output_merge3 <- output_merge2 %>%
  rename(
    ussen22...LillieBoddie = REPLillieBoddie,
    ussen22...KatieBritt = REPKatieBritt,
    ussen22...MoBrooks = REPMoBrooks,
    ussen22...KarlaM.Dupriest = REPKarlaM.Dupriest,
    ussen22...MikeDurant = REPMikeDurant,
    ussen22...JakeSchafer = REPJakeSchafer,
  )

test_merge <- merge(alreturns_final, output_merge3, by = "id", all = T)
write.csv(test_merge, file = "almerge.csv")

test_merge$ID <- row.names(test_merge)



