#################
# Health indicator and legislation analysis
# Citation: Goldstein ND, Purtle J. Epidemiology as a tool for legislative accountability and health policy advocacy. Manuscript in preparation.
# 9/14/17 -- Neal Goldstein
#################


### FUNCTIONS ###

library(psych) #describe, describeBy
library(gmodels) #CrossTable
library(stringr) #string functions
library(RecordLinkage) #Levenshtein string comparison
library(nlme) #mixed effects modeling
library(lme4) #generalized mixed effects
library(maptools) #read shapefiles
library(RColorBrewer) #color palette

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


### READ DATA ###

health_indicators = read.csv("Congressional_Districts_Listed_in_alphabetical_order_by_state.csv", stringsAsFactors=F)
census_b050 = read.csv("ACS_15_1YR_B05012/ACS_15_1YR_B05012_with_ann.csv", stringsAsFactors=F, header=T, as.is=T, na.strings=c("","-"))
census_dp03 = read.csv("ACS_15_1YR_DP03/ACS_15_1YR_DP03_with_ann.csv", stringsAsFactors=F, header=T, as.is=T, na.strings=c("","-"))
census_dp05 = read.csv("ACS_15_1YR_DP05/ACS_15_1YR_DP05_with_ann.csv", stringsAsFactors=F, header=T, as.is=T, na.strings=c("","-"))
#census_urban = read.csv("2010 Census Urban Areas/ua_st_list_ua.csv", stringsAsFactors=F)
#census_urban_zcta = read.csv("2010 Census Urban Areas/ua_zcta_rel_10.csv", stringsAsFactors=F)
#census_urban_zcta_cd = read.csv("2010 Census Urban Areas/natl_zccd_delim.csv", stringsAsFactors=F)
bills = read.csv("mental_health_bills.csv", stringsAsFactors=F)
representatives = read.csv("114th Congress Members 11.9 KN.csv", stringsAsFactors=F, header=F)

#read 114th Congress congressional district shapefile
districts = readShapePoly("tl_2015_us_cd114/tl_2015_us_cd114")

#read FIPS codes: http://www.columbia.edu/~sue/state-fips.html
state_fips = read.csv("state_fips.csv", stringsAsFactors=F, header=T)


### CLEAN and LINK ###

census_b050 = census_b050[-1,] #drop labels
census_b050 = census_b050[-437,] #drop PR
census_dp03 = census_dp03[-1,] #drop labels
census_dp03 = census_dp03[-437,] #drop PR
census_dp05 = census_dp05[-1,] #drop labels
census_dp05 = census_dp05[-437,] #drop PR

#create linking variable
census_b050$district = health_indicators$district
census_dp03$district = health_indicators$district
census_dp05$district = health_indicators$district

#merge
health_indicators = merge(health_indicators, census_dp05[, c("district", "HC01_VC03")], by="district")
health_indicators = merge(health_indicators, census_dp05[, c("district", "HC03_VC04")], by="district")
health_indicators = merge(health_indicators, census_dp05[, c("district", "HC01_VC23")], by="district")
health_indicators = merge(health_indicators, census_dp05[, c("district", "HC03_VC49")], by="district")
health_indicators = merge(health_indicators, census_dp05[, c("district", "HC03_VC50")], by="district")
health_indicators = merge(health_indicators, census_dp05[, c("district", "HC03_VC88")], by="district")
health_indicators = merge(health_indicators, census_dp05[, c("district", "HC03_VC108")], by="district")
health_indicators = merge(health_indicators, census_dp03[, c("district", "HC03_VC07")], by="district")
health_indicators = merge(health_indicators, census_dp03[, c("district", "HC01_VC85")], by="district")
health_indicators = merge(health_indicators, census_b050[, c("district", "HD01_VD03")], by="district")

#rename variables
names(health_indicators)[names(health_indicators)=="HC01_VC03"] = "population"
names(health_indicators)[names(health_indicators)=="HC03_VC04"] = "percent_male"
names(health_indicators)[names(health_indicators)=="HC01_VC23"] = "median_age"
names(health_indicators)[names(health_indicators)=="HC03_VC49"] = "percent_white"
names(health_indicators)[names(health_indicators)=="HC03_VC50"] = "percent_black"
names(health_indicators)[names(health_indicators)=="HC03_VC88"] = "percent_hispanic"
names(health_indicators)[names(health_indicators)=="HC03_VC108"] = "percent_citizen_vote"
names(health_indicators)[names(health_indicators)=="HC03_VC07"] = "percent_unemployed"
names(health_indicators)[names(health_indicators)=="HC01_VC85"] = "median_income"
names(health_indicators)[names(health_indicators)=="HD01_VD03"] = "percent_foreign_born"

#recast
health_indicators$population = as.numeric(health_indicators$population)
health_indicators$percent_male = as.numeric(health_indicators$percent_male)
health_indicators$median_age = as.numeric(health_indicators$median_age)
health_indicators$percent_white = as.numeric(health_indicators$percent_white)
health_indicators$percent_black = as.numeric(health_indicators$percent_black)
health_indicators$percent_hispanic = as.numeric(health_indicators$percent_hispanic)
health_indicators$percent_citizen_vote = as.numeric(health_indicators$percent_citizen_vote)
health_indicators$percent_unemployed = as.numeric(health_indicators$percent_unemployed)
health_indicators$median_income = as.numeric(health_indicators$median_income)
health_indicators$percent_foreign_born = as.numeric(health_indicators$percent_foreign_born)

#calculate percents
health_indicators$percent_citizen_vote = round(health_indicators$percent_citizen_vote / health_indicators$population,1)
health_indicators$percent_foreign_born = round(health_indicators$percent_foreign_born / health_indicators$population,1)

# #map urban areas to CD
# census_urban_cd = merge(merge(census_urban,census_urban_zcta[, c("UA","ZCTA5")], by.x="UACE", by.y="UA", all.x=T, all.y=F), census_urban_zcta_cd[, c("ZCTA", "DISTRICT")], by.x="ZCTA5", by.y="ZCTA", all.x=T, all.y=F)
# census_urban_cd$CD = paste(census_urban_cd$STATE_ABREV,census_urban_cd$DISTRICT,sep="-")
# 
# health_indicators$urban = NA
# for (i in 1:nrow(health_indicators))
# {
#   if(sum(health_indicators$district[i]==census_urban_cd$CD)>0) {
#     health_indicators$urban[i] = 1
#   } else {
#     health_indicators$urban[i] = 0
#   }
# }
# rm(i)

#transpose representatives
representatives$ID = NA
representatives_wide = data.frame("ID"=NA, "District"=NA, "Representative"=NA, "Party"=NA, "Gender"=NA, "Experience"=NA, stringsAsFactors=F)
r=0
for (i in 1:nrow(representatives))
{
  #new representative
  if (length(grep("MEMBER",representatives$V1[i]))>0) {
    r = r + 1
  }
  representatives$ID[i] = r
  
  #flush to wide dataset
  if (i %% 5 == 0) {
    state = substr(representatives[which(representatives$ID==r),1][4], start=(str_locate(representatives[which(representatives$ID==r),1][4], "State")+7), stop=(str_locate(representatives[which(representatives$ID==r),1][4], "District")-1))
    district = substr(representatives[which(representatives$ID==r),1][4], start=(str_locate(representatives[which(representatives$ID==r),1][4], "District")+10), stop=(str_locate(representatives[which(representatives$ID==r),1][4], "Party")-1))
    
    #create the legislative district as state abbreviation-district
    if (is.na(state) || state=="") {
      state = substr(representatives[which(representatives$ID==r),1][4], start=(str_locate(representatives[which(representatives$ID==r),1][4], "State")+7), stop=(str_locate(representatives[which(representatives$ID==r),1][4], "Party")-1))
      district = "AL"
    }
    
    if (state=="District of Columbia") {
      district = "DC-AL"
    } else {
      district = paste(state.abb[match(state,state.name)],district,sep="-")
    }
    
    representatives_wide = rbind(representatives_wide, data.frame("ID"=r,"District"=district, "Representative"=paste(substr(representatives[which(representatives$ID==r),1][3], start=(str_locate(representatives[which(representatives$ID==r),1][3],",")+2), stop=str_length(representatives[which(representatives$ID==r),1][3])), substr(representatives[which(representatives$ID==r),1][3], start=1, stop=(str_locate(representatives[which(representatives$ID==r),1][3],",")-1))), "Party"=substr(representatives[which(representatives$ID==r),1][4], start=(str_locate(representatives[which(representatives$ID==r),1][4],"Party")+7), stop=(str_locate(representatives[which(representatives$ID==r),1][4],"Served")-1)), "Gender"=representatives[which(representatives$ID==r),2][1], "Experience"=(as.numeric(substr(representatives[which(representatives$ID==r),1][5], start=(str_locate(representatives[which(representatives$ID==r),1][5],"-")+1), stop=(str_locate(representatives[which(representatives$ID==r),1][5],"-")+4))) - as.numeric(substr(representatives[which(representatives$ID==r),1][5], start=(str_locate(representatives[which(representatives$ID==r),1][5],"-")-4), stop=(str_locate(representatives[which(representatives$ID==r),1][5],"-")-1)))), stringsAsFactors=F))
  }
}
representatives_wide = representatives_wide[-1, ]
rm(i,r,state,district)

#bills
health_indicators$bill_sponsored = 0
health_indicators$bill_cosponsored = 0
for (i in 1:nrow(bills))
{
  #sponsored
  representative = str_sub(bills$Sponsor[i], start=str_locate(bills$Sponsor[i], "\\[")[1]+1, end=str_locate(bills$Sponsor[i], "\\]")[1]-1)
  party = str_sub(representative, start=1, end=str_locate(representative,"-")[1]-1)
  district = str_sub(representative, start=str_locate(representative,"-")[1]+1, end=str_length(representative))
  health_indicators$bill_sponsored[which(health_indicators$district==district)] = health_indicators$bill_sponsored[which(health_indicators$district==district)] + 1
    
  #cosponsored
  for (j in 1:bills$Cosponsors[i])
  {
    representative = str_sub(bills[i,(3+j)], start=str_locate(bills[i,(3+j)], "\\[")[1]+1, end=str_locate(bills[i,(3+j)], "\\]")[1]-1)
    party = str_sub(representative, start=1, end=str_locate(representative,"-")[1]-1)
    district = str_sub(representative, start=str_locate(representative,"-")[1]+1, end=str_length(representative))
    health_indicators$bill_cosponsored[which(health_indicators$district==district)] = health_indicators$bill_cosponsored[which(health_indicators$district==district)] + 1
  }
}
rm(i, j, representative, party, district)

#total sponsorship
health_indicators$bills = health_indicators$bill_sponsored + health_indicators$bill_cosponsored
health_indicators$bills_cat = ifelse(health_indicators$bills>0, 1, 0)

#join representatives to health indicators
health_indicators$representative_party = NA
health_indicators$representative_gender = NA
health_indicators$representative_experience = NA
for (i in 1:nrow(health_indicators))
{
  #find the corresponding district in the representative dataset
  if (length(which(representatives_wide$District==health_indicators$district[i]))==1) {
    
    #exact match
    health_indicators$representative_party[i] = representatives_wide$Party[which(representatives_wide$District==health_indicators$district[i])]
    health_indicators$representative_gender[i] = representatives_wide$Gender[which(representatives_wide$District==health_indicators$district[i])]
    health_indicators$representative_experience[i] = representatives_wide$Experience[which(representatives_wide$District==health_indicators$district[i])]

  } else if (length(which(representatives_wide$District==health_indicators$district[i]))>1) {
    
    #multiple matches, resolve by representative name, choosing the one with the highest Levenschtein value (1=perfect match, 0=no match)
    if (levenshteinSim(health_indicators$representative[i], representatives_wide$Representative[which(representatives_wide$District==health_indicators$district[i])][1]) < levenshteinSim(health_indicators$representative[i], representatives_wide$Representative[which(representatives_wide$District==health_indicators$district[i])][2])) {
      #second representative is closer
      health_indicators$representative_party[i] = representatives_wide$Party[which(representatives_wide$District==health_indicators$district[i])][2]
      health_indicators$representative_gender[i] = representatives_wide$Gender[which(representatives_wide$District==health_indicators$district[i])][2]
      health_indicators$representative_experience[i] = representatives_wide$Experience[which(representatives_wide$District==health_indicators$district[i])][2]
      
    } else {
      #first representative is closer
      health_indicators$representative_party[i] = representatives_wide$Party[which(representatives_wide$District==health_indicators$district[i])][1]
      health_indicators$representative_gender[i] = representatives_wide$Gender[which(representatives_wide$District==health_indicators$district[i])][1]
      health_indicators$representative_experience[i] = representatives_wide$Experience[which(representatives_wide$District==health_indicators$district[i])][1]
    }
    
  }
}
rm(i)

#manually set representatives who could not be automatically matched
health_indicators$representative[health_indicators$district=="FL-10"] = "Daniel Webster"
health_indicators$representative_party[health_indicators$district=="FL-10"] = "Republican"
health_indicators$representative_gender[health_indicators$district=="FL-10"] = "M"
health_indicators$representative_experience[health_indicators$district=="FL-10"] = 5
health_indicators$representative[health_indicators$district=="IL-8"] = "Tammy Duckworth"
health_indicators$representative_party[health_indicators$district=="IL-8"] = "Democratic"
health_indicators$representative_gender[health_indicators$district=="IL-8"] = "F"
health_indicators$representative_experience[health_indicators$district=="IL-8"] = 4
health_indicators$representative[health_indicators$district=="IN-9"] = "Todd Young"
health_indicators$representative_party[health_indicators$district=="IN-9"] = "Republican"
health_indicators$representative_gender[health_indicators$district=="IN-9"] = "M"
health_indicators$representative_experience[health_indicators$district=="IN-9"] = 6
health_indicators$representative[health_indicators$district=="MD-8"] = "Chris Van Hollen"
health_indicators$representative_party[health_indicators$district=="MD-8"] = "Democratic"
health_indicators$representative_gender[health_indicators$district=="MD-8"] = "M"
health_indicators$representative_experience[health_indicators$district=="MD-8"] = 4
health_indicators$representative[health_indicators$district=="NC-13"] = "George Holding"
health_indicators$representative_party[health_indicators$district=="NC-13"] = "Republican"
health_indicators$representative_gender[health_indicators$district=="NC-13"] = "M"
health_indicators$representative_experience[health_indicators$district=="NC-13"] = 4

#factor for changing ref group
health_indicators$representative_party = as.factor(health_indicators$representative_party)

#add regions
health_indicators$region = ifelse(health_indicators$state=="CT" | health_indicators$state=="ME" | health_indicators$state=="MA" | health_indicators$state=="NH" | health_indicators$state=="RI" | health_indicators$state=="VT" | health_indicators$state=="NJ" | health_indicators$state=="NY" | health_indicators$state=="PA", "Northeast", NA)
health_indicators$region = ifelse(health_indicators$state=="IL" | health_indicators$state=="IN" | health_indicators$state=="MI" | health_indicators$state=="OH" | health_indicators$state=="WI" | health_indicators$state=="IA" | health_indicators$state=="KS" | health_indicators$state=="MN" | health_indicators$state=="MO" | health_indicators$state=="NE" | health_indicators$state=="ND" | health_indicators$state=="SD", "Midwest", health_indicators$region)
health_indicators$region = ifelse(health_indicators$state=="DE" | health_indicators$state=="FL" | health_indicators$state=="GA" | health_indicators$state=="MD" | health_indicators$state=="NC" | health_indicators$state=="SC" | health_indicators$state=="VA" | health_indicators$state=="DC" | health_indicators$state=="WV" | health_indicators$state=="AL" | health_indicators$state=="KY" | health_indicators$state=="MS" | health_indicators$state=="TN" | health_indicators$state=="AR" | health_indicators$state=="LA" | health_indicators$state=="OK" | health_indicators$state=="TX", "South", health_indicators$region)
health_indicators$region = ifelse(health_indicators$state=="AZ" | health_indicators$state=="CO" | health_indicators$state=="ID" | health_indicators$state=="MT" | health_indicators$state=="NV" | health_indicators$state=="NM" | health_indicators$state=="UT" | health_indicators$state=="WY" | health_indicators$state=="AK" | health_indicators$state=="CA" | health_indicators$state=="HI" | health_indicators$state=="OR" | health_indicators$state=="WA", "West", health_indicators$region)
health_indicators$region = as.factor(health_indicators$region)

#save analytic dataset
write.csv(health_indicators, file="Mental_health_dataset.csv", row.names=F, na="")

#mapping data

#recode factors as numeric
districts@data$STATEFP = as.numeric(as.character(districts@data$STATEFP))
districts@data$CD114FP = as.numeric(as.character(districts@data$CD114FP))

#subset to lower 48
districts = districts[districts@data$STATEFP %in% state_fips$FIPS[state_fips$Lower48==1], ]

#join health indicators data
districts@data$mental_percent = NA
districts@data$bills = NA
for (i in 1:nrow(districts@data))
{
  state = state.abb[match(state_fips$State[state_fips$FIPS==districts@data$STATEFP[i]],state.name)]
  cd = ifelse(districts@data$CD114FP[i]==0, "AL", districts@data$CD114FP[i])
  if (!(is.na(state) | is.na(cd))==T) {
    districts@data$mental_percent[i] = health_indicators$mental_percent[health_indicators$district==paste(state,cd,sep="-")]
    districts@data$bills[i] = health_indicators$bills[health_indicators$district==paste(state,cd,sep="-")]
  }
}
rm(i,state,cd)

#save Rdata
save.image(file="indicators.RData")
load("indicators.RData")


### DESCRIPTIVES ###

hist(health_indicators$bills,breaks="fd")
describe(health_indicators$bills)
CrossTable(health_indicators$bills_cat)
describe(health_indicators$mental_percent)


### MIXED EFFECTS ANALYSIS ###

#check for sig clustering
model = lme(mental_percent ~ 1, random=~1|state, data= health_indicators, method="ML") 
model2 = lm(mental_percent ~ as.factor(state), data= health_indicators) 
anova(model,model2)

#correlation structure
model = lme(mental_percent ~ 1, random=~1|state, data= health_indicators, method="REML") 
model = lme(mental_percent ~ 1, random=~1|state, correlation=corSymm(), data= health_indicators, method="REML") 
model = lme(mental_percent ~ 1, random=~1|state, correlation=corAR1(), data= health_indicators, method="REML") 
model = lme(mental_percent ~ 1, random=~1|state, correlation=corCompSymm(), data= health_indicators, method="REML") 
summary(model)

#check for crude association
model = lme(mental_percent ~ bills, random=~1|state, correlation=corAR1(), data= health_indicators, method="ML") 
model = lme(mental_percent ~ bill_sponsored, random=~1|state, correlation=corAR1(), data= health_indicators, method="ML") 
model = lme(mental_percent ~ bill_cosponsored, random=~1|state, correlation=corAR1(), data= health_indicators, method="ML") 
summary(model)

#bill predictors
#model = lme(bills ~ percent_male + median_age + percent_white + percent_black + percent_hispanic + percent_unemployed + scale(median_income) + percent_foreign_born, random=~1|state, correlation=corAR1(), data=health_indicators, method="ML") 
#model = lme(bills ~ percent_male + percent_white + percent_black + percent_unemployed + scale(median_income) + percent_foreign_born + as.factor(representative_party) + as.factor(representative_gender) + representative_experience + as.factor(region), random=~1|state, correlation=corAR1(), data=health_indicators, method="ML") 
model = lme(bills ~ scale(mental_percent) + scale(percent_male) + scale(percent_white) + scale(percent_black) + scale(percent_unemployed) + scale(median_income) + scale(percent_foreign_born) + as.factor(representative_party) + as.factor(representative_gender) + relevel(region, ref="South"), random=~1|state, correlation=corAR1(), data=health_indicators, method="ML") 
model = lme(bill_sponsored ~ scale(mental_percent) + scale(percent_male) + scale(percent_white) + scale(percent_black) + scale(percent_unemployed) + scale(median_income) + scale(percent_foreign_born) + as.factor(representative_party) + relevel(region, ref="South"), random=~1|state, correlation=corAR1(), data=health_indicators, method="ML") 
model = lme(bill_cosponsored ~ scale(mental_percent) + scale(percent_male) + scale(percent_white) + scale(percent_black) + scale(percent_unemployed) + scale(median_income) + scale(percent_foreign_born) + as.factor(representative_party) + relevel(region, ref="South"), random=~1|state, correlation=corAR1(), data=health_indicators, method="ML") 
summary(model)
intervals(model, which="fixed")

#mental health predictors
#model = lme(mental_percent ~ percent_male + median_age + percent_white + percent_black + percent_hispanic + percent_citizen_vote + percent_unemployed + scale(median_income) + percent_foreign_born, random=~1|state, correlation=corAR1(), data=health_indicators, method="ML") 
#model = lme(mental_percent ~ percent_male + median_age + percent_white + percent_black + percent_hispanic + percent_unemployed + scale(median_income) + percent_foreign_born + as.factor(representative_party) + as.factor(representative_gender) + representative_experience + as.factor(region), random=~1|state, correlation=corAR1(), data=health_indicators, method="ML") 
#model = lme(mental_percent ~ percent_male + median_age + percent_white + percent_black + percent_hispanic + percent_unemployed + scale(median_income) + percent_foreign_born + as.factor(representative_party) + as.factor(region), random=~1|state, correlation=corAR1(), data=health_indicators, method="ML") 
model = lme(mental_percent ~ bills + scale(percent_male) + scale(percent_white) + scale(percent_black) + scale(percent_unemployed) + scale(median_income) + scale(percent_foreign_born) + as.factor(representative_party) + relevel(region, ref="South"), random=~1|state, correlation=corAR1(), data= health_indicators, method="ML") 
model = lme(mental_percent ~ bill_sponsored + bill_cosponsored + scale(percent_male) + scale(percent_white) + scale(percent_black) + scale(percent_unemployed) + scale(median_income) + scale(percent_foreign_born) + as.factor(representative_party) + relevel(region, ref="South"), random=~1|state, correlation=corAR1(), data= health_indicators, method="ML") 
model = lme(mental_percent ~ bill_sponsored + scale(percent_male) + scale(percent_white) + scale(percent_black) + scale(percent_unemployed) + scale(median_income) + scale(percent_foreign_born) + as.factor(representative_party) + relevel(region, ref="South"), random=~1|state, correlation=corAR1(), data= health_indicators, method="ML") 
model = lme(mental_percent ~ bill_cosponsored + scale(percent_male) + scale(percent_white) + scale(percent_black) + scale(percent_unemployed) + scale(median_income) + scale(percent_foreign_born) + as.factor(representative_party) + relevel(region, ref="South"), random=~1|state, correlation=corAR1(), data= health_indicators, method="ML") 
summary(model)
intervals(model, which="fixed")

#dichotomized any bill predictor
model = glmer(bills_cat ~ (1 | state) + scale(mental_percent) + scale(percent_male) + scale(percent_white) + scale(percent_black) + scale(percent_unemployed) + scale(median_income) + scale(percent_foreign_born) + relevel(representative_party, ref="Republican") + as.factor(representative_gender) + relevel(region, ref="South"), family=binomial(), data=health_indicators) 
summary(model)
round(exp(fixef(model)),2)
round(exp(confint.merMod(model, method="Wald")),2)


### MAPS ###

#choropleth maps
tiff("Figure1a.tif",height=4,width=6,units='in',res=1200)
spplot(districts, "mental_percent", cuts=8, col.regions=brewer.pal(9, "Reds"), col="transparent", main="a.", par.settings=list(axis.line=list(col=NA)))
dev.off()

tiff("Figure1b.tif",height=4,width=6,units='in',res=1200)
spplot(districts, "bills", cuts=8, col.regions=brewer.pal(9, "Blues"), col="transparent", main="b.", par.settings=list(axis.line=list(col=NA)))
dev.off()
