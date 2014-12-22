require(gbm)
require(sqldf)
##options(java.parameters = "-Xmx32g")
require(extraTrees)
my_cap<-function(v, l, h){
  v[v<l]<-l
  v[v>h]<-h
  return(v)
}
resodf <- read.csv("resources.csv")
projdf <- read.csv("projects.csv")
outcomedf <- read.csv("outcomes.csv")
activeTeacherdf <- read.csv("activeTeacher20100401.csv")
whichSchooldf <- read.csv("whichSchool.csv")
schoolDistrictdf <- read.csv("schoolDistrict20100401.csv ")
schoolInCitydf <- read.csv("schoolInCity_20100401.csv")
schoolInCountydf <- read.csv("schoolInCounty_20100401.csv")
zipOfSchooldf <- read.csv("zipOfSchool_20100401.csv")
stateOfSchooldf <- read.csv("stateOfSchool.csv")
getDonorEachTeacherdf <- read.csv("getDonorEachTeacher.csv")
getDonorEachTeacherdf <- getDonorEachTeacherdf[,!(colnames(getDonorEachTeacherdf) %in% c("eachTeacherActDays","teacherActProjCount", "teacherActProjCount_train"))]
getDonorEachSchooldf <- read.csv("getDonorEachSchool.csv")
getDonorEachSchooldf <- getDonorEachSchooldf[,!(colnames(getDonorEachSchooldf) %in% c("daysOfSchool","cntOfSchool", "cntOfSchool_train"))]
resodf$totalPrice <- with(resodf, unitPriceOfItem*quantityOfItem)
resodf$resourceType <- with(resodf, ifelse(project_typeOfReso=="","Other", as.character(project_typeOfReso)))
resoBookdf <- sqldf("select projectid, sum(1) as booksCount, sum(totalPrice) as totalAmountBook
                    from resodf where resourceType='Books' group by projectid")
supplyResodf <- sqldf("select projectid, sum(1) as countSupplies, sum(totalPrice) as totalAmountSupplies
                      from resodf where resourceType='Supplies' group by projectid")
tripsResodf <- sqldf("select projectid, sum(1) as CountTrips, sum(totalPrice) as totalAmountOfItems
                     from resodf where resourceType='Trips' group by projectid")
otherInResodf <- sqldf("select projectid, sum(1) as countOther, sum(totalPrice) as totalAmountOfOther
                       from resodf where resourceType='Other' group by projectid")
teachInResodf <- sqldf("select projectid, sum(1) as countTech, sum(totalPrice) as totalAmountOfTech
                       from resodf where resourceType='Technology' group by projectid")
Visitorsdf <- sqldf("select projectid, sum(1) as VisitorsCount, sum(totalPrice) as totalAmountOfVisitors
                    from resodf where resourceType='Visitors' group by projectid")
Aggdf <- sqldf("select projectid, sum(1) as agg_cnt, sum(totalPrice) as totalAmountOfAgg
               from resodf group by projectid")

activeTeacherdf <- activeTeacherdf[,!(colnames(activeTeacherdf)=="whenPost")]
whichSchooldf <- whichSchooldf[,!(colnames(whichSchooldf)=="whenPost")]
schoolInCitydf <- schoolInCitydf[,!(colnames(schoolInCitydf)=="whenPost")]
schoolInCountydf <- schoolInCountydf[,!(colnames(schoolInCountydf)=="whenPost")]
schoolDistrictdf <- schoolDistrictdf[,!(colnames(schoolDistrictdf)=="whenPost")]
zipOfSchooldf <- zipOfSchooldf[,!(colnames(zipOfSchooldf)=="whenPost")]
stateOfSchooldf <- stateOfSchooldf[,!(colnames(stateOfSchooldf)=="whenPost")]
schoolDistrictdf$previousSchoolDist <- as.numeric(as.character(schoolDistrictdf$previousSchoolDist))
schoolInCitydf$previousSchoolCity <- as.numeric(as.character(schoolInCitydf$previousSchoolCity))

df <- merge(projdf, outcomedf, by.x="projectid", by.y="projectid", all.x=T)
df <- merge(df, activeTeacherdf, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, whichSchooldf, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, schoolInCitydf, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, schoolInCountydf, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, zipOfSchooldf, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, schoolDistrictdf, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, stateOfSchooldf, by.x="projectid",by.y="projectid", all.x=T)

Vec <- c(40:149)
for (i in 1:length(Vec)) {
  c <- Vec[i]
  df[,c][is.na(df[,c])] <- 0
}

#df <- merge(df, text_preds_ydf, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, getDonorEachTeacherdf, by.x="projectid", by.y="projectid", all.x=T)
df <- merge(df, getDonorEachSchooldf, by.x="projectid", by.y="projectid", all.x=T)
df <- merge(df, Aggdf, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, resoBookdf, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, tripsResodf, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, supplyResodf, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, otherInResodf, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, teachInResodf, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, Visitorsdf, by.x="projectid",by.y="projectid", all.x=T)

df$agg_cnt[is.na(df$agg_cnt)] <- 0
df$totalAmountOfAgg[is.na(df$totalAmountOfAgg)] <- 0
df$booksCount[is.na(df$booksCount)] <- 0
df$totalAmountBook[is.na(df$totalAmountBook)] <- 0
df$totalAmountOfItems <- as.numeric(df$totalAmountOfItems)
df$CountTrips[is.na(df$CountTrips)] <- 0
df$totalAmountOfItems[is.na(df$totalAmountOfItems)] <- 0
df$countOther[is.na(df$countOther)] <- 0
df$totalAmountOfOther[is.na(df$totalAmountOfOther)] <- 0
df$countTech[is.na(df$countTech)] <- 0
df$totalAmountOfTech[is.na(df$totalAmountOfTech)] <- 0
df$totalAmountOfVisitors <- as.numeric(df$totalAmountOfVisitors)
df$VisitorsCount[is.na(df$VisitorsCount)] <- 0
df$totalAmountOfVisitors[is.na(df$totalAmountOfVisitors)] <- 0
df$countSupplies[is.na(df$countSupplies)] <- 0
df$totalAmountSupplies[is.na(df$totalAmountSupplies)] <- 0

rm(outcomedf)
rm(projdf)
rm(Aggdf)
rm(resoBookdf)
rm(otherInResodf)
rm(tripsResodf)
rm(supplyResodf)
rm(teachInResodf)
rm(Visitorsdf)
rm(resodf)
rm(schoolInCitydf)
rm(schoolInCountydf)
rm(schoolDistrictdf)
rm(zipOfSchooldf)
rm(whichSchooldf)
rm(stateOfSchooldf)
rm(activeTeacherdf)
#rm(text_preds_ydf)
rm(getDonorEachSchooldf)
rm(getDonorEachTeacherdf)
df$whenPost_posix <- as.POSIXlt(df$whenPost)
df$split <- "train"
df$split[df$whenPost_posix<as.POSIXlt("2011-01-01")] <- "none"
df$split[df$whenPost_posix>=as.POSIXlt("2013-05-01")] <- "test"
#df$split[df$whenPost_posix>=as.POSIXlt("2014-01-01")] <- "test"
df$ref_date <- as.POSIXlt("2010-12-31")
df$time <- with(df, as.numeric(difftime(whenPost_posix, ref_date,unit="days")))
df$log_time <- log(df$time + 1)

df$previousSchoolRate <- with(df, ifelse(cntOfSchool==0,0,previousSchoolID/cntOfSchool))
df$previousTeacherActRate <- with(df, ifelse(teacherActProjCount==0,0,teacher_acctid_y_prev/teacherActProjCount))
df$previousSchoolCity_rate <- with(df, ifelse(schoolCityNum==0,0,previousSchoolCity/schoolCityNum))
df$previousZipRateOfSchool <- with(df, ifelse(schoolZipNum==0,0,school_zip_y_prev/schoolZipNum))
df$previousSchoolDist_rate <- with(df, ifelse(schoolDistNum==0,0,previousSchoolDist/schoolDistNum))
df$previousSchoolCountyRate <- with(df, ifelse(schoolCountyNum==0,0, previousSchoolCountyRate /schoolCountyNum))
df$previousStateRate <- with(df, ifelse(stateNum==0,0, previousStateRate /stateNum))

df$ previousSchoolRateY2 <- with(df, ifelse(cntOfSchool==0,0, previousSchoolRateY2/cntOfSchool))
df$ previousTeacherActRateY2 <- with(df, ifelse(teacherActProjCount==0,0, previousTeacherActRateY2/teacherActProjCount))
df$ previousSchoolCity_rateY2 <- with(df, ifelse(schoolCityNum==0,0, previousSchoolCity_rate Y2/schoolCityNum))
df$ previousZipRateOfSchoolY2 <- with(df, ifelse(schoolZipNum==0,0, previousZipRateOfSchool Y2/schoolZipNum))
df$ previousSchoolDist_rateY2 <- with(df, ifelse(schoolDistNum==0,0,school_district_y2_prev/schoolDistNum))
df$ previousSchoolCountyRateY2 <- with(df, ifelse(schoolCountyNum==0,0, previousSchoolCountyRate Y2/schoolCountyNum))
df$ previousStateRateY2 <- with(df, ifelse(stateNum==0,0, previousStateRateY2/stateNum))

df$ previousSchoolRateY3 <- with(df, ifelse(cntOfSchool==0,0, previousSchoolRateY3/cntOfSchool))
df$previousTeacherActRateY3 <- with(df, ifelse(teacherActProjCount==0,0,teacher_acctid_y3_prev/teacherActProjCount))
df$previousSchoolCityY3_rate <- with(df, ifelse(schoolCityNum==0,0,previousSchoolCityY3/schoolCityNum))
df$previousZipRateOfSchoolY3_rate <- with(df, ifelse(schoolZipNum==0,0,previousZipRateOfSchoolY3/schoolZipNum))
df$previousSchoolDistY3 <- with(df, ifelse(schoolDistNum==0,0,previousSchoolDistY3/schoolDistNum))
df$previousSchoolCountyY3_rate <- with(df, ifelse(schoolCountyNum==0,0,previousSchoolCountyY3/schoolCountyNum))
df$previousStateY3_rate <- with(df, ifelse(stateNum==0,0,previousStateY3/stateNum))

df$ previousSchoolRateY4 <- with(df, ifelse(cntOfSchool==0,0, previousSchoolRateY4/cntOfSchool))
df$previousTeacherActRateY4 <- with(df, ifelse(teacherActProjCount==0,0,teacher_acctid_y4_prev/teacherActProjCount))
df$previousSchoolCityY4_rate <- with(df, ifelse(schoolCityNum==0,0,previousSchoolCityY4/schoolCityNum))
df$previousZipRateOfSchoolY4_rate <- with(df, ifelse(schoolZipNum==0,0,previousZipRateOfSchoolY4/schoolZipNum))
df$previousSchoolDistY4_rate <- with(df, ifelse(schoolDistNum==0,0,previousSchoolDistY4/schoolDistNum))
df$previousSchoolCountyY4_rate <- with(df, ifelse(schoolCountyNum==0,0,previousSchoolCountyY4/schoolCountyNum))
df$previousStateY4_rate <- with(df, ifelse(stateNum==0,0,previousStateY4/stateNum))

df$ previousSchoolRateY5 <- with(df, ifelse(cntOfSchool==0,0, previousSchoolRateY5/cntOfSchool))
df$previousTeacherActRateY5 <- with(df, ifelse(teacherActProjCount==0,0,teacher_acctid_y5_prev/teacherActProjCount))
df$previousSchoolCityY5_rate <- with(df, ifelse(schoolCityNum==0,0,previousSchoolCityY5/schoolCityNum))
df$previousZipRateOfSchoolY5_rate <- with(df, ifelse(schoolZipNum==0,0,previousZipRateOfSchoolY5/schoolZipNum))
df$previousSchoolDistY5_rate <- with(df, ifelse(schoolDistNum==0,0,previousSchoolDistY5/schoolDistNum))
df$previousSchoolCountyY5_rate <- with(df, ifelse(schoolCountyNum==0,0,previousSchoolCountyY5/schoolCountyNum))
df$previousStateY5_rate <- with(df, ifelse(stateNum==0,0,previousStateY5/stateNum))

df$ previousSchoolRateY6 <- with(df, ifelse(cntOfSchool==0,0, previousSchoolRateY6/cntOfSchool))
df$previousTeacherActRateY6_rate <- with(df, ifelse(teacherActProjCount==0,0,previousTeacherActRateY6/teacherActProjCount))
df$previousSchoolCityY6_rate <- with(df, ifelse(schoolCityNum==0,0,previousSchoolCityY6/schoolCityNum))
df$previousZipRateOfSchoolY6_rate <- with(df, ifelse(schoolZipNum==0,0,previousZipRateOfSchoolY6/schoolZipNum))
df$previousSchoolDistY6_rate <- with(df, ifelse(schoolDistNum==0,0,previousSchoolDistY6/schoolDistNum))
df$previousSchoolCountyY6_rate <- with(df, ifelse(schoolCountyNum==0,0,previousSchoolCountyY6/schoolCountyNum))
df$previousStateY6_rate <- with(df, ifelse(stateNum==0,0,previousStateY6/stateNum))

df$ previousSchoolRateY7 <- with(df, ifelse(cntOfSchool==0,0, previousSchoolRateY7/cntOfSchool))
df$previousTeacherActRateY7_rate <- with(df, ifelse(teacherActProjCount==0,0,previousTeacherActRateY7/teacherActProjCount))
df$previousSchoolCityY7_rate <- with(df, ifelse(schoolCityNum==0,0,previousSchoolCityY7/schoolCityNum))
df$previousZipRateOfSchoolY7_rate <- with(df, ifelse(schoolZipNum==0,0,previousZipRateOfSchoolY7/schoolZipNum))
df$previousSchoolDistY7_rate <- with(df, ifelse(schoolDistNum==0,0,previousSchoolDistY7/schoolDistNum))
df$previousSchoolCountyY7_rate <- with(df, ifelse(schoolCountyNum==0,0,previousSchoolCountyY7/schoolCountyNum))
df$previousStateY7_rate <- with(df, ifelse(stateNum==0,0,previousStateY7/stateNum))

df$previousSchoolRateY8 <- with(df, ifelse(cntOfSchool==0,0,schoolid_y8_prev/cntOfSchool))
df$previousTeacherActRateY8_rate <- with(df, ifelse(teacherActProjCount==0,0,previousTeacherActRateY8/teacherActProjCount))
df$previousSchoolCityY8_rate <- with(df, ifelse(schoolCityNum==0,0,previousSchoolCityY8/schoolCityNum))
df$previousZipRateOfSchoolY8_rate <- with(df, ifelse(schoolZipNum==0,0,previousZipRateOfSchoolY8/schoolZipNum))
df$previousSchoolDistY8_rate <- with(df, ifelse(schoolDistNum==0,0,previousSchoolDistY8/schoolDistNum))
df$previousSchoolCountyY8_rate <- with(df, ifelse(schoolCountyNum==0,0,previousSchoolCountyY8/schoolCountyNum))
df$previousStateY8_rate <- with(df, ifelse(stateNum==0,0,previousStateY8/stateNum))

df$previousSchoolRateY9 <- with(df, ifelse(cntOfSchool==0,0,schoolid_y9_prev/cntOfSchool))
df$previousTeacherActRateY9_rate <- with(df, ifelse(teacherActProjCount==0,0,previousTeacherActRateY9/teacherActProjCount))
df$previousSchoolCityY9_rate <- with(df, ifelse(schoolCityNum==0,0,previousSchoolCityY9/schoolCityNum))
df$previousZipRateOfSchoolY9_rate <- with(df, ifelse(schoolZipNum==0,0,previousZipRateOfSchoolY9/schoolZipNum))
df$previousSchoolDistY9_rate <- with(df, ifelse(schoolDistNum==0,0,previousSchoolDistY9/schoolDistNum))
df$previousSchoolCountyY9_rate <- with(df, ifelse(schoolCountyNum==0,0,previousSchoolCountyY9/schoolCountyNum))
df$previousStateY9_rate <- with(df, ifelse(stateNum==0,0,previousStateY9/stateNum))

df$previousSchoolRateY10 <- with(df, ifelse(cntOfSchool==0,0,schoolid_y10_prev/cntOfSchool))
df$previousTeacherActRateY10_rate <- with(df, ifelse(teacherActProjCount==0,0,previousTeacherActRateY10/teacherActProjCount))
df$previousSchoolCityY10_rate <- with(df, ifelse(schoolCityNum==0,0,previousSchoolCityY10/schoolCityNum))
df$previousZipRateOfSchoolY10_rate <- with(df, ifelse(schoolZipNum==0,0,previousZipRateOfSchoolY10/schoolZipNum))
df$previousSchoolDistY10_rate <- with(df, ifelse(schoolDistNum==0,0,previousSchoolDistY10/schoolDistNum))
df$previousSchoolCountyY10_rate <- with(df, ifelse(schoolCountyNum==0,0,previousSchoolCountyY10/schoolCountyNum))
df$previousStateY10_rate <- with(df, ifelse(stateNum==0,0,previousStateY10/stateNum))


df$y <- 0
df$y[df$excitingProbability=="t"] <- 1
df$y2 <- 0
df$y2[df$moreThanOneRefer=="t"] <- 1
df$y3 <- 0
df$y3[df$goodReview=="t"] <- 1
df$y4 <- 0
df$y4[df$gotFullFund=="t"] <- 1
df$y5 <- 0
df$y5[df$moreThanOneGreen=="t"] <- 1
df$y6 <- 0
df$y6[df$fromReputaionDonor=="t"] <- 1
df$y7 <- 0
df$y7[df$threeNoneTeacherRefer=="t"] <- 1
df$y8 <- 0
df$y8[df$oneNoneTeacherRefer=="t"] <- 1
df$y9 <- 0
df$y9[df$TeacherReferNum>=1] <- 1
df$y10 <- 0
df$y10[df$noneTeacherReferNum>=1] <- 1

df$poverty <- 0
df$poverty[df$poverty =="average poverty"] <- 1
df$poverty[df$poverty =="high level poverty"] <- 2
df$poverty[df$poverty =="highest poverty"] <- 3
df$grade_level_num <- 0
df$gradeLevel[df$grade =="Grades 3-4"] <- 1
df$gradeLevel[df$grade=="Grades 5-9"] <- 2
df$gradeLevel[df$grade=="Grades 10-12"] <- 3
df$weekday <- weekdays(df$whenPost_posix)
df$weekday <- as.factor(df$weekday)


df$school_district_days <- as.numeric(df$school_district_days)
df$price_per_student <- with(df, TotallyGot/students_reached)


kdd_gbm_v1 <- gbm(y~Latitude+
                    Longitude+
                    Metro+
                    titleOfTeacher+
                    teachForAmeri+
                    teachFeoolw+
                    TotallyGot+
                    studentsImp+
                    doubleImpact+
                    homeMatch+
                    poverty+
                    originalArea+
                    typeOfReso+
                    
                    
                    totalAmountOfAgg+
                    agg_cnt+                 
                    booksCount+
                    totalAmountBook+
                    totalAmountSupplies+
                    totalAmountOfOther+
                    totalAmountOfTech+
                    weekday+
                    eachTeacherActDays+
                    daysOfSchool+
                    teacherActProjCount+
                    cntOfSchool+
                    previousTeacherActRate_cred+
                    previousSchoolRate_cred_cap+ 
                    previousSchoolDist_rate+                    
                    previousSchoolCountRate+
                    previousSchoolRateY2+                 
                    previousTeacherActRateY2+                            
                    previousSchoolRateY3+                 
                    previousTeacherActRateY3+                            
                    previousSchoolDistY3+
                    previousSchoolRateY4+                                        
                    previousSchoolRateY5+                 
                    previousTeacherActRateY5+
                    eachStuPrice+
                    previousSchoolRateY11+
                    previousTeacherActRate+
                    previousSchoolRateY12+
                    previousTeacherActRateY12+
                    previousSchoolRateY13+
                    previousTeacherActRateY13,                
                  ,
                  data = df[split,],
                  train.fraction = 1,
                  distribution="bernoulli",
                  interaction.depth=7,
                  n.minobsinnode = 700,
                  shrinkage = 0.1,
                  bag.fraction = 0.5,
                  verbose=T,
                  n.trees = 650)

df$whenPost_string <- as.character(df$whenPost)
tmp1 <- df[df$whenPost_posix >= as.POSIXlt("2010-09-01"),c("projectid", "whenPost_string","predictV1")]
tmp1 <- sqldf("select whenPost_string, sum(1) as whenPost_cnt, sum(predictV1) as previousSumV1 from tmp1 group by whenPost_string")
previousSum <- filter(tmp1$previousSumV1,rep(1,14),sides=1)
cnt_sum <- filter(tmp1$whenPost_cnt,rep(1,14),sides=1)
tmp1$predictTwoWeeksAvgV1 <- previousSum/cnt_sum
tmp1$predictTwoWeeksAvgV1[2:1350] <- tmp1$predictTwoWeeksAvgV1[1:1349]
previousSum <- filter(tmp1$previousSumV1,rep(1,30),sides=1)
cnt_sum <- filter(tmp1$whenPost_cnt,rep(1,30),sides=1)
tmp1$eachMonthPredAvgV1 <- previousSum/cnt_sum
tmp1$eachMonthPredAvgV1[2:1350] <- tmp1$eachMonthPredAvgV1[1:1349]
previousSum <- filter(tmp1$previousSumV1,rep(1,61),sides=1)
cnt_sum <- filter(tmp1$whenPost_cnt,rep(1,61),sides=1)
tmp1$prevTwoMonthesAverage <- previousSum/cnt_sum
tmp1$prevTwoMonthesAverage[3:1350] <- tmp1$prevTwoMonthesAverage[2:1349]
df <- merge(df, tmp1, by.x="whenPost_string",by.y="whenPost_string",all.x=T)
rm(tmp1)
df <- df[order(df$time),]



kdd_gbm_v2 <- gbm(y~Latitude+
                    Longitude+
                    Metro+
                    titleOfTeacher+
                    teachForAmeri+
                    teachFeoolw+
                    TotallyGot+
                    studentsImp+
                    doubleImpact+
                    homeMatch+
                    poverty+
                    originalArea+
                    typeOfReso+
                    totalAmountOfAgg+
                    agg_cnt+                 
                    booksCount+
                    totalAmountBook+
                    totalAmountSupplies+
                    totalAmountOfOther+
                    totalAmountOfTech+
                    weekday+
                    eachTeacherActDays+
                    daysOfSchool+
                    teacherActProjCount+
                    cntOfSchool+
                    previousTeacherActRate_cred+
                    previousSchoolRate_cred_cap+ 
                    previousSchoolDist_rate+                    
                    previousSchoolCountRate+
                    previousSchoolRateY2+                 
                    previousTeacherActRateY2+                            
                    previousSchoolRateY3+                 
                    previousTeacherActRateY3+                            
                    previousSchoolDistY3+
                    previousSchoolRateY4+                                        
                    previousSchoolRateY5+                 
                    previousTeacherActRateY5+
                    eachStuPrice+
                    previousSchoolRateY11+
                    previousTeacherActRate+
                    previousSchoolRateY12+
                    previousTeacherActRateY12+
                    previousSchoolRateY13+
                    previousTeacherActRateY13+
                    eachMonthPredAvgV1,                
                  ,
                  data = df[split,],
                  train.fraction = 1,
                  distribution="bernoulli",
                  interaction.depth=7,
                  n.minobsinnode = 750,
                  shrinkage = 0.1,
                  bag.fraction = 0.5,
                  verbose=T,
                  n.trees = 600)
df$predictV2<- predict(kdd_gbm_v2, newdata=df, n.trees=600, type="response")


x <- model.matrix(~ school_metro+
                             titleOfTeacher+
                             teachForAmeri+
                             studentsImp+
                             eachStuPrice+
                             teachFeoolw+
                             TotallyGot+
                             doubleImpact+
                             homeMatch+
                             poverty+
                             originalArea+
                             typeOfReso+
                             totalAmountOfAgg+
                             agg_cnt+                 
                             booksCount+
                             totalAmountBook+
                             totalAmountSupplies+
                             totalAmountOfOther+
                             totalAmountOfTech+
                             weekday+
                             teacherActProjCount+
                             cntOfSchool+
                             previousTeacherActRate_cred+
                             previousSchoolRate_cred_cap+ 
                             previousSchoolDist_rate+                    
                             previousSchoolCountRate+
                             previousSchoolRateY2+                 
                             previousTeacherActRateY2+                            
                             previousSchoolRateY3+                 
                             previousTeacherActRateY3+                            
                             previousSchoolDistY3+
                             previousSchoolRateY4+                                        
                             previousSchoolRateY5+                 
                             previousTeacherActRateY5+
                             previousSchoolRateY11+
                             previousTeacherActRate+
                             previousSchoolRateY12+
                             previousTeacherActRateY12+
                             previousSchoolRateY13+
                             previousTeacherActRateY13+
                             predictTwoWeeksAvgV1, data=df[split,])
y <- df$y[split]

x_test <- expectationOut.matrix(~school_metro+
                                  titleOfTeacher+
                                  teachForAmeri+
                                  studentsImp+
                                  eachStuPrice+
                                  teachFeoolw+
                                  TotallyGot+
                                  doubleImpact+
                                  homeMatch+
                                  poverty+
                                  originalArea+
                                  typeOfReso+
                                  totalAmountOfAgg+
                                  agg_cnt+                 
                                  booksCount+
                                  totalAmountBook+
                                  totalAmountSupplies+
                                  totalAmountOfOther+
                                  totalAmountOfTech+
                                  weekday+
                                  teacherActProjCount+
                                  cntOfSchool+
                                  previousTeacherActRate_cred+
                                  previousSchoolRate_cred_cap+ 
                                  previousSchoolDist_rate+                    
                                  previousSchoolCountRate+
                                  previousSchoolRateY2+                 
                                  previousTeacherActRateY2+                            
                                  previousSchoolRateY3+                 
                                  previousTeacherActRateY3+                            
                                  previousSchoolDistY3+
                                  previousSchoolRateY4+                                        
                                  previousSchoolRateY5+                 
                                  previousTeacherActRateY5+
                                  previousSchoolRateY11+
                                  previousTeacherActRate+
                                  previousSchoolRateY12+
                                  previousTeacherActRateY12+
                                  previousSchoolRateY13+
                                  previousTeacherActRateY13+
                                  predictTwoWeeksAvgV1, data=df[df$split=="test",])

df$ref_date <- as.POSIXlt("2014-12-12")
df$timeToEnd <- with(df, as.numeric(difftime(ref_date, whenPost_posix,unit="days")))
proportionLow <- 0.5 
slope <- (1-proportionLow)/131
df$dis <- with(df, timeToEnd*slope + proportionLow)

df_test <- df[df$split=="test",c("projectid","predictV1","predictV2","dis")]

df_test$excitingProbability <- with(df_test, 0.5*predictV1+0.5*predictV2)
write.csv(df_test[,c("projectid","excitingProbability","dis")],"expectationOut.csv",row.names=F)
