library(RODBC)
library(stringr)
library(plyr)
library(dplyr)

uid = "ommitted to post on github"
pwd = "ommitted to post on github"
data=NULL
Raw_Data=NULL
MIN_YEAR_TO_SHOW=as.integer( format(Sys.Date(), "%Y"))-7
Year_Semester=NULL
Year_Cap=NULL
Years_Tracked<-NULL

get_data<- function(semester,year, years_wanted,year_semester=NULL,year_cap=NULL){
  connection = odbcConnect("OIEA_SERVER",uid,pwd)
  if(is.null(year_semester))
    Year_Semester<<-get_Year_Semester(year, semester)
  else
    Year_Semester<<-year_semester
  
  Years_Tracked<<-years_wanted
  
  if(is.null(year_cap)){
    Year_Cap<<-get_Year_Sem_cap(year,semester,years_wanted)#year and semester
  }
  else{
    Year_Cap<<-year_cap
  }
  query = paste0("select enrolled.Student_Id, Cohort, enrolled.Year_Semester, enrolled.Grade,sections2.Discipline, sections2.Course_Number, Levels_Below_Transfer,
                 Units,UC_Transferable,CSU_Transferable, case when Award is null then 0 else 1 end as Received_Award, Education_Goal,
                 DATEDIFF(day,Birthdate,terms.Term_Start_Date)/365 as Term_Age ,Ethnicity, Gender, student_profiles.BOGG, student_profiles.Pell_Grant, 
                 student_profiles.EOPS,student_profiles.DSPS, Residency_Status              
                 from cohorts
                 right join(	select *
                 from enrollments
                 where College='E' and Drop_Date IS NULL and Grade!='E') as enrolled
                 on enrolled.Student_Id= cohorts.Student_Id and enrolled.College=cohorts.Cohort_College
                 left join sections2 on sections2.College=cohorts.Cohort_College and sections2.Year_Semester=enrolled.Year_Semester
                 and sections2.Section_Number=enrolled.Section_Number
                 left join courses on courses.Curriculum_Id=sections2.Curriculum_Id and courses.Curriculum_Version=sections2.Curriculum_Version
                 left join terms on enrolled.College=terms.College and enrolled.Year_Semester=terms.Year_Semester
                 left join students on students.Student_Id=cohorts.Student_Id
                 LEFT join (
                 
                 select result.Student_Id, BOGG,DSPS,EOPS, Education_Goal,Pell_Grant, Residency_Status
                 from (select cohorts.Student_ID,Max(DSPS) AS DSPS,max(EOPS) as EOPS,MAX(BOGG) as BOGG,MAX(Pell_Grant) as Pell_Grant, Residency_Status
                 from cohorts 
                 left join student_profiles on student_profiles.College=cohorts.Cohort_College and student_profiles.Student_Id=cohorts.Student_Id and  cohorts.Cohort=student_profiles.Year_Semester
                 where cohorts.Cohort=",Year_Semester," and cohorts.Cohort_College='E' 
                 group by cohorts.Student_Id, Residency_Status) as result
                 left join (select distinct Student_Id, Education_Goal
                 from student_profiles
                 where College='E' and Year_Semester=",Year_Semester,") as goals on goals.Student_Id=result.Student_Id
                 ) as student_profiles on student_profiles.Student_Id=cohorts.Student_Id 
                 full outer join (select Student_Id, count(distinct Student_Id) as Award
                 from awards
                 where College='E' and Year(Award_Date)>='",substr(Year_Semester,1,4) ,"' and Year(Award_Date)<='",substr(Year_Cap,1,4) ,"'
                 group by Student_Id) as awards_ on awards_.Student_Id=cohorts.Student_Id
                 where Cohort_College='E' and Cohort='",Year_Semester,"' and enrolled.Year_Semester>='",Year_Semester,"' 
                 and enrolled.Year_Semester<='",Year_Cap,"'")
  
  data <<- sqlQuery(connection, query, stringsAsFactors = FALSE)
  odbcClose(connection)
  return(data)
}

#function used to add a row to data including
# 1)number of students awardedAA/AS degrees
# 2)number of students awarded ADT degrees
# 3)number of students awarded Certificates over 18 units\
#range in years is from initial year to year cap
get_award_data<-function(raw_data,initial_year,year_cap){
  
  query=paste(
    "select Student_Id, awards.Award_Type, programs.Required_Units
    from awards    
    left join programs on programs.Catalog_Year=awards.Catalog_Year AND programs.Major_Code=awards.Major_Code
    and programs.College=awards.College AND programs.Award_Type=awards.Award_Type
     where awards.College='E' and Year(Award_Date)>='",initial_year,"' and Year(Award_Date)<='",year_cap,"'")
  
  connection = odbcConnect("OIEA_SERVER",uid,pwd)
  result <- sqlQuery(connection, query, stringsAsFactors = FALSE)
  odbcClose(connection)
  
  #pull out cohort
  print(unique(raw_data$Pell_Grant))
  result<-result[result$Student_Id %in% raw_data[raw_data$Received_Award==1,]$Student_Id,]
  print(paste("Raw_data:",length(unique(raw_data[raw_data$Received_Award==1,]$Student_Id)),"   result:",length(unique(result$Student_Id))))
    
  AA_AS=length(unique(result[result$Award_Type=="AA" | result$Award_Type=="AS",]$Student_Id))
  ADT=length(unique(result[result$Award_Type=="AT" | result$Award_Type=="SV",]$Student_Id))
  Certificate_18_Over=length(unique(result[result$Required_Units>=18,]$Student_Id))
  
  result=data.frame(Categories=c("Received Cert. >18 Units","Recieved AA/AS","Received ADT Award"),Total=c(Certificate_18_Over,AA_AS,ADT))

  data<<- rbind(data,result)
  return(data)
}

thin_the_herd<-function(ethnicity, age_range, gender, bogg, dsps, eops,english_placement,math_placement, goals,pell_grant,ab540){
  
  if(english_placement=="1 Level Below Transfer")
    english_placement="1 Levels Below Transfer"
  
  if(math_placement=="1 Level Below Transfer")
    math_placement="1 Levels Below Transfer"
  
  if(ethnicity!="ALL")
    data<<-data[data$Ethnicity==ethnicity,]
  
  if(age_range!="ALL"){
    age_min=as.integer(substr(age_range,1,2))
    age_max=100
    if(is.na(age_min)){#is 18 and younger
      age_max=17
      age_min=0
    }
    else if(age_min<40){
      age_max=as.integer(substr(age_range,4,5)) 
    }
    
    data<<-data[data$Term_Age>=age_min & data$Term_Age<=age_max,]
  }
  
  if(gender!="ALL"){
    data<<-data[data$Gender==substr(gender,1,1),]
  }
  
  if(bogg!="ALL"){
    BOGG=F
    
    if(bogg=="Yes")    
      BOGG=TRUE
    
    data<<-data[data$BOGG==BOGG,]
  }
  
  if(pell_grant!="ALL"){
    PELL=F
    
    if(pell_grant=="Yes")    
      PELL=T
    
    data<<-data[data$Pell_Grant==PELL,]
  }
  
  if(dsps!="ALL"){
    DSPS=F
    
    if(dsps=="Yes")
      DSPS=T
    
    data<<-data[data$DSPS==DSPS,]
  }
  
  if(eops!="ALL"){
    EOPS=F
    
    if(eops=="Yes")
      EOPS=T
    
    data<<-data[data$EOPS==EOPS,]
  }
  if(ab540!="ALL"){
    if(ab540=="Yes"){
      data<<-data[data$Residency_Status=="AB 540",]}
    else{
      data<<-data[data$Residency_Status!="AB 540",]
    }
  }
  
  if(nrow(data)==0){
    return(NULL)
  }
  else if (length(unique(data$Student_Id))<15){
    return (-1)
  }
  else{
    
    determine_placement()
    
    if(math_placement!="ALL"){
      
      data<<-data[data$Math_Placement==math_placement,]
      print(paste("Total Students math placement:",length(unique(data$Student_Id))))
    }
    if(english_placement!="ALL"){
      
      data<<-data[data$English_Placement==english_placement,]
      print(paste("Total Students english placement:",length(unique(data$Student_Id))))
    }
    if(goals!="ALL"){
      data<<-data[data$Education_Goal==goals,]
    }
    if(nrow(data)!=0){
      Raw_Data<<-data
      get_milestones(math_placement,english_placement)
    }else{
      return(NULL)
    }
  }
 
}

determine_placement<-function(){
  result=data %>% group_by(Student_Id) %>% slice(which.min(Year_Semester))
  print(paste("Total Students grouped:",length(unique(data$Student_Id))))
  #getting English placement
  temp=data[data$Discipline=='ENGLISH' | data$Discipline=='READING'| data$Discipline=='ESL'| data$Discipline=='E.S.L.',]
  
  if(nrow(temp[temp$Discipline=='ENGLISH' & temp$Course_Number=='019',])!=0)
    temp[temp$Discipline=='ENGLISH' & temp$Course_Number=='019',]$Levels_Below_Transfer<-4
  
  if(nrow(temp[temp$Discipline=='E.S.L' | temp$Discipline=='ESL',])!=0)
    temp[temp$Discipline=='E.S.L' | temp$Discipline=='ESL',]$Levels_Below_Transfer=(-1)
  temp=temp %>% group_by(Student_Id) %>% slice(which.min(Year_Semester))
  temp=temp %>% group_by(Student_Id) %>% slice(which.min(Levels_Below_Transfer))
  #temp=aggregate(Year_Semester ~ Student_Id, temp, min)
  #temp=aggregate(Levels_Below_Transfer ~ Student_Id, temp, min)
  
  if(nrow(temp[temp$Levels_Below_Transfer>="4",])!=0)
    temp[as.integer(temp$Levels_Below_Transfer)>=4,]$Levels_Below_Transfer="4 or more Levels Below Transfer"
  
  if(nrow(temp[temp$Levels_Below_Transfer==0,])!=0){
    temp[temp$Levels_Below_Transfer=="0",]$Levels_Below_Transfer="Transfer Level"
  }
  
  
  if(nrow(temp[temp$Levels_Below_Transfer=="1" | temp$Levels_Below_Transfer=="2" |temp$Levels_Below_Transfer=="3",])!=0)
    temp[temp$Levels_Below_Transfer=="1" | temp$Levels_Below_Transfer=="2" |temp$Levels_Below_Transfer=="3",]$Levels_Below_Transfer=paste(
      temp[temp$Levels_Below_Transfer=="1" | temp$Levels_Below_Transfer=="2" |temp$Levels_Below_Transfer=="3",]$Levels_Below_Transfer,"Levels Below Transfer"    
    )
  
  if(nrow(temp[temp$Levels_Below_Transfer=="-1",])!=0)
    temp[temp$Levels_Below_Transfer=="-1",]$Levels_Below_Transfer="ESL"
  
  colnames(temp)[7]="English_Placement"
  temp=temp[,c("Student_Id","English_Placement")]
  
  result=merge(x = result, y = temp, by = "Student_Id", all.x = TRUE)
  
  if(nrow(result[is.na(result$English_Placement),])!=0)
    result[is.na(result$English_Placement),]$English_Placement="No Placement Found"
  print(paste("New table total:",length(unique(result$Student_Id))))
  
  #get the math placement
  temp=data[data$Discipline=="MATH",]
  temp=temp %>% group_by(Student_Id) %>% slice(which.min(Year_Semester))
  temp=temp %>% group_by(Student_Id) %>% slice(which.min(Levels_Below_Transfer))
  
  
  if(nrow(temp[as.integer(temp$Levels_Below_Transfer)>=4,])!=0)
    temp[temp$Levels_Below_Transfer>=4,]$Levels_Below_Transfer="4 or more Levels Below Transfer"
  
  if(nrow(temp[temp$Levels_Below_Transfer=="0",])!=0)
    temp[temp$Levels_Below_Transfer=="0",]$Levels_Below_Transfer="Transfer Level"
  
  if(nrow(temp[temp$Levels_Below_Transfer=="1" | temp$Levels_Below_Transfer=="2" |temp$Levels_Below_Transfer=="3",])!=0)
    temp[temp$Levels_Below_Transfer=="1" | temp$Levels_Below_Transfer=="2" |temp$Levels_Below_Transfer=="3",]$Levels_Below_Transfer=paste(
      temp[temp$Levels_Below_Transfer=="1" | temp$Levels_Below_Transfer=="2" |temp$Levels_Below_Transfer=="3",]$Levels_Below_Transfer,"Levels Below Transfer"    
    )
  
  
  colnames(temp)[7]="Math_Placement"
  temp=temp[,c("Student_Id","Math_Placement")]
  
  
  result=merge(x = result, y = temp, by = "Student_Id", all.x = TRUE)
  
  if(nrow(result[is.na(result$Math_Placement),])!=0)
    result[is.na(result$Math_Placement),]$Math_Placement="No Placement Found"
  
  result=result[,c("Student_Id","English_Placement","Math_Placement")]
  
  data<<-merge(x = data, y = result, by = "Student_Id", all.x = TRUE)
  
  return(result)
}

get_milestones<-function(math_placement,english_placement){
  
  result=subset(data, !duplicated(Student_Id))
  result=result[,c("Student_Id","Received_Award")]
  
  
  cohort=data$Cohort[1]
  b_year=as.integer(substr(cohort,1,4))
  a_semester=substr(cohort,5,5)
  
  if(a_semester=="1"){#Spring
    a="Spring"
    b="Fall"
    b_semester=3
    a_year=b_year+1
  }
  else if(a_semester=='2'){#Summer
    a="Summer"
    b="Fall"
    c="Spring"
    b_semester=3
    a_year=b_year+1
    c_semester=1
    #am trackinf (Summer to Fall)(Summer to Spring)
    #a semester-> b semester ->c semester ->b semester
    #a_year is with c semester and 2nd b semester
  }
  else{#Fall
    a="Fall"
    b="Spring"
    b_semester=1
    b_year=b_year+1 
    a_year=b_year
  } 
  
  #persisted a to b
  persisted_a_to_a=paste0("Persisted_",a,"_to_",a,"_(",a_year,")")
  persisted_a_to_b=paste0("Persisted_",a,"_to_",b,"_(",b_year,")")
  temp=data.frame(Student_Id=data[data$Year_Semester==paste0(b_year,b_semester),]$Student_Id)
  temp=subset(temp, !duplicated(Student_Id))
  temp[[persisted_a_to_b]]=1
  result=merge(x = result, y = temp, by = "Student_Id", all.x = TRUE)
  if(nrow(result[is.na(result[[persisted_a_to_b]]),])!=0)
    result[is.na(result[[persisted_a_to_b]]),][[persisted_a_to_b]]<-0
  
  
  if(a=="Summer"){
    #order: persisted a to b->a to c -> a to a
    persisted_a_to_b<-paste0("Persisted Summer to Fall (",b_year,")")
    colnames(result)[3]<-persisted_a_to_b
    persisted_a_to_c=paste0("Persisted Summer to Spring (",a_year,")")
    persisted_a_to_a=paste0("Persisted Summer to Fall (",a_year,")")
    #a->c
    temp=data.frame(Student_Id=data[data$Year_Semester==paste0(a_year,c_semester),]$Student_Id)
    temp=subset(temp, !duplicated(Student_Id))
    temp[[persisted_a_to_c]]=1
    result=merge(x = result, y = temp, by = "Student_Id", all.x = TRUE)
    if(nrow(result[is.na(result[[persisted_a_to_c]]),])!=0)
      result[is.na(result[[persisted_a_to_c]]),][[persisted_a_to_c]]<-0
    
  }
  
  
  if(Years_Tracked!="1 Year"){
    if(a!="Summer"){
      #persisted a to a (1 year done)
      temp=data.frame(Student_Id=data[data$Year_Semester==paste0(a_year,a_semester),]$Student_Id)
      temp=subset(temp, !duplicated(Student_Id))
      temp[[persisted_a_to_a]]=1
      result=merge(x = result, y = temp, by = "Student_Id", all.x = TRUE)
      if(nrow(result[is.na(result[[persisted_a_to_a]]),])!=0)
        result[is.na(result[[persisted_a_to_a]]),][[persisted_a_to_a]]<-0
    }else{#<--------Summer
      temp=data.frame(Student_Id=data[data$Year_Semester==paste0(a_year,a_semester),]$Student_Id)
      temp=subset(temp, !duplicated(Student_Id))
      temp[[persisted_a_to_a]]=1
      result=merge(x = result, y = temp, by = "Student_Id", all.x = TRUE)
      if(nrow(result[is.na(result[[persisted_a_to_a]]),])!=0)
        result[is.na(result[[persisted_a_to_a]]),][[persisted_a_to_a]]<-0
    }
  }
  
  #completed transfer level English
  if(english_placement=="No Placement Found" || nrow(data[data$Discipline=="ENGLISH" & data$Levels_Below_Transfer==0 & 
                                                          (data$Grade=='A' | data$Grade=='B' | data$Grade=='C' | data$Grade=='P'),])==0){
    result$Completed_Transfer_Level_English=0
  }else{
    temp=data.frame(Student_Id=data[data$Discipline=="ENGLISH" & data$Levels_Below_Transfer==0 & (data$Grade=='A'
                                                                                                  | data$Grade=='B' | data$Grade=='C' | data$Grade=='P'),]$Student_Id ,Completed_Transfer_Level_English=c(1))
    temp=subset(temp, !duplicated(Student_Id))
    result=merge(x = result, y = temp, by = "Student_Id", all.x = TRUE)
    if(nrow(result[is.na(result$Completed_Transfer_Level_English),])!=0)
      result[is.na(result$Completed_Transfer_Level_English),]$Completed_Transfer_Level_English=0
  }
  
  #completed transfer level Math
  if(math_placement=="No Placement Found" || nrow(data[data$Discipline=="MATH" & data$Levels_Below_Transfer==0 &
                                                       (data$Grade=='A'| data$Grade=='B' | data$Grade=='C' | data$Grade=='P'),])==0){
    result$Completed_Transfer_Level_Math=0
  }else{
    temp=data.frame(Student_Id=data[data$Discipline=="MATH" & data$Levels_Below_Transfer==0 &
                                      (data$Grade=='A'| data$Grade=='B' | data$Grade=='C' | data$Grade=='P'),]$Student_Id,
                    Completed_Transfer_Level_Math=c(1))
    temp=subset(temp, !duplicated(Student_Id))
    result=merge(x = result, y = temp, by = "Student_Id", all.x = TRUE)
    if(nrow(result[is.na(result$Completed_Transfer_Level_Math),])!=0)
      result[is.na(result$Completed_Transfer_Level_Math),]$Completed_Transfer_Level_Math=0
  }
  
  
  #UC units completed
  temp=get_transferable_units("UC")
  temp$Completed_15_UC_Units=0
  temp$Completed_30_UC_Units=0
  temp$Completed_45_UC_Units=0
  temp$Completed_60_UC_Units=0
  if(nrow(temp[temp$UC_Units_Completed>=15,])>0)
    temp[temp$UC_Units_Completed>=15,]$Completed_15_UC_Units=1
  if(nrow(temp[temp$UC_Units_Completed>=30,])>0)
    temp[temp$UC_Units_Completed>=30,]$Completed_30_UC_Units=1
  if(nrow(temp[temp$UC_Units_Completed>=45,])>0)
    temp[temp$UC_Units_Completed>=45,]$Completed_45_UC_Units=1
  if(nrow(temp[temp$UC_Units_Completed>=60,])>0)
    temp[temp$UC_Units_Completed>=60,]$Completed_60_UC_Units=1
  temp$UC_Units_Completed=NULL
  result=merge(x = result, y = temp, by = "Student_Id", all.x = TRUE)
  if(nrow(result[is.na(result$Completed_15_UC_Units),])>0)
    result[is.na(result$Completed_15_UC_Units),]$Completed_15_UC_Units=0
  if(nrow(result[is.na(result$Completed_30_UC_Units),])>0)
    result[is.na(result$Completed_30_UC_Units),]$Completed_30_UC_Units=0
  if(nrow(result[is.na(result$Completed_45_UC_Units),])>0)
    result[is.na(result$Completed_45_UC_Units),]$Completed_45_UC_Units=0
  if(nrow(result[is.na(result$Completed_60_UC_Units),])>0)
    result[is.na(result$Completed_60_UC_Units),]$Completed_60_UC_Units=0
  
  #csu units completed
  temp=get_transferable_units("CSU")
  temp$Completed_15_CSU_Units=0
  temp$Completed_30_CSU_Units=0
  temp$Completed_45_CSU_Units=0
  temp$Completed_60_CSU_Units=0
  if(nrow(temp[temp$CSU_Units_Completed>=15,])>0)
    temp[temp$CSU_Units_Completed>=15,]$Completed_15_CSU_Units=1
  if(nrow(temp[temp$CSU_Units_Completed>=30,])>0)
    temp[temp$CSU_Units_Completed>=30,]$Completed_30_CSU_Units=1
  if(nrow(temp[temp$CSU_Units_Completed>=45,])>0)
    temp[temp$CSU_Units_Completed>=45,]$Completed_45_CSU_Units=1
  if(nrow(temp[temp$CSU_Units_Completed>=60,])>0)
    temp[temp$CSU_Units_Completed>=60,]$Completed_60_CSU_Units=1
  temp$CSU_Units_Completed=NULL
  result=merge(x = result, y = temp, by = "Student_Id", all.x = TRUE)
  
  if(nrow(result[is.na(result$Completed_15_CSU_Units),])!=0)
    result[is.na(result$Completed_15_CSU_Units),]$Completed_15_CSU_Units=0
  if(nrow(result[is.na(result$Completed_30_CSU_Units),])!=0)
    result[is.na(result$Completed_30_CSU_Units),]$Completed_30_CSU_Units=0
  if(nrow(result[is.na(result$Completed_45_CSU_Units),])!=0)
    result[is.na(result$Completed_45_CSU_Units),]$Completed_45_CSU_Units=0
  if(nrow(result[is.na(result$Completed_60_CSU_Units),])!=0)
    result[is.na(result$Completed_60_CSU_Units),]$Completed_60_CSU_Units=0
  
  
  ##-----------------FINAL TALLY---------------------------------------
  if(substr(Year_Semester,5,5)==3){
    cohort_name=paste0(substr(Year_Semester,1,4)," Fall Cohort")
  }else if(substr(Year_Semester,5,5)==2){
    cohort_name=paste0(substr(Year_Semester,1,4)," Summer Cohort")
  }else{
    cohort_name=paste0(substr(Year_Semester,1,4)," Spring Cohort")
  }
  
  if(Years_Tracked!="1 Year"){# there is more than one year
    if(a=="Summer"){
      catagories=c(cohort_name, str_replace_all(persisted_a_to_b,"_"," "),
                   str_replace_all(persisted_a_to_c,"_"," "),str_replace_all(persisted_a_to_a,"_"," "),
                   "Completed Transfer Level English",
                   "Completed Transfer Level Math",
                   "Completed 15 UC Units",
                   "Completed 15 CSU Units",
                   "Completed 30 UC Units",
                   "Completed 30 CSU Units",
                   "Completed 45 UC Units",
                   "Completed 45 CSU Units",
                   "Completed 60 UC Units",
                   "Completed 60 CSU Units",
                   "Earned Any Award")
      final_tally=data.frame(Categories=catagories,
                             Total=c(nrow(result),
                                     nrow(result[result[[persisted_a_to_b]]==1,]),
                                     nrow(result[result[[persisted_a_to_c]]==1,]),
                                     nrow(result[result[[persisted_a_to_a]]==1,]),
                                     nrow(result[result$Completed_Transfer_Level_English==1,]),
                                     nrow(result[result$Completed_Transfer_Level_Math==1,]),
                                     nrow(result[result$Completed_15_UC_Units==1,]),#15 UC UNITS
                                     nrow(result[result$Completed_15_CSU_Units==1,]),#15 CSU UNITS
                                     nrow(result[result$Completed_30_UC_Units==1,]),#30 UC UNITS
                                     nrow(result[result$Completed_30_CSU_Units==1,]),#30 CSU UNITS
                                     nrow(result[result$Completed_45_UC_Units==1,]),#45 UC UNITS
                                     nrow(result[result$Completed_45_CSU_Units==1,]),#45 CSU UNITS
                                     nrow(result[result$Completed_60_UC_Units==1,]),#60 UC UNITS
                                     nrow(result[result$Completed_60_CSU_Units==1,]),#60 CSU UNITS
                                     nrow(result[result$Received_Award==1,])))
    }else{#not summer
      catagories=c(cohort_name, str_replace_all(persisted_a_to_b,"_"," "),str_replace_all(persisted_a_to_a,"_"," "),
                   "Completed Transfer Level English",
                   "Completed Transfer Level Math",
                   "Completed 15 UC Units",
                   "Completed 15 CSU Units",
                   "Completed 30 UC Units",
                   "Completed 30 CSU Units",
                   "Completed 45 UC Units",
                   "Completed 45 CSU Units",
                   "Completed 60 UC Units",
                   "Completed 60 CSU Units",
                   "Earned Any Award")
      final_tally=data.frame(Categories=catagories,
                             Total=c(nrow(result),
                                     nrow(result[result[[persisted_a_to_b]]==1,]),
                                     nrow(result[result[[persisted_a_to_a]]==1,]),
                                     nrow(result[result$Completed_Transfer_Level_English==1,]),
                                     nrow(result[result$Completed_Transfer_Level_Math==1,]),
                                     nrow(result[result$Completed_15_UC_Units==1,]),#15 UC UNITS
                                     nrow(result[result$Completed_15_CSU_Units==1,]),#15 CSU UNITS
                                     nrow(result[result$Completed_30_UC_Units==1,]),#30 UC UNITS
                                     nrow(result[result$Completed_30_CSU_Units==1,]),#30 CSU UNITS
                                     nrow(result[result$Completed_45_UC_Units==1,]),#45 UC UNITS
                                     nrow(result[result$Completed_45_CSU_Units==1,]),#45 CSU UNITS
                                     nrow(result[result$Completed_60_UC_Units==1,]),#60 UC UNITS
                                     nrow(result[result$Completed_60_CSU_Units==1,]),#60 CSU UNITS
                                     nrow(result[result$Received_Award==1,])))
    }
    
  }else{#only one year
    if(a!="Summer"){
      catagories=c(cohort_name, str_replace_all(persisted_a_to_b,"_"," "),
                   "Completed Transfer Level English",
                   "Completed Transfer Level Math",
                   "Completed 15 UC Units",
                   "Completed 15 CSU Units",
                   "Completed 30 UC Units",
                   "Completed 30 CSU Units",
                   "Completed 45 UC Units",
                   "Completed 45 CSU Units",
                   "Completed 60 UC Units",
                   "Completed 60 CSU Units",
                   "Earned Any Award")
      final_tally=data.frame(Categories=catagories,
                             Total=c(nrow(result),
                                     nrow(result[result[[persisted_a_to_b]]==1,]),
                                     nrow(result[result$Completed_Transfer_Level_English==1,]),
                                     nrow(result[result$Completed_Transfer_Level_Math==1,]),
                                     nrow(result[result$Completed_15_UC_Units==1,]),#15 UC UNITS
                                     nrow(result[result$Completed_15_CSU_Units==1,]),#15 CSU UNITS
                                     nrow(result[result$Completed_30_UC_Units==1,]),#30 UC UNITS
                                     nrow(result[result$Completed_30_CSU_Units==1,]),#30 CSU UNITS
                                     nrow(result[result$Completed_45_UC_Units==1,]),#45 UC UNITS
                                     nrow(result[result$Completed_45_CSU_Units==1,]),#45 CSU UNITS
                                     nrow(result[result$Completed_60_UC_Units==1,]),#60 UC UNITS
                                     nrow(result[result$Completed_60_CSU_Units==1,]),#60 CSU UNITS
                                     nrow(result[result$Received_Award==1,])))
    }else{
      catagories=c(cohort_name, persisted_a_to_b,persisted_a_to_c,
                   "Completed Transfer Level English",
                   "Completed Transfer Level Math",
                   "Completed 15 UC Units",
                   "Completed 15 CSU Units",
                   "Completed 30 UC Units",
                   "Completed 30 CSU Units",
                   "Completed 45 UC Units",
                   "Completed 45 CSU Units",
                   "Completed 60 UC Units",
                   "Completed 60 CSU Units",
                   "Earned Any Award")
      final_tally=data.frame(Categories=catagories,
                             Total=c(nrow(result),
                                     nrow(result[result[[persisted_a_to_b]]==1,]),
                                     nrow(result[result[[persisted_a_to_c]]==1,]),
                                     nrow(result[result$Completed_Transfer_Level_English==1,]),
                                     nrow(result[result$Completed_Transfer_Level_Math==1,]),
                                     nrow(result[result$Completed_15_UC_Units==1,]),#15 UC UNITS
                                     nrow(result[result$Completed_15_CSU_Units==1,]),#15 CSU UNITS
                                     nrow(result[result$Completed_30_UC_Units==1,]),#30 UC UNITS
                                     nrow(result[result$Completed_30_CSU_Units==1,]),#30 CSU UNITS
                                     nrow(result[result$Completed_45_UC_Units==1,]),#45 UC UNITS
                                     nrow(result[result$Completed_45_CSU_Units==1,]),#45 CSU UNITS
                                     nrow(result[result$Completed_60_UC_Units==1,]),#60 UC UNITS
                                     nrow(result[result$Completed_60_CSU_Units==1,]),#60 CSU UNITS
                                     nrow(result[result$Received_Award==1,])))
    }
  }
  
  
  data<<-final_tally
  data$Categories <- factor(data$Categories, levels =catagories)
  return(result)
}

get_transferable_units<-function(college_type="UC"){
  if(college_type=="UC"){
    college_type="UC_Transferable"
    column_name="UC_Units_Completed"
  }
  else{
    college_type="CSU_Transferable"
    column_name="CSU_Units_Completed"
  }
  
  connection = odbcConnect("OIEA_SERVER",uid,pwd)
  
  query = paste0("
                 select cohorts.Student_Id,sum(Units) as ",column_name,"             
                 from cohorts
                 right join(	select *
                 from enrollments
                 where College='E' and Drop_Date IS NULL and Grade!='E') as enrolled
                 on enrolled.Student_Id= cohorts.Student_Id and enrolled.College=cohorts.Cohort_College
                 left join sections2 on sections2.College=cohorts.Cohort_College and sections2.Year_Semester=enrolled.Year_Semester
                 and sections2.Section_Number=enrolled.Section_Number
                 left join courses on courses.Curriculum_Id=sections2.Curriculum_Id and courses.Curriculum_Version=sections2.Curriculum_Version
                 left join terms on enrolled.College=terms.College and enrolled.Year_Semester=terms.Year_Semester
                 left join students on students.Student_Id=cohorts.Student_Id
                 LEFT join student_profiles on student_profiles.Student_Id=cohorts.Student_Id and student_profiles.Year_Semester=cohorts.cohort
                 full outer join (select Student_Id, count(distinct Student_Id) as Award
                 from awards
                 where College='E'
                 group by Student_Id) as awards_ on awards_.Student_Id=cohorts.Student_Id
                 where Cohort_College='E' and Cohort='",Year_Semester,"' and enrolled.Year_Semester>='",Year_Semester,"' 
                 and enrolled.Year_Semester<='",Year_Cap,"'  and (Grade='A' OR  Grade='B' OR  Grade='C' OR  Grade='P') and ",college_type,"=1
                 group by cohorts.Student_Id  ")
  result <- sqlQuery(connection, query, stringsAsFactors = FALSE)
  
  odbcClose(connection)
  return(result)
  
}

get_Year_Semester<-function(year, semester){
  if(semester=="Fall" || semester==3)
    return (paste0(substr(year,1,4),"3"))
  else if(semester=="Summer" || semester==2)
    return (paste0(substr(year,1,4),"2"))
  else#spring
    return (paste0(substr(year,6,9),"1"))
}

get_Year_Sem_cap<-function(year, semester, years_wanted){
  start_year=as.integer(substr(year,1,4))
  if(semester=="Fall")
    semester=2
  else if(semester=="Summer")
    semester=1
  else if(semester=="Spring"){
    semester=0
    start_year=as.integer(substr(year,6,9))
  }
  
  return(paste0(start_year+as.integer(substr(years_wanted,1,1)),semester))
  
}

get_possible_year_range<-function(years_wanted){
  if((as.integer(format(Sys.Date(), "%m"))>7  && as.integer(format(Sys.Date(), "%m"))<=12) ||  as.integer(format(Sys.Date(), "%m"))==1){#Fall
    min_years=as.integer( format(Sys.Date(), "%Y"))-as.integer(substr(years_wanted,1,1))
  }else{#cannot get any data for prior year if in Spring
    min_years=as.integer( format(Sys.Date(), "%Y"))-as.integer(substr(years_wanted,1,1))-1 
  }
  
  dates=paste0(min_years:MIN_YEAR_TO_SHOW,"-",(min_years+1):(MIN_YEAR_TO_SHOW+1))
  #SPRING data is available 7(July) onward for that academic year current_year-1 / current_year
  #Fall data available Feb onward for 
  
  return(dates)
  
}

get_noti2=function(){
  notification <- notificationItem(icon = icon("info-circle"), status = "info", paste0("Tutorial"))
  notification$children[[1]] <- a(href="#shiny-tab-main","onclick"=paste0("clickFunction('",
                                                                          paste0(substr(as.character(runif(1, 0, 1)),1,6),
                                                                                 "Tutorial"),"'); return false;"),
                                  list(notification$children[[1]]$children))
  return(notification)
}

get_fall_to_fall<-function(){
  
}