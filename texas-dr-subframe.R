##texas subset###
index <- grep("TX", Death.row, ignore.case=FALSE)
tx.sub <- Death.row[index]

####getting the texas frame done ####
tx.date = NULL
tx.fname = NULL
tx.mname = NULL
tx.lname = NULL
tx.state = NULL
tx.dvrace = NULL
tx.gender = NULL

for (i in 1:length(tx.sub)){
  A <- unlist(str_split(tx.sub[i], " "))
  A<- A[A !=""]
    if (length(A) == 6){
      tx.date[i] = A[1]
      tx.fname[i] = A[2]
      tx.lname[i] = A[3]
      tx.state[i] = A[4]
      tx.dvrace[i] = A[5]
      tx.gender[i] = A[6]}else
        if (length(A)==7){
          tx.date[i] = A[1]
          tx.fname[i] = A[2]
          tx.mname[i] = A[3]
          tx.lname[i] = A[4]
          tx.state[i] = A[5]
          tx.dvrace[i] = A[6]
          tx.gender[i] = A[7] 
        }
}
texas.sub <- cbind(tx.date, tx.fname, tx.lname, tx.state, tx.dvrace, tx.gender)
texas.sub <- as.data.frame(texas.sub)
colnames(texas.sub) <- c("ex.date", "fname", "lname","state", "dvrace", "vgender")
texas.sub$lname <- gsub("Jr.", " Jr.", texas.sub$lname)
texas.sub$lname <- gsub("Sr.", " Sr.", texas.sub$lname)
texas.sub$lname <- gsub("III", " III", texas.sub$lname)
texas.sub$lname <- gsub("IV", " IV", texas.sub$lname)
texas.sub$fullname <- paste(as.character(texas.sub$fname), texas.sub$lname)

colnames(Texas) <- c("ex.no", "offender_info","last_statement", "lname", "fname", "tdcj_no",
                     "age", "ex.date", "drace", "county", "statement_text", "statement_text2",
                     "drace2")
Texas$lname <- as.character(Texas$lname)
Texas$fullname <- paste(as.character(Texas$fname), Texas$lname)

#merge on full name
Texas.clean <- merge(texas.sub, Texas, by="fullname")


#sentiment analysis set up 
Texas.clean$statement_text2 <- as.character(Texas.clean$statement_text)
Texas.clean$statement_text2 <- gsub("[[:punct:]]|â???T", " ", Texas.clean$statement_text2)
Texas.clean$statement_text2 <-tolower(Texas.clean$statement_text2)

require(stringr)
#individual function!
Texas.clean$neg.score = NULL
Texas.clean$pos.score = NULL
Texas.clean$relig.score = NULL

#sentiment measures
for(i in 1:length(Texas.clean$statement_text2)){
  statement<- unlist(str_split(Texas.clean$statement_text2[i], "\\s+"))
  neg <- match(statement, huliu.neg)
  neg=!is.na(neg)
  Texas.clean$neg.score[i] = sum(neg)
  pos <- match(statement, huliu.pos)
  pos=!is.na(pos)
  Texas.clean$pos.score[i] <- sum(pos)
  relig <- match(statement, jp.relig)
  relig = !is.na(relig)
  Texas.clean$relig.score[i] = sum(relig)
}

for(i in 1:length(Texas.clean$statement_text2)){
  statement<- unlist(str_split(Texas.clean$statement_text2[i], "\\s+"))
  hostile <- match(statement, huliu.hostile)
  hostile = !is.na(hostile)
  Texas.clean$host.score[i] = sum(hostile)
}

for(i in 1:length(Texas.clean$statement_text2)){
  statement<- unlist(str_split(Texas.clean$statement_text2[i], "\\s+"))
  pain <- match(statement, huliu.pain)
  pain = !is.na(pain)
  Texas.clean$pain.score[i] = sum(pain)
}


for(i in 1:length(Texas.clean$statement_text2)){
  statement<- unlist(str_split(Texas.clean$statement_text2[i], "\\s+"))
  power <- match(statement, huliu.power)
  power = !is.na(power)
  Texas.clean$power.score[i] = sum(power)
  strong <- match(statement, huliu.strong)
  strong = !is.na(strong)
  Texas.clean$strong.score[i] = sum(strong)
  finish <- match(statement, huliu.finish)
  finish = !is.na(finish)
  Texas.clean$finish.score[i] = sum(finish)
}


#recoding for analysis 
Texas.clean$drace.code = c(99)
#Assigning numbers to numeric groups 
Texas.clean$drace.code[Texas.clean$drace2=="White"]=1 
Texas.clean$drace.code[Texas.clean$drace2=="Black"]=2 
Texas.clean$drace.code[Texas.clean$drace2=="Hispanic"]=3 
Texas.clean$drace.code[Texas.clean$drace2=="Other"]=4 

#clean dvrace (defender victim race combo)
Texas.clean$vrace <- as.character(Texas.clean$dvrace)
Texas.clean$vrace <- gsub("[[:alpha:]]{1}\\/", "", Texas.clean$dvrace)
Texas.clean$vrace <- gsub("^[[:digit:]]", "", Texas.clean$vrace)
Texas.clean$vrace <- gsub("\\/", "", Texas.clean$vrace)
##do not have a regex for this but for vrace, i removed the second character manually
Texas.clean$vrace[Texas.clean$vrace =="W"] = 1
Texas.clean$vrace[Texas.clean$vrace =="B"] = 2
Texas.clean$vrace[Texas.clean$vrace =="L"] = 3
Texas.clean$vrace[Texas.clean$vrace=="A"]=4
Texas.clean$vrace <- as.numeric(Texas.clean$vrace)

#extract number of victims from dvrace 
Texas.clean$vnum <- as.character(Texas.clean$dvrace)
Texas.clean$vnum <- gsub("[A-Z]{1}\\/[A-Z]{1}","1", Texas.clean$vnum)
Texas.clean$vnum <- gsub("[^[:digit:]]","", Texas.clean$vnum)
Texas.clean$vnum <- as.numeric(Texas.clean$vnum)

#clean victim gender - when there are mutiple victims, gender is based on majority rule 
#when it is one male and female, female was selected. 
#did not use regular expressions, manual hand cleaning. ooops
Texas.clean$vgender <- as.character(Texas.clean$vgender)
Texas.clean$vgender[Texas.clean$vgender == "M"] = 1
Texas.clean$vgender[Texas.clean$vgender == "F"] = 2
Texas.clean$vgender <- as.numeric(Texas.clean$vgender)

#treat date as date
Texas.clean$ex.date <- as.character(Texas.clean$ex.date.x)
Texas.clean$ex.date <- as.Date(Texas.clean$ex.date, format="%m-%d-%y")
require(lubridate)
Texas.clean$ex.year <- year(Texas.clean$ex.date)
Texas.clean$ex.mon <- month(Texas.clean$ex.date)
Texas.clean$ex.day <- day(Texas.clean$ex.date)

#race different or not between victim and offender (0 is same race, 1 is not)
Texas.clean$dv.rdiff <- Texas.clean$drace.code - Texas.clean$vrace
Texas.clean$dv.rdiff[Texas.clean$dv.rdiff!=0] = 1 #1 is different races, 0 is same

#White offender minority victim, 1 white on minority, 2 is same race, 3 crimes between minorities, 4 minority on white  
Texas.clean$w.m <- c(9) 
Texas.clean$w.m[Texas.clean$drace.code==1 & Texas.clean$vrace !=1] =1 
Texas.clean$w.m[Texas.clean$drace.code!=1 & Texas.clean$vrace ==1] =4 
Texas.clean$w.m[Texas.clean$drace.code == Texas.clean$vrace] = 2
Texas.clean$w.m[Texas.clean$w.m==9] = 3

Texas.clean$w.m2 <- Texas.clean$w.m
Texas.clean$w.m2[Texas.clean$w.m==3] =2
Texas.clean$w.m2[Texas.clean$w.m==4] =3

#binary victims race
Texas.clean$vrace.bi <- Texas.clean$vrace
Texas.clean$vrace.bi[Texas.clean$vrace.bi==1] = 0
Texas.clean$vrace.bi[Texas.clean$vrace.bi!=0] = 1