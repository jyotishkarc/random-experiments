
setwd("/kaggle//input//tusdata")

data_2 = read.csv("TUS106_L02.txt",header = F) 
data_5 = read.csv("TUS106_L05.txt", header = F) 
cat("Data imported.....\n")
data_2_new = cbind(data.frame(paste(substr(data_2[,1],1,32),substr(data_2[,1],37,39),sep=""),substr(data_2[,1],41,41),as.numeric(substr(data_2[,1],130,139))/100,substr(data_2[,1],16,16)))
colnames(data_2_new) = c("id","gender","mult","sector")

cat("Data frame data_2_new created...\n")
data_5_new = cbind(data.frame(paste(substr(data_5[,1],1,32),substr(data_5[,1],37,39),sep=""),substr(data_5[,1],59,59)))
colnames(data_5_new) = c("id","job")

cat("Data frame data_5_new created...\n")
data_5_new = data_5_new[-dim(data_5_new)[1],]

data_new = merge(data_2_new,data_5_new,by="id")
cat("Data frame data_new created...\n")

uni_data = unique(data_new[,c("id","sector","gender","mult")])
cat("Unique data has been created to estimate..")
cat(dim(uni_data),"\n")
head(uni_data)

rural = uni_data[uni_data[,"sector"]=="1",c("mult","gender")]
urban = uni_data[uni_data[,"sector"]=="2",c("mult","gender")]
x_rural = sum(rural[,c("mult")])
x_urban = sum(urban[,c("mult")])
x_rural_male = sum(rural[rural[,"gender"]=="1",c("mult")])
x_rural_female = sum(rural[rural[,"gender"]=="2",c("mult")])
x_urban_male = sum(urban[urban[,"gender"]=="1",c("mult")])
x_urban_female = sum(urban[urban[,"gender"]=="2",c("mult")])

job_data = unique(data_new[,c("job","id","mult","sector","gender")])

est_rural = job_data[job_data[,"sector"]=="1",]
est_urban = job_data[job_data[,"sector"]=="2",]
est_rural_male = est_rural[est_rural[,"gender"]=="1",]
est_rural_female = est_rural[est_rural[,"gender"]=="2",]
est_urban_male = est_urban[est_urban[,"gender"]=="1",]
est_urban_female = est_urban[est_urban[,"gender"]=="2",]

for(j in c("1","2","3","4","5","6","7","8","9")){
    estimate_rural_male = (sum(est_rural_male[est_rural_male[,"job"]==j,"mult"])/x_rural_male)*100
    estimate_rural_female = (sum(est_rural_female[est_rural_female[,"job"]==j,"mult"])/x_rural_female)*100
    estimate_rural = (sum(est_rural[est_rural[,"job"]==j,"mult"])/x_rural)*100
    estimate_urban_male = (sum(est_urban_male[est_urban_male[,"job"]==j,"mult"])/x_urban_male)*100
    estimate_urban_female = (sum(est_urban_female[est_urban_female[,"job"]==j,"mult"])/x_urban_female)*100
    estimate_urban = (sum(est_urban[est_urban[,"job"]==j,"mult"])/x_urban)*100
    estimate_male = ((sum(est_urban_male[est_urban_male[,"job"]==j,"mult"])+sum(est_rural_male[est_rural_male[,"job"]==j,"mult"]))/(x_rural_male+x_urban_male))*100
    estimate_female = ((sum(est_urban_female[est_urban_female[,"job"]==j,"mult"])+sum(est_rural_female[est_rural_female[,"job"]==j,"mult"]))/(x_rural_female+x_urban_female))*100
    estimate = ((sum(est_urban[est_urban[,"job"]==j,"mult"])+sum(est_rural[est_rural[,"job"]==j,"mult"]))/(x_rural+x_urban))*100

    est_param = switch(as.numeric(j),
        "Employment and related activities",
        "Production of goods for own final use",
        "Unpaid domestic services for household members",
        "Unpaid caregiving services for household members",
        "Unpaid volunteer, trainee and other unpaid work",
        "Learning",
        "Socializing and communication, community participation and religious practice",
        "Culture, leisure, mass-media and sports practices",
        "Self-care and maintenance")
    print(est_param)
    cat("Rural.......\n")
    cat("Male:",estimate_rural_male,"\n")
    cat("Female:",estimate_rural_female,"\n")
    cat("Person:",estimate_rural,"\n")
    cat("Urban.......\n")
    cat("Male:",estimate_urban_male,"\n")
    cat("Female:",estimate_urban_female,"\n")
    cat("Person:",estimate_urban,"\n")
    cat("Rural+Urban......\n")
    cat("Male:",estimate_male,"\n")
    cat("Female:",estimate_female,"\n")
    cat("Person:",estimate,"\n")
}

head(data_5,n=25)

data_5_new = cbind(data.frame(paste(substr(data_5[,1],1,32),substr(data_5[,1],37,39),sep=""),substr(data_5[,1],59,59),as.numeric(substr(data_5[,1],46,47)),as.numeric(substr(data_5[,1],49,50)),as.numeric(substr(data_5[,1],51,52)),as.numeric(substr(data_5[,1],54,55)),substr(data_5[,1],56,56)))
colnames(data_5_new) = c("id","job","sh","sm","fh","fm","act")

data_5_new[(data_5_new[,"fh"]<data_5_new[,"sh"]),"fh"] = data_5_new[(data_5_new[,"fh"]<data_5_new[,"sh"]),"fh"]+24

print(dim(data_5_new))
head(data_5_new,50)

data_5_new$inter = (data_5_new[,"fh"]-data_5_new[,"sh"])*2+(data_5_new[,"fm"]-data_5_new[,"sm"])/30
head(data_5_new,50)

data_new = merge(data_2_new,data_5_new,by="id")
cat("Data frame data_new created...\n")
data_new[,"inter"] = data_new[,"inter"]*30
data_new[data_new$act == " ","act"] = "0"
data_new$act = as.numeric(data_new$act)
data_temp = data_new$act
i = 1
while(i <= length(data_temp)){
    if(data_temp[i] == 2){
        data_temp[i] = 1 
        i = i+1
    }else{
        n = 1
        j = i+1
        while(data_temp[j] == 0){
            n = n+1
            j = j+1
        }
        data_temp[i:(i+n-1)] = 1/n
        i = i + n
    }
}
data_new$inter = data_new$inter * data_temp

head(data_new,25)

est_rural = data_new[data_new[,"sector"]=="1",]
est_urban = data_new[data_new[,"sector"]=="2",]
est_rural_male = est_rural[est_rural[,"gender"]=="1",]
est_rural_female = est_rural[est_rural[,"gender"]=="2",]
est_urban_male = est_urban[est_urban[,"gender"]=="1",]
est_urban_female = est_urban[est_urban[,"gender"]=="2",]

x_est_rural = job_data[job_data[,"sector"]=="1",]
x_est_urban = job_data[job_data[,"sector"]=="2",]
x_est_rural_male = x_est_rural[x_est_rural[,"gender"]=="1",]
x_est_rural_female = x_est_rural[x_est_rural[,"gender"]=="2",]
x_est_urban_male = x_est_urban[x_est_urban[,"gender"]=="1",]
x_est_urban_female = x_est_urban[x_est_urban[,"gender"]=="2",]

for(j in c("1","2","3","4","5","6","7","8","9")){
    estimate_rural_male = sum(est_rural_male[est_rural_male[,"job"]==j,"mult"]*est_rural_male[est_rural_male[,"job"]==j,"inter"])/sum(x_est_rural_male[x_est_rural_male[,"job"]==j,"mult"])
    estimate_rural_female = (sum(est_rural_female[est_rural_female[,"job"]==j,"mult"]*est_rural_female[est_rural_female[,"job"]==j,"inter"]))/sum(x_est_rural_female[x_est_rural_female[,"job"]==j,"mult"])
    estimate_rural = (sum(est_rural[est_rural[,"job"]==j,"mult"]*est_rural[est_rural[,"job"]==j,"inter"]))/sum(x_est_rural[x_est_rural[,"job"]==j,"mult"])
    estimate_urban_male = (sum(est_urban_male[est_urban_male[,"job"]==j,"mult"]*est_urban_male[est_urban_male[,"job"]==j,"inter"]))/sum(x_est_urban_male[x_est_urban_male[,"job"]==j,"mult"])
    estimate_urban_female = (sum(est_urban_female[est_urban_female[,"job"]==j,"mult"]*est_urban_female[est_urban_female[,"job"]==j,"inter"]))/sum(x_est_urban_female[x_est_urban_female[,"job"]==j,"mult"])
    estimate_urban = (sum(est_urban[est_urban[,"job"]==j,"mult"]*est_urban[est_urban[,"job"]==j,"inter"]))/sum(x_est_urban[x_est_urban[,"job"]==j,"mult"])
    estimate_male = (sum(est_urban_male[est_urban_male[,"job"]==j,"mult"]*est_urban_male[est_urban_male[,"job"]==j,"inter"])+sum(est_rural_male[est_rural_male[,"job"]==j,"mult"]*est_rural_male[est_rural_male[,"job"]==j,"inter"]))/(sum(x_est_urban_male[x_est_urban_male[,"job"]==j,"mult"])+sum(x_est_rural_male[x_est_rural_male[,"job"]==j,"mult"]))
    estimate_female = (sum(est_urban_female[est_urban_female[,"job"]==j,"mult"]*est_urban_female[est_urban_female[,"job"]==j,"inter"])+sum(est_rural_female[est_rural_female[,"job"]==j,"mult"]*est_rural_female[est_rural_female[,"job"]==j,"inter"]))/(sum(x_est_urban_female[x_est_urban_female[,"job"]==j,"mult"])+sum(x_est_rural_female[x_est_rural_female[,"job"]==j,"mult"]))
    estimate = (sum(est_urban[est_urban[,"job"]==j,"mult"]*est_urban[est_urban[,"job"]==j,"inter"])+sum(est_rural[est_rural[,"job"]==j,"mult"]*est_rural[est_rural[,"job"]==j,"inter"]))/(sum(x_est_urban[x_est_urban[,"job"]==j,"mult"])+sum(x_est_rural[x_est_rural[,"job"]==j,"mult"]))

    est_param = switch(as.numeric(j),
        "Employment and related activities",
        "Production of goods for own final use",
        "Unpaid domestic services for household members",
        "Unpaid caregiving services for household members",
        "Unpaid volunteer, trainee and other unpaid work",
        "Learning",
        "Socializing and communication, community participation and religious practice",
        "Culture, leisure, mass-media and sports practices",
        "Self-care and maintenance")
    print(est_param)
    cat("Rural.......\n")
    cat("Male:",estimate_rural_male,"\n")
    cat("Female:",estimate_rural_female,"\n")
    cat("Person:",estimate_rural,"\n")
    cat("Urban.......\n")
    cat("Male:",estimate_urban_male,"\n")
    cat("Female:",estimate_urban_female,"\n")
    cat("Person:",estimate_urban,"\n")
    cat("Rural+Urban......\n")
    cat("Male:",estimate_male,"\n")
    cat("Female:",estimate_female,"\n")
    cat("Person:",estimate,"\n")
}

for(j in c("1","2","3","4","5","6","7","8","9")){
    estimate_rural_male = sum(est_rural_male[est_rural_male[,"job"]==j,"mult"]*est_rural_male[est_rural_male[,"job"]==j,"inter"])/x_rural_male *(100/1440)
    estimate_rural_female = (sum(est_rural_female[est_rural_female[,"job"]==j,"mult"]*est_rural_female[est_rural_female[,"job"]==j,"inter"]))/x_rural_female *(100/1440)
    estimate_rural = (sum(est_rural[est_rural[,"job"]==j,"mult"]*est_rural[est_rural[,"job"]==j,"inter"]))/x_rural*(100/1440)
    estimate_urban_male = (sum(est_urban_male[est_urban_male[,"job"]==j,"mult"]*est_urban_male[est_urban_male[,"job"]==j,"inter"]))/x_urban_male *(100/1440)
    estimate_urban_female = (sum(est_urban_female[est_urban_female[,"job"]==j,"mult"]*est_urban_female[est_urban_female[,"job"]==j,"inter"]))/x_urban_female *(100/1440)
    estimate_urban = (sum(est_urban[est_urban[,"job"]==j,"mult"]*est_urban[est_urban[,"job"]==j,"inter"]))/x_urban *(100/1440)
    estimate_male = (sum(est_urban_male[est_urban_male[,"job"]==j,"mult"]*est_urban_male[est_urban_male[,"job"]==j,"inter"])+sum(est_rural_male[est_rural_male[,"job"]==j,"mult"]*est_rural_male[est_rural_male[,"job"]==j,"inter"]))/(x_rural_male+x_urban_male)*(100/1440)
    estimate_female = (sum(est_urban_female[est_urban_female[,"job"]==j,"mult"]*est_urban_female[est_urban_female[,"job"]==j,"inter"])+sum(est_rural_female[est_rural_female[,"job"]==j,"mult"]*est_rural_female[est_rural_female[,"job"]==j,"inter"]))/(x_rural_female+x_urban_female)*(100/1440)
    estimate = (sum(est_urban[est_urban[,"job"]==j,"mult"]*est_urban[est_urban[,"job"]==j,"inter"])+sum(est_rural[est_rural[,"job"]==j,"mult"]*est_rural[est_rural[,"job"]==j,"inter"]))/(x_rural+x_urban)*(100/1440)

    est_param = switch(as.numeric(j),
        "Employment and related activities",
        "Production of goods for own final use",
        "Unpaid domestic services for household members",
        "Unpaid caregiving services for household members",
        "Unpaid volunteer, trainee and other unpaid work",
        "Learning",
        "Socializing and communication, community participation and religious practice",
        "Culture, leisure, mass-media and sports practices",
        "Self-care and maintenance")
    print(est_param)
    cat("Rural.......\n")
    cat("Male:",estimate_rural_male,"\n")
    cat("Female:",estimate_rural_female,"\n")
    cat("Person:",estimate_rural,"\n")
    cat("Urban.......\n")
    cat("Male:",estimate_urban_male,"\n")
    cat("Female:",estimate_urban_female,"\n")
    cat("Person:",estimate_urban,"\n")
    cat("Rural+Urban......\n")
    cat("Male:",estimate_male,"\n")
    cat("Female:",estimate_female,"\n")
    cat("Person:",estimate,"\n")
}

data_2 = read.csv("TUS106_L02.txt",header =  F) 
data_5 = read.csv("TUS106_L05.txt", header = F) 
cat("Data imported.....\n")
data_2_new = cbind(data.frame(paste(substr(data_2[,1],1,32),substr(data_2[,1],37,39),sep=""),substr(data_2[,1],41,41),as.numeric(substr(data_2[,1],130,139))/100,substr(data_2[,1],16,16)))
colnames(data_2_new) = c("id","gender","mult","sector")

cat("Data frame data_2_new created...\n")
data_5_new = cbind(data.frame(paste(substr(data_5[,1],1,32),substr(data_5[,1],37,39),sep=""),substr(data_5[,1],59,59),as.numeric(substr(data_5[,1],46,47)),as.numeric(substr(data_5[,1],49,50)),as.numeric(substr(data_5[,1],51,52)),as.numeric(substr(data_5[,1],54,55)),substr(data_5[,1],63,64),substr(data_5[,1],56,56)))
colnames(data_5_new) = c("id","job","sh","sm","fh","fm","unpaid","act")
data_5_new = data_5_new[-dim(data_5_new)[1],]
data_5_new[(data_5_new[,"fh"]<data_5_new[,"sh"]),"fh"] = data_5_new[(data_5_new[,"fh"]<data_5_new[,"sh"]),"fh"]+24
data_5_new$inter = (data_5_new[,"fh"]-data_5_new[,"sh"])*2+(data_5_new[,"fm"]-data_5_new[,"sm"])/30
data_5_new[,"inter"] = data_5_new[,"inter"]*30
data_5_new = data_5_new[,c("id","inter","unpaid","act")]
cat("Data frame data_5_new created...\n")



data_new = merge(data_2_new,data_5_new,by="id")
cat("Data frame data_new created...\n")
head(data_new)

data_new[data_new$act == " ","act"] = "0"
data_new$act = as.numeric(data_new$act)
data_temp = data_new$act
i = 1
while(i <= length(data_temp)){

    if(data_temp[i] == 2){
        data_temp[i] = 1 
        i = i+1
    }else{
        n = 1
        j = i+1
        while(data_temp[j] == 0){
            n = n+1
            j = j+1
        }
        data_temp[i:(i+n-1)] = 1/n
        i = i + n
    }
}
data_new$inter = data_new$inter * data_temp

data_new_state = data_new[substr(data_new$id,17,19)=="321",]
data_new_state = rbind(data_new_state,data_new[substr(data_new$id,17,19)=="322",])
head(data_new_state,25)

est_rural = data_new_state[data_new_state[,"sector"]=="1",]
est_rural_female = est_rural[est_rural[,"gender"]=="2",]
job_data = unique(est_rural[,c("id","gender")])
x_rural_female = dim(job_data[job_data[,"gender"]=="2",])[1]

for(j in c("01","02","03","04","05","06","07","08","09","10","11","12")){
    estimate_rural_female = (sum(est_rural_female[est_rural_female[,"unpaid"]==j,"mult"]*est_rural_female[est_rural_female[,"unpaid"]==j,"inter"]))/x_rural_female
    

    est_param = switch(as.numeric(j),
        "self development/ self care/ self maintenance",
        "care for children, sick, elderly, differently- abled persons in own households ",
        "production of other services (except care activities as covered in code 02) for own consumption",
        "production of goods for own consumption",
        "voluntary work for production of goods in households",
        "voluntary work for production of services in households",
        "voluntary work for production of goods in market/non-market units",
        "voluntary work for production of services in market/non-market units",
        "unpaid trainee work for production of goods",
        "unpaid trainee work for production of services",
        "other unpaid work for production of goods",
        "other unpaid work for production of services")
    print(est_param)
    cat("Female Rural.......\n")
    cat("Average Time spent per person",estimate_rural_female,"\n")
}

x_rural_female

