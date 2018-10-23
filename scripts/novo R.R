treinamento <- read.csv("./data/trainingdt.csv", na.strings=c("","NA"))
teste <- read.csv("./data/test.csv", na.strings=c("","NA"))
#inseri as classes que precisamos predizer 
classe <- read.csv("./data/training labels.csv")

treinamento <- cbind(treinamento, classe)

treinamento$recorded_by = NULL
teste$recorded_by = NULL

treinamento$quantity_group = NULL
teste$quantity_group = NULL

treinamento$extraction_type_group = NULL
teste$extraction_type_group = NULL

treinamento$source_type = NULL
teste$source_type = NULL

treinamento$payment_type = NULL
teste$payment_type = NULL

#treinamento$water_quality = NULL
treinamento$quality_group = NULL
teste$quality_group = NULL

treinamento$waterpoint_type_group = NULL
teste$waterpoint_type_group = NULL

treinamento$scheme_name = NULL
teste$scheme_name = NULL

treinamento$num_private = NULL
teste$num_private = NULL

#treinamento$public_meeting = NULL
treinamento$wpt_name = NULL
teste$wpt_name = NULL

treinamento$lga = NULL
teste$lga = NULL

treinamento$region_code = NULL
teste$region_code = NULL

treinamento$management_group = NULL
teste$management_group = NULL

#treinamento$source_class = NULL
treinamento$scheme_management = NULL
teste$scheme_management = NULL

treinamento$subvillage = NULL
teste$subvillage =  NULL

treinamento$district_code = NULL
teste$district_code = NULL

treinamento$installer = NULL
teste$installer = NULL

treinamento$ward = NULL
teste$ward = NULL

treinamento$id <- NULL
teste$id <- NULL


# FUNDER

trim<-as.data.frame(treinamento$funder)
trimTest <- as.data.frame(teste$funder)

trim<-as.data.frame(apply(trim,2,function(x)gsub('\s+', '',x)))
trimTest <- as.data.frame(apply(trimTest,2,function(x)gsub('\s+','',x)))

trim[trim %in% c("0", "", "-"," ","")] <- "other"
trimTest[trimTest %in% c("0", "_", "-"," ","")] <- "other"

treinamento$newfunder<-trim[,1]
teste$newfunder <- trimTest[,1]

treinamento$newfunder <- substr(tolower(treinamento$funder),1,3)
teste$newfunder <- substr(tolower(teste$funder),1,3)

treinamento$funder<-treinamento$newfunder
teste$funder <- teste$newfunder

treinamento$newfunder<-NULL
teste$newfunder <- NULL

treinamento$funder[treinamento$funder %in% c("0", "", "-"," ","")] <- "other"
teste$funder[teste$funder %in% c("0", "", "-"," ","")] <- "other"

treinamento$funder <- as.factor(treinamento$funder)
teste$funder <- as.factor(teste$funder)

limitLevels = 10

funders_levels <- names(summary(treinamento$funder)[1:limitLevels])
funder <- factor(treinamento$funder, levels=c(funders_levels, "Other"))
funder[is.na(funder)] <- "Other"
treinamento$funder <- funder

funder <- factor(teste$funder, levels=c(funders_levels, "Other"))
funder[is.na(funder)] <- "Other"
teste$funder <- funder

teste$funder <- NULL
treinamento$funder <- NULL

# numeric 
library(stringr)
treinamento$date_recorded <- str_sub(treinamento$date_recorded, -12, -7)
treinamento$date_recorded <- as.numeric(treinamento$date_recorded)

teste$date_recorded <- str_sub(teste$date_recorded, -12, -7)
teste$date_recorded <- as.numeric(teste$date_recorded)


#normalize

treinamento$amount_tsh <- scale(treinamento$amount_tsh)

treinamento <- treinamento[!(treinamento$amount_tsh>0.1),]

treinamento$date_recorded <- scale(treinamento$date_recorded)

teste$amount_tsh <- scale(teste$amount_tsh)

teste <- teste[!(teste$amount_tsh>0.1),]

teste$date_recorded <- scale(teste$date_recorded)

treinamento$amount_tsh <- as.numeric(treinamento$amount_tsh)
teste$amount_tsh <- as.numeric(teste$amount_tsh)
treinamento$date_recorded <- as.numeric(treinamento$date_recorded)
teste$date_recorded <- as.numeric(teste$date_recorded)

# predict
library(randomForest)

#Prevendo se permit e public_meeting são true or false
sapply(treinamento, function(x) sum(is.na(x)))
row.has.na <- apply(treinamento, 1, function(x){any(is.na(x))})

permit.treinamento <- treinamento[!row.has.na,]

permit.test <- treinamento[row.has.na,]
permit.test$permit <-NULL

model.permit <- randomForest(permit ~., data=permit.treinamento)
predictteste <- predict(model.permit, newdata=permit.test)
permit.test$permit <- predictteste

treinamento <- rbind(permit.treinamento, permit.test)


#Sorting factor vars
sort(table(treinamento$waterpoint_type), decreasing = TRUE)
sort(table(teste$waterpoint_type), decreasing = TRUE)


#Criando modelo pra prever o status_group

row.has.na <- apply(treinamento, 1, function(x){any(is.na(x))})
treinamento <- treinamento[!row.has.na,]

model <- randomForest(status_group ~., data=treinamento)

predteste <- predict(model, newdata=teste)

table <- table(predicttest, teste$status_group)
confusionMatrix(tT)
