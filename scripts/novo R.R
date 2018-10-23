treinamento <- read.csv("./data/trainingdt.csv", na.strings=c("","NA"))
teste <- read.csv("./data/test.csv", na.strings=c("","NA"))
#inseri as classes que precisamos predizer 
classe <- read.csv("./data/training labels.csv")

treinamento <- merge(treinamento, classe, by="id")

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



treinamento$funder <- NULL
teste$funder <- NULL
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

permit.test.id <- permit.test$id

permit.test <- cbind(permit.test$id, permit.test$permit)


treinamento <- merge(permit.treinamento, permit.test, by="id")



#Criando modelo pra prever o status_group

treinamento$id <- NULL
teste$id <- NULL

row.has.na <- apply(treinamento, 1, function(x){any(is.na(x))})
treinamento <- treinamento[!row.has.na,]

names(treinamento)

formula <- status_group ~ date_recorded + water_quality + basin + payment + management + extraction_type_class + quantity + region + amount_tsh + gps_height + permit + population

modelo <- randomForest(formula, ntree = 250, data =  treinamento, importance=TRUE)

predict <- predict(modelo, newdata=teste)

teste$predicted <- predict


teste$predicted <- as.factor(
  ifelse(is.na(teste$predicted),
         "functional",
         paste(teste$predicted))
)


submiting <- teste[, c('predicted')]

submitingID <- read.csv("./data/SubmissionFormat.csv")
submitingID$status_group <- submiting
submiting <- submitingID

names(submiting) <- c('ID','status_group')
write.csv(submiting, "results_1.csv",row.names = FALSE)

