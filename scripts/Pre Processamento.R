
treinamento <- read.csv("./data/trainingdt.csv", na.strings=c("","NA"))
teste <- read.csv("./data/test.csv", na.strings=c("","NA"))

#inseri as classes que precisamos predizer 
classe <- read.csv("./data/training labels.csv")
treinamento <- cbind(treinamento, classe)


#replace blank values to "NA"

names(treinamento)
sapply(treinamento, class)
nrow(treinamento)

names(sum(is.na(treinamento)))

#Number of NA in Dataframe
sapply(treinamento, function(x) sum(is.na(x)))

summary(treinamento)
treinamento$scheme_name

#Apparently, scheme_name is not helping us, so it's gone

treinamento$scheme_name <- NULL

#We don't need IDs

treinamento$id <- NULL

#Recorded By has the same values for all obs.

treinamento$recorded_by <-NULL

#----------------------------
#Variáveis que um dos caras que o cosas mandou elimina (colocar em ordem pra diferenciar do script do cara?)
#arrumei o nome das variáveis do cara pra ficar igual as nossas

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

treinamento$extraction_type = NULL
teste$extraction_type = NULL

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

#----------------------------

#Transforming to factor
treinamento$payment <- as.factor( treinamento$payment )
treinamento$ward <- as.factor( treinamento$ward )


#Plotting to find outliers
plot(treinamento$amount_tsh ~ treinamento$quality_group, pch=19, main="Relação entre ")
plot(treinamento$amount_tsh ~ treinamento$waterpoint_type , pch=19, main="Relação entre ")

#Normalize all numeric columns

treinamento$amount_tsh <- scale(treinamento$amount_tsh)

treinamento <- treinamento[!(treinamento$amount_tsh>0.1),]
summary(treinamento$amount_tsh)
