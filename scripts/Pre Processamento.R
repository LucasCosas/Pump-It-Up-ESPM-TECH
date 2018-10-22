df <- read.csv("C:/Users/11510206/OneDrive/Semestres/Oitavo Semestre/Machine Learning and Data Analytics/Projeto/trainingdt.csv", na.strings=c("","NA"))

#replace blank values to "NA"

names(df)
sapply(df, class)
nrow(df)

is.na(df)

#Number of NA in Dataframe
sapply(df, function(x) sum(is.na(x)))

summary(df)
df$scheme_name

#Apparently, scheme_name is not helping us, so it's gone

df$scheme_name <- NULL

#We don't need IDs

df$id <- NULL

#Recorded By has the same values for all obs.

df$recorded_by <-NULL

#Transforming to factor
df$payment <- as.factor( df$payment )
train.df$ward <- as.factor( train.df$ward )


#Plotting to find outliers
plot(df$amount_tsh ~ df$quality_group, pch=19, main="Relação entre ")
plot(df$amount_tsh ~ df$waterpoint_type , pch=19, main="Relação entre ")



