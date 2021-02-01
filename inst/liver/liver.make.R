setwd("inst/liver")
df <- read.table("liver.data.txt", sep=",", na.strings = "?")
colnames(df) <- c(
"age",
"sex",
"totalBilirubin",
"directBilirubin",
"alkaline",
"alamine",
"aspartate",
"proteins",
"albumin",
"albuminGlobulin",
"diagnosis")

df$diagnosis <- ifelse(df$diagnosis == 1, 2, 0)
df$diagnosis <- factor(df$diagnosis, labels = c("No liver disease", "Liver disease"))

ordr <- c(11,1:10)
df <- df[ordr]

# name the dataframe & clean up
assign("liver", df)
rm(df, ordr)
liver <- liver[complete.cases(liver), ]

save(liver, file = "liver.RData")
