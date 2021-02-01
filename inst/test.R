library(datacat)
df <- frog
data.input <- df
data.input <- makeBinaryClass(data.input)
# greedy FFTs
fft_model_greedy <- fftree(df, max_depth = 6)
model <- fft_model_greedy
plot(fft_model_greedy)
plotFFT(fft_model_greedy, showBox = F)
costs <- c



# cross-entropy FFT

system.time(fft_model_ce <- fftree(df, method = "cross-entropy", cross_entropy_parameters = cross_entropy_control()))
plot(fft_model_ce)

predict(fft_model_greedy, df, "metric")
predict(fft_model_ce, df, "metric")

# tallying
set.seed(1)
system.time(tally_model_regression <- tally(df, method = "regression", cv = T))
tally_model_regression@tally$

tally_model_regression@tally$intercept

set.seed(100)
df <- data.frame(replicate(10, runif(100)))
df <- round(df > .6) *1
df <- data.frame(df)
#df <- round(df)

tally_model_ce <- tally(df, method = "cross-entropy", nthreads = 1, cross_entropy_parameters = cross_entropy_control(starts = 1, threads = 1))

tally_model_ce@tally$intercept


system.time(mod <- tally(df, method = "regression", nthreads = 1, cross_entropy_parameters = cross_entropy_control(starts = 1, threads = 1)))
for (i in -6:6){
  pp <- mod
  pp@tally$intercept = i
  print(paste(i, predict(pp, df, "metric")[2]))
}


model <- tally(liver)
model
fftree(liver)

predict(tally_model_regression, df, "metric")
predict(tally_model_simple, df, "metric")
predict(tally_model_ce, df, "metric")


tally_cross_entropy(df, ncues = 6)






model <- tally_cross_entropy(data.input)
predicting(model, data.input)


tally_model_simple <- tally(df)


ncues = 6
N = 50
nthresh = 50
nBest = 5
iter = 25
costs = c(.5,.5)
alpha = 0.1
timeMax = 3600
quickAbort = 25
percentiles = F
verbose = F



# TODO: The following function should already oputput a tally model, not an optimalMofN model.
# WE should get rid of the optimalMofN model class altogether!

rmarkdown::render("vignette/vignette.Rmd", output_format = "latex_document")








p <- .5
model <- fftree(diagnosis ~ ., data = liver[1:300,], weights = c(1-p,p))
predict(model, newdata = liver[301:310, -1], type = "response")
predict(model, newdata = liver[301:310, -1], type = "probability")
predict(model, newdata = liver[301:nrow(liver),-1], type = "metric")

data(liver)
df <- liver
df[,1] <- ifelse(df[,1] == "yes", 1,0)

df[4,4] <- NA

model <- fftree( data = liver)


p <- mean(df[,1])
c(1-p,p)

m <- fftree(diagnosis ~ age + sex, data = df, weights = c(1-p,p))


m1 <- tally(diagnosis ~ age + sex, data = df, weights = c(1-p,p))

p <- .5

m1 <- tally(df[, c(1,3,4,5,6,7)], weights = c(1-p,p))


params = list(b = 4, c = 4)
bb <- c(list(data.input), c(100, 100), params)
length(bb)

do.call(lala, c(a = 2, p =5,  params))


lala <- function(a= 1, b = 2, c = 0){

  return(a+b+c)

}





data(liver)

summary(data.frame(liver, stringsAsFactors = T))

liver$diagnosis <- as.character(ifelse(liver$diagnosis == "Liver disease", 1,0))
liver$sex <- as.factor(liver$sex)

system.time(model <- fftree(liver, max_depth = 6, method = "greedy"))
system.time(model <- fftree(liver, max_depth = 10, method = "basic"))

model <- fftree(liver, max_depth = 10, method = "basic")



#### plot tally model as table ####
# https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html


library(gridExtra)
library(grid)
d <- head(iris[,1:3])
grid.table(d)



p <- sum(liver$diagnosis == "yes")/nrow(liver)
model <- tally(df, weights = c(1-p,p), maximum_size = 6)



library(datacat)




test_fun(x = 23)

test_fun <- function(x, ...){
  print(is.null(...))
  return(x)

}




p <- sum(liver$diagnosis == "Liver disease")/nrow(liver)
model <- tally(diagnosis ~ ., data = liver[1:300,], weights = c(1-p,p), maximum_size = 6)

predict(model, newdata = liver[301:nrow(liver),], type = "metric")


data(liver)
model <- fftree(liver, use_features_once = FALSE, method = "greedy", max_depth = 6)



