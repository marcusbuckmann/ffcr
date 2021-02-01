## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(ffcr)
data(liver)

## -----------------------------------------------------------------------------
model <- fftree(liver, use_features_once = FALSE, method = "greedy", max_depth = 6)

## ---- eval=FALSE--------------------------------------------------------------
#  fftree(diagnosis ~ sex + age  + albumin + proteins + aspartate  , data = liver)

## -----------------------------------------------------------------------------
print(model)

## ---- fig1, fig.height = 6, fig.width=4, fig.align= "center"------------------
plot(model)

## -----------------------------------------------------------------------------
p <- sum(liver$diagnosis == "Liver disease")/nrow(liver)
model <- fftree(liver, weights = c(1-p,p), cv = TRUE)
model

## -----------------------------------------------------------------------------
model <- fftree(diagnosis ~ ., data = liver[1:300,], weights = c(1-p,p))

predict(model, newdata = liver[301:310,], type = "response")

predict(model, newdata = liver[301:310,], type = "probability")

predict(model, newdata = liver[301:nrow(liver),], type = "metric")


## -----------------------------------------------------------------------------
p <- sum(liver$diagnosis == "Liver disease")/nrow(liver)
model <- tally(diagnosis ~ ., data = liver[1:300,], weights = c(1-p,p), max_size = 6)

predict(model, newdata = liver[301:nrow(liver),], type = "metric")


