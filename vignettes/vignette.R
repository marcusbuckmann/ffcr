## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(ffcr)
model <- fftree(liver, method = "greedy", max_depth = 4)

## ---- eval=FALSE--------------------------------------------------------------
#  fftree(diagnosis ~ age + albumin + proteins + aspartate, data = liver, max_depth = 4)

## -----------------------------------------------------------------------------
print(model)

## ---- fig1, fig.height = 5, fig.width=4, fig.align= "center"------------------
plot(model)

## -----------------------------------------------------------------------------
p <- sum(liver$diagnosis == "Liver disease")/nrow(liver)
weights <- c("No liver disease" = p, "Liver disease" = 1 - p)
model <- fftree(liver, weights = weights, cv = TRUE, max_depth = 4)
model

## -----------------------------------------------------------------------------
model <- fftree(diagnosis ~ ., data = liver[1:300,], weights = c(1-p,p), max_depth = 4)

predict(model, newdata = liver[301:310,], type = "response")
predict(model, newdata = liver[301:nrow(liver),], type = "metric")


## -----------------------------------------------------------------------------
p <- sum(liver$diagnosis == "Liver disease")/nrow(liver)
weights <- c("No liver disease" = p, "Liver disease" = 1 - p)
model <- tally(diagnosis ~ ., data = liver[1:300,], weights = weights, max_size = 4)
model
predict(model, newdata = liver[301:nrow(liver),], type = "metric")



