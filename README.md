
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

<!-- badges: end -->

## Fast-and-frugal classification in R (ffcr)

The *ffcr* package constructs two transparent classification models:
*fast-and-frugal trees* and *tallying* models. The book *Classification
in the Wild: The Science and Art of Transparent Decision Making.*
(Katiskopoulos et al., 2020) describes these models, their applications
and the algorithms to construct them in detail.

A fast-and-frugal tree is a decision tree with a simple structure: one
branch of each node exits tree, the other continues to the next node
until the final node is reached. A tallying model gives pieces of
evidence the same weight. This package implements several methods for
training these two models, ranging from simple heuristics to
computationally more complex cross-entropy optimization (Rubinstein,
1999).

## Installation

The package can be installed with the following command:

``` r
# install.packages("devtools")
devtools::install_github("marcusbuckmann/ffcr")
```

## Training fast-and-frugal trees

We start by training a fast-and-frugal tree on a medical data set,
predicting whether patients have liver disease (Ramana, Babu, and
Venkateswarlu, 2011; Dua and Graff, 2017). The data set is included in
the package.

``` r
library(ffcr)
model <- fftree(diagnosis~., data = liver, use_features_once = FALSE, method = "greedy", max_depth = 4)
```

The structure of the tree and its performance is shown by printing the
model.

``` r
print(model)
#> Fast-and-frugal Tree object
#> Trained with : "recursive" method. 
#> 
#> Call: 
#> fftree(data = data, formula = formula, method = "greedy", max_depth = 4, 
#>     use_features_once = FALSE)
#> 
#> Formula: 
#> diagnosis ~ age + sex + totalBilirubin + directBilirubin + alkaline + 
#>     alamine + aspartate + proteins + albumin + albuminGlobulin
#> 
#> Tree: 
#> Reason / Prediction / (Proportion of class 'Liver disease') / (Number of objects classified)
#> 
#>   totalBilirubin > 1.65: Liver disease (0.91) (213)
#>     alkaline > 211.5: Liver disease (0.76) (132)
#>       age <= 25.5: No liver disease (0.30) (27)
#>         age > 25.5: Liver disease (0.55) (207)
#> 
#> 
#> Fitted values:
#>     Observed         Predicted        N  
#>     Liver disease    Liver disease    406
#>     No liver disease Liver disease    146
#>     Liver disease    No liver disease   8
#>     No liver disease No liver disease  19
#> 
#> Fitting performance:                            
#>     Accuracy            0.73
#>     Sensitivity         0.98
#>     Specificity         0.12
#>     Balanced accuracy   0.55
#>     F1 score            0.84
#>                   
#>     Depth     3.00
#>     Features  3.00
#>     Frugality 2.04
```

To visualize the tree we use

``` r
plot(model)
```

<img src="man/figures/tree.png" width="50%" style="display: block; margin: auto;" />

To make predictions according to the fast-and-frugal tree, we can use
the `predict` function.

``` r
pred <- predict(model, newdata = liver[1:10,])
pred
#>  [1] "Liver disease"    "Liver disease"    "Liver disease"    "Liver disease"   
#>  [5] "Liver disease"    "Liver disease"    "Liver disease"    "Liver disease"   
#>  [9] "No liver disease" "Liver disease"
```

## Training tallying models

To train tallying models, we use the *tally* function. To make
predictions we use the same command as for the fast-and-frugal trees.

``` r
model <- tally(diagnosis~., data = liver, max_size = 4)
print(model)
#> Tallying object
#> Trained with : "basic" method. 
#> 
#> Call: 
#> tally(data = data, formula = formula, max_size = 4)
#> 
#> Formula: 
#> diagnosis ~ age + sex + totalBilirubin + directBilirubin + alkaline + 
#>     alamine + aspartate + proteins + albumin + albuminGlobulin
#> 
#> Reasons:                               
#>    + totalBilirubin   >    1.65
#>    + directBilirubin  >    0.85
#>    + aspartate        >   47.50
#>    + proteins         <=   3.65
#>    ____________________________________ 
#> Predict Liver disease if at least 1 reasons hold. 
#> 
#> 
#> Fitted values:
#>     Observed         Predicted        N  
#>     Liver disease    Liver disease    272
#>     No liver disease Liver disease     44
#>     Liver disease    No liver disease 142
#>     No liver disease No liver disease 121
#> 
#> Fitting performance:                            
#>     Accuracy            0.68
#>     Sensitivity         0.66
#>     Specificity         0.73
#>     Balanced accuracy   0.70
#>     F1 score            0.75
pred <- predict(model, newdata = liver[1:10,])
```

Please consult the vignette and the documentation of the package for
more details on how to use it. The book *Classification in the Wild*
provides background information on fast-and-frugal trees and tallying
models.

## References

Dua, Dheeru, and Casey Graff. 2017. “UCI Machine Learning Repository.”
University of California, Irvine, School of Information; Computer
Sciences. <http://archive.ics.uci.edu/ml>.

Katikopoulos, Konstantinos V., Özgür Şimşek, Marcus Buckmann, and Gerd
Gigerenzer. 2020. Classification in the Wild: The Science and Art of
Transparent Decision Making. MIT Press.

Ramana, Bendi Venkata, M Surendra Prasad Babu, and N. B. Venkateswarlu.
2011. “A Critical Study of Selected Classification Algorithms for Liver
Disease Diagnosis.” International Journal of Database Management Systems
3 (2): 101–114.

Rubinstein, Reuven. 1999. “The Cross-Entropy Method for Combinatorial
and Continuous Optimization.” Methodology and Computing in Applied
Probability 1 (2): 127–190.
