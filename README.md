
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

<!-- badges: end -->

The *ffcr* package allows the construction of two families of
transparent classification models: *fast-and-frugal trees* and
*tallying* models. The book *Classification in the Wild: The Science and
Art of Transparent Decision Making.*\[@katsikopoulos2021\] describes
these models, their applications and the algorithms to construct these
in detail.

A fast-and-frugal tree is a decision tree with a simple structure: one
branch of each node exits tree, the other continues to the next node
until the final node is reached. A tallying model gives pieces of
evidence the same weight. The package implemts several methods for
training these models, ranging from simple heuristics to computationyll
more complex cross-entropy optimization (Rubinstein, 1999).

There exist another R package,
[FFTrees](https://github.com/ndphillips/FFTrees) to train
fast-and-frugal trees.

## Installation

You can install ffcr using from this GitHub page with

``` r
# install.packages("devtools")
devtools::install_github("marcusbuckmann/ffcr")
```

or download the Windows Binary *fftr\_1.0.zip* and install it from your
hard drive.

## How to use the package

To illustrate the functionality of the package, we use the *Liver* data
set \[@ramana2011\] that we obtained form the UCI machine learning
repository \[@dua2017\]. It contains 579 patients of which 414 have a
liver the condition and the other 165 do not. We predict which patient
has a liver condition using medical measures and the age and gender of
the people.

We start with loading the package and the data.

``` r
library(ffcr)
data(liver)
```

## Training fast-and-frugal trees

We start by training a fast-and-frugal tree on the Liver data set. If
the first column in the data set is the class label, we can simply pass
the data set as the first argument.

``` r
model <- fftree(liver, use_features_once = FALSE, method = "greedy", max_depth = 6)
```

The model object shows the structure of the trees and its performance on
the data set.

``` r
print(model)
#> Fast-and-frugal Tree object
#>   type: "recursive" 
#> 
#> Call: 
#> fftree(data = data, method = "greedy", max_depth = 6, use_features_once = FALSE, 
#>     formula = formula)
#> 
#> Formula: 
#> diagnosis ~ age + sex + totalBilirubin + directBilirubin + alkaline + 
#>     alamine + aspartate + proteins + albumin + albuminGlobulin
#> 
#> Tree: 
#> 
#>  Reason: Predicted class - (Proportion of class Liver disease) (Number of objects classified)
#> 
#>   totalBilirubin > 1.65: Liver disease (1.00) (213)
#>     alkaline > 211.5: Liver disease (1.00) (132)
#>       age <= 25.5: No liver disease (0.00) (27)
#>         alkaline <= 144.5: Liver disease (1.00) (27)
#>           albuminGlobulin > 1.68: No liver disease (0.00) (4)
#>             alamine <= 19.5: No liver disease (0.00) (38)
#>               alamine > 19.5: Liver disease (1.00) (138)
#> 
#> 
#> Fitted values:
#>             Observed       Predicted    N
#>        Liver disease    Liver disease   0
#>     No liver disease    Liver disease 510
#>        Liver disease No liver disease   0
#>     No liver disease No liver disease  69
#> 
#> Fitting:                            
#>     AUC                   NA
#>     Accuracy            0.12
#>     Sensitivity          NaN
#>     Specificity         0.12
#>     Balanced accuracy    NaN
#>     F1 score            0.00
#>                   
#>     Depth     6.00
#>     Features  5.00
#>     Frugality 3.01
```

To visualize the tree we use

``` r
plot(model)
```

<img src="man/figures/README-fig1-1.png" width="60%" style="display: block; margin: auto;" />

To make predictions according to a fast-and-frugal tree, we can use the
`predict` function.

``` r
predict(model, newdata = liver[301:nrow(liver),], type = "metric")
#>               AUC          Accuracy       Sensitivity       Specificity 
#>         0.6078142         0.7383513         0.9481865         0.2674419 
#> Balanced accuracy          F1 score    True positives   False positives 
#>         0.6078142         0.8337130       183.0000000        63.0000000 
#>    True negatives   False negatives 
#>        23.0000000        10.0000000
```

## Training tallying models

To train tallying models, we use the *tally* function.

``` r
model <- tally(liver, max_size = 6)
```

Please consult the vignette of the package for more details on the
functionalities of the package and the book *Classification in the Wild*
for background information on fast-and-frugal trees and tallying models.

# References

Dua, Dheeru, and Casey Graff. 2017. “UCI Machine Learning Repository.”
University of California, Irvine, School of Information; Computer
Sciences. <http://archive.ics.uci.edu/ml>.

Katikopoulos, Konstantinos V., Özgür Şimşek, Marcus Buckmann, and Gerd
Gigerenzer. 2020. Classification in the Wild: The Science and Art of
Transparent Decision Making. MIT Press.

Ramana, Bendi Venkata, M Surendra Prasad Babu, and N. B. Venkateswarlu.
2011. “A Critical Study of Selected Classification Algorithms for Liver
Disease Diagnosis.” International Journal of Database Management Systems
3 (2): 101–14.

Rubinstein, Reuven. 1999. “The Cross-Entropy Method for Combinatorial
and Continuous Optimization.” Methodology and Computing in Applied
Probability 1 (2): 127–90.
