#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericMatrix makeCueSplitsC(NumericVector criterion, NumericVector cue){
  int n = criterion.size();
  NumericVector uniqueCueValues = unique(cue);
  std::sort(uniqueCueValues.begin(), uniqueCueValues.end());
  int nUnique = uniqueCueValues.size();

  IntegerVector a(nUnique);
  IntegerVector b(nUnique);
  IntegerVector c(nUnique);
  IntegerVector d(nUnique);
  

  nUnique -= 1;
  // Create empty matrix to fill in confusionMatrix
  NumericMatrix confusionMatrix(nUnique, 5);
  int nPos = 0;
  int nNeg = 0;
  int thX = 0;

  
  for(int i = 0; i < n; ++i) {
  	
  	if(cue[i] > uniqueCueValues[thX]){
  	    thX += 1;
  	    c[thX] = c[thX-1] + c[thX];
  	    d[thX] = d[thX-1] + d[thX];
  	}
  	if(criterion[i] == 1){
  		nPos += 1;
  		c[thX] += 1;
  	} else {
		nNeg += 1;
  		d[thX] += 1;
	}
  	
  
  	
  	}
  	
  	
  	for(int j = 0; j < nUnique; ++j){
  		confusionMatrix(j,1) = nPos - c[j];
  		confusionMatrix(j,2) = nNeg - d[j];
  		confusionMatrix(j,3) = c[j];
  		confusionMatrix(j,4) = d[j];
  	}
  	

  confusionMatrix(_, 0) = uniqueCueValues;
  return confusionMatrix;
}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

