#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
List  fastf(const arma::mat& X, const arma::vec& yr, int Sim=100){
        RNGScope scope;	
        int n = X.n_rows, k = X.n_cols;
        int df2 = n - k;
        int df1 = k-1;
        
        arma::mat simdata(Sim,3);
        simdata.row(0) = arma::zeros<arma::mat>(1,3);
        for(int i = 0 ; i < Sim ; i++) {
                // Sample initial data
                arma::colvec y = shuffle(yr);
                // Calculate sample mean and std dev
                arma::colvec coef = arma::solve(X, y);
                arma::colvec fitted = X*coef - mean(y);
                arma::colvec res  = y - fitted;
                double mtss = std::inner_product(fitted.begin(),
                                                 fitted.end(), fitted.begin(), 0.0)/df1;
                // std.errors of coefficients
                double s2 = std::inner_product(res.begin(), res.end(), res.begin(), 0.0)/df2;
        
                double fv = mtss/s2; 
                simdata(i, 0) = df1;
                simdata(i, 1) = df2;
                simdata(i, 2)=  fv ;
                
        }  
        
        return Rcpp::List::create(Rcpp::Named("F")   =  simdata );       
        
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma:: vec Csample(const int N, const int b, const arma::vec& yr){
        RNGScope scope;
        arma::uvec Iid(N);
        arma::colvec y(N);
        Iid= arma::randi<arma::uvec>(N,arma::distr_param(1, b));
        y=yr(Iid);
        return y;
}
        

