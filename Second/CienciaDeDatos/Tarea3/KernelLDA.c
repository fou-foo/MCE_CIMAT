#include <math.h>
#include <cmath>
#include <RcppEigen.h>
#include <Rcpp.h>
#include <iostream>
#include <vector>
#include <Eigen/Core>
#include <iostream>
using namespace Rcpp;
// [[Rcpp::depends(RcppEigen)]]


// [[Rcpp::export]]
double KernelGauss (NumericVector x, NumericVector y, int dimension, double sigma)
{
  //Calculo individual del kernel entre dos observaciones
  // input: x observaciones
  //        y observaciones
  double alpha = 1.0;
  double kernel_value = 0;
  for (int i = 0; i < dimension; i++ )
  {
    kernel_value += pow( (double)(x[i] - y[i]),2);
  }
  kernel_value = exp(-kernel_value/ sigma);
  return( kernel_value );
}

// [[Rcpp::export]]
NumericMatrix  Kernel ( NumericMatrix W, int n,  double sigma )
{
  // Funcion para calcular  la matriz de similaridades de todos los puntos de una imagen 
  //  input: n: numero de muestra
  //         m: numero de variables
  //         M: m.a.
  //         sigma: sigma para kernell gaussiano, de las distancias
  //output: W, similaridad calculada, dense 
  int i, j = 0 ;
  float vecindad = (float)distancia;
  float d2 = 0.;
  float calculo = 0.;
  std::vector<T> tripletList; //inicializamos tripleta
  tripletList.reserve((int)floor(h*h*w*w*.1));
  //comienza calculo de matriz de similaridades
  for(i=0; i < h; i++)
  {
    for(j=0; j< w; j++ )
    {
      for(k=0; k<h; k++)
      {
        for(l=0; l<w; l++)
        {
          d2 = (float)(pow(i-k,2)+pow(j-l,2));
          if(d2 <= vecindad)
          {
            calculo = (float) (exp(-pow(M(j,i)-M(l,k),2)/(2*pow(sigJ,2))-d2/(2*pow(sigd,2))));
            tripletList.push_back(T((j)*h+i, (l)*h+k, calculo ));
          }
        }
      }
    }
  }
  //termina calculo de matriz de similaridades
  Eigen::SparseMatrix<float>  W(h*w, h*w); //construccion de matriz de similaridades
  W.setFromTriplets(tripletList.begin(), tripletList.end());
  return(W);
}