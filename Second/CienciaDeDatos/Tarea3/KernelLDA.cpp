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
  double kernel_value = 0;
  for (int i = 0; i < dimension; i++ )
  {
    kernel_value += pow( (double)(x[i] - y[i]),2);
  }
  kernel_value = exp(-kernel_value/ sigma);
  return( kernel_value );
}

// [[Rcpp::export]]
NumericMatrix  Kernel ( NumericMatrix W, NumericMatrix data, int n,  double sigma, int dimension )
{
  // Funcion para calcular  la matriz de similaridades de todos los puntos de una imagen 
  //  input: n: numero de muestra
  //         W: matriz inicializacda
  //         m: numero de variables
  //         M: m.a.
  //         sigma: sigma para kernell gaussiano, de las distancias
  //output: W, similaridad calculada, dense 
  int i, j = 0 ;
  double calculo = 0.;
  //comienza calculo de matriz de Kernel
  for(i=0; i < n; i++)
  {
    for(j=0; j< n; j++ )
    {
      calculo = KernelGauss (data.row(i), data.row(j), dimension, sigma);
      W(i, j )  = calculo;
      W(j, i )  = calculo;
    }
  }
  return(W);
}

// [[Rcpp::export]]
NumericMatrix  KernelTest (NumericMatrix W1, NumericMatrix test, NumericMatrix train, int l1, int l2,  double sigma, int dimension )
{
  // Funcion para calcular  la matriz de similaridades de todos los puntos de una imagen 
  //  input: l1: numero de muestra test
  //         l2 :numero d emuestra train
  //         W: matriz inicializacda
  //         m: numero de variables
  //         M: m.a. train
  //         M: m.a. tets
  //         sigma: sigma para kernell gaussiano, de las distancias
  //output: W, similaridad calculada, dense 
  int i, j = 0 ;
  double calculo = 0.;
  //comienza calculo de matriz de Kernel
  for(i=0; i < l1; i++)
  {
    //printf("%i i \n", i);
    for(j=0; j< l2; j++ )
    {
      //printf("%i j \n", j);
      calculo = KernelGauss (test.row(i), train.row(j), dimension, sigma);
      W1(i, j )  = calculo;
    }
  }
  return(W1);
}