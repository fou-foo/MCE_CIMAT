#ifndef FRUTA_DEFINICIONES
#define FRUTA_DEFINICIONES


#include <opencv2/opencv.hpp>
#include <opencv2/imgproc/imgproc.hpp>

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <sstream>

using namespace std;
using namespace cv;

#define smaller_image_size 100  //Tamaño (pixeles) de cada dimension de la imagen de salida



//PARAMETROS DE ENTRADA: varian de acuerdo a cada imagen que se desee procesar

//Tamaño (pixeles) considerados como fondo del borde inferior de la imagen final de la fruta detectada (marco inferior)
#define motor_shaft_height 0

//Distancia máxima (pixeles) entre las intensidades de dos pixeles (sus canales) que definen el cambio de fondo a fruta.
#define color_distance 21

//Dirección de la imagen (no necesariamente cuadrada) que contiene una fruta individual y tiene fondo blanco
string input_file_name = "platano.jpg";

//Cuadrado con las dimensiones de la imagen anterior que contiene la fruta en el centro (literalmente su centro debe contener pixeles de la fruta)
//30 corresponde a la posición en x de la esquina superior izquierda del cuadrado en la imagen
//60 corresponde a la posición en y de la esquina superior izquierda del cuadrado en la imagen
//240 corresponde a la longitud del cuadrado, estalecido para el eje x (horizontal) y para el eje y (vertical), en este caso.
Rect r_box(30,60,240,240);



//Otros ejemplos

/*#define motor_shaft_height 2
#define color_distance 10
string input_file_name = "manzana.jpg";
Rect r_box(360,470,1940,1940);*/

/*#define motor_shaft_height 10
#define color_distance 5
string input_file_name = "pera.jpg";
Rect r_box(390,320,1800,1800);*/


#endif // FRUTA_DEFINICIONES

