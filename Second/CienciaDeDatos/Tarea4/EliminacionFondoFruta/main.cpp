// Extracción de una fruta individual en una imagen con fondo sólido
// Autor: Ana Karen Loya Olivas
// Codigo basado en el proyecto https://github.com/Horea94/Fruit-Images-Dataset
// Se realizó con Opencv 2.4.9.1

// Para utilizarse modificar los PARAMETROS DE ENTRADA del archivo fruta_definiciones.h

// Desde terminal en linux:
// 	Compilación: g++ main.cpp -o output `pkg-config --cflags --libs opencv`
// 	Ejecución:   ./output



#include "fruta_definiciones.h"


//Estructura que contiene dos puntos en el plano (x,y) y representan un rectángulo
struct t_bbox {
    Point min, max;
};

//Función que regresa la mayor diferencia en valor absoluto entre los canales de dos pixeles.
int get_color_distance(Vec3b &color1, Vec3b &color2)
{
    int c0 = abs(color1[0] - color2[0]);
    int c1 = abs(color1[1] - color2[1]);
    int c2 = abs(color1[2] - color2[2]);

    int max = c0;
    if (max < c1)
        max = c1;
    if (max < c2)
        max = c2;

    return  max;
}

//Funcion que asigna el pixel sin clasificar como fondo y verifica si sus cuatro vecinos son fondo
//de acuerdo a la diferencia de color
void flood_fill(int x, int y, Mat *picture, char** filled_matrix, int color_tolerance)
{
    if (!filled_matrix[x][y]) {
        filled_matrix[x][y] = 1;

        //Se consideran los casos esquina
        if (y + 1 < smaller_image_size
                && get_color_distance(picture->at<Vec3b>(Point(y, x)), picture->at<Vec3b>(Point(y + 1, x))) <= color_tolerance)
            flood_fill(x, y + 1, picture, filled_matrix, color_tolerance);

        if (y - 1 >= 0
                && get_color_distance(picture->at<Vec3b>(Point(y, x)), picture->at<Vec3b>(Point(y - 1, x))) <= color_tolerance)
            flood_fill(x, y - 1, picture, filled_matrix, color_tolerance);

        if (x + 1 < smaller_image_size
                && get_color_distance(picture->at<Vec3b>(Point(y, x)), picture->at<Vec3b>(Point(y, x + 1))) <= color_tolerance)
            flood_fill(x + 1, y, picture, filled_matrix, color_tolerance);

        if (x - 1 >= 0
                && get_color_distance(picture->at<Vec3b>(Point(y, x)), picture->at<Vec3b>(Point(y, x - 1))) <= color_tolerance)
            flood_fill(x - 1, y, picture, filled_matrix, color_tolerance);
    }
}

//Funcion que asigna el pixel sin clasificar como fruta y verifica si sus cuatro vecinos también se
//encuentran sin clasificar para asignarlos como fruta
void flood_fill2(int x, int y, char** filled_matrix)
{
    if (filled_matrix[x][y] == 0) {
        filled_matrix[x][y] = 2;

        //Se consideran los casos esquina
        if (y + 1 < smaller_image_size && !filled_matrix[x][y + 1])
            flood_fill2(x, y + 1, filled_matrix);

        if (y - 1 >= 0 && !filled_matrix[x][y - 1])
            flood_fill2(x, y - 1, filled_matrix);

        if (x + 1 < smaller_image_size && !filled_matrix[x + 1][y])
            flood_fill2(x + 1, y, filled_matrix);

        if (x - 1 >= 0 && !filled_matrix[x - 1][y])
            flood_fill2(x - 1, y, filled_matrix);
    }
}

//Función que busca el rectangulo que contiene la fruta encontrada
t_bbox compute_bbox(char** matrix)
{
    t_bbox bbox;

    bbox.min.x = smaller_image_size;
    bbox.min.y = smaller_image_size;
    bbox.max.x = 0;
    bbox.max.y = 0;

    for (int i = 0; i < smaller_image_size; i++)
        for (int j = 0; j < smaller_image_size; j++){
            if (matrix[i][j] == 2) {
                if (bbox.min.x > i)
                    bbox.min.x = i;
                if (bbox.min.y > j)
                    bbox.min.y = j;
                if (bbox.max.x < i)
                    bbox.max.x = i;
                if (bbox.max.y < j)
                    bbox.max.y = j;
            }
        }
    return bbox;
}

//Función que localiza la fruta en la imagen y actualiza dicha imagen
//con el cuadrado que contiene la fruta sin el fondo y un marco inferior definido.
bool remove_background(Mat &image)
{
    //Asignación de memoria para la mascara de la imagen que localiza la fruta y el fondo
    char **matrix = new char*[smaller_image_size];
    for (int i = 0; i < smaller_image_size; i++) {
        matrix[i] = new char[smaller_image_size];
        for (int j = 0; j < smaller_image_size; j++){
            matrix[i][j] = 0; // 2 - fruta // 1- fondo
        }
    }

    //Se busca el fondo a partir de los bordes
    // Recorrido desde borde superior
    for (int i = 0; i < smaller_image_size; i++)
        flood_fill(0, i, &image, matrix, color_distance);

    // Recorrido desde borde izquierdo
    for (int i = 0; i < smaller_image_size; i++)
        flood_fill(i, 0, &image, matrix, color_distance);

    // Recorrido desde borde derecho
    for (int i = 0; i < smaller_image_size; i++)
        flood_fill(i, smaller_image_size - 1, &image, matrix, color_distance);

    // Recorrido desde borde inferior menos un marco
    for (int i = 0; i < smaller_image_size; i++)
        flood_fill(smaller_image_size - motor_shaft_height - 1, i, &image, matrix, 1);

    //Se comienza a llenar la fruta desde el centro
    flood_fill2(smaller_image_size / 2, smaller_image_size / 2, matrix);

    //Se ignora el marco inferior especificado (se asigna como fondo)
    for (int i = smaller_image_size - motor_shaft_height; i < smaller_image_size; i++)
        for (int j = 0; j < smaller_image_size; j++)
            matrix[i][j] = 1;

    //Se busca el rectangulo que contiene la fruta
    t_bbox bbox = compute_bbox(matrix);

    //Se aplica la mascara y se dibuja el fondo de blanco
    for (int i = 0; i < smaller_image_size; i++)
        for (int j = 0; j < smaller_image_size; j++)
            if (matrix[i][j] != 2) {//No hay fruta
                image.at<Vec3b>(Point(j, i))[0] = 255; //Dibujar blanco
                image.at<Vec3b>(Point(j, i))[1] = 255;
                image.at<Vec3b>(Point(j, i))[2] = 255;
            }
    //El marco inferior se dibuja como blanco
    for (int i = smaller_image_size - motor_shaft_height; i < smaller_image_size; i++)
        for (int j = 0; j < smaller_image_size; j++){
                image.at<Vec3b>(Point(j, i))[0] = 255; //Dibujar blanco
                image.at<Vec3b>(Point(j, i))[1] = 255;
                image.at<Vec3b>(Point(j, i))[2] = 255;
            }

    //Liberación de memoria dinámica
    for (int i = 0; i < smaller_image_size; i++)
        delete[] matrix[i];
    delete[] matrix;

    //Si el rectangulo de la fruta existe (valores lógicos) se actualiza la imagen sin fondo
    if ((bbox.min.x < bbox.max.x) && (bbox.min.y < bbox.max.y)) {

        //Se extrae la imagen del rectangulo de la fruta
        cv::Mat tmp = image(cv::Rect(bbox.min.y, bbox.min.x, bbox.max.y - bbox.min.y + 1, bbox.max.x - bbox.min.x + 1)).clone();

        //Se actualiza la imagen original con la fruta centrada
        image.setTo(Scalar(255, 255, 255));
        int max_size = bbox.max.y - bbox.min.y + 1;
        if (max_size < bbox.max.x - bbox.min.x + 1)
            max_size = bbox.max.x - bbox.min.x + 1;
        Mat tmp_new = Mat(max_size, max_size, image.type());
        tmp_new.setTo(Scalar(255, 255, 255)); //Dibujar blanco, de otra forma es gris
        tmp.copyTo(tmp_new(cv::Rect((max_size - (bbox.max.y - bbox.min.y + 1)) / 2, (max_size - (bbox.max.x - bbox.min.x + 1)) / 2,
                                    bbox.max.y - bbox.min.y + 1, bbox.max.x - bbox.min.x + 1)));
        image = tmp_new;

        return true;
    }
    else
        return false; //La imagen esta en blanco, no se detecto fruta

}


//Funcion principal que lee una imagen, localiza la fruta, elimina el fondo y guarda el resultado en una nueva imagen cuadrada
int main(void){

    //Lectura de imagen
    Mat input_image = imread(input_file_name, CV_LOAD_IMAGE_COLOR);;
    if (input_image.empty()){
        printf("¡No se pudo abrir la imagen!\n");
        return 1;
    }

    //Recorte de la imagen en base al cuadrado estimado que contiene la fruta centrada
    Mat smaller_image = input_image(r_box);

    //Asignación del nombre de la imagen de salida
    std::stringstream ss;
    ss << smaller_image_size;
    string out_filename = input_file_name + "_" + ss.str() + ".jpg";

    //Se redimensiona la imagen cuadrada estimada
    resize(smaller_image, smaller_image, Size(smaller_image_size, smaller_image_size));

    //Si se localiza la fruta y se elimina exitosamente el fondo, se guardan los resultados
    if (remove_background(smaller_image)) {

        //Se redimensiona la imagen cuadrada de la fruta y se guarda
        resize(smaller_image, smaller_image, Size(smaller_image_size, smaller_image_size));
        if (!imwrite(out_filename, smaller_image)) {
            printf("¡No se pudo guardar la imagen!\n");
            return 1;
        }
    }
    else
        printf("¡No se encontró la fruta! Verificar los parametros de entrada.\n");

    return 0;
}
