/* José Antonio García Ramirez fou-foo@hotmail.com*/
# include <stdio.h>

int strlen(char *s)                 //Para que el programa sea autocontenido definimos esta funcion que calcula la longitud de un string
{
    int n;
    for (n=0; *s!='\0'; s++)
        n++;
    return n;
}


// Ejercicio 1 :Escribe una función que cambie las letras de una cadena de caracteres por la siguiente, es decir a->b->c->...->y->z->a
void pi(char * word )
{
    int i, n = strlen(word);
    for(i = 0; i < n ; i++)
        word[i] =  ( ( ((int)(word[i]-'a') + 1)) % (26) + (int)'a')  ;  //Cambiamos de manera ciclica los carcateres z->a

}

// Ejercico 2: función tipo void que calcule el cuadrado de una matriz leída de archivo y otra función que escriba el resultado en consola y en un archivo "res.dat". Prueba con los siguientes archivos cua33.dat, cua55.dat, el primer renglon contiene 2 enteros indicando el tamaño de la matriz.

void printMatrix(int **matrixA, int rows, int cols )
{
    int i, j;
    FILE *archivo;
    char s[100];
    printf("A donde escribir:");
    scanf("%s", &s);
    archivo = fopen(s, "w");
    for( i = 0; i < rows; i++)
    {
        for( j = 0; j< cols; j++)
        {
            fprintf(archivo,"%d ", *(*(matrixA+i)+j));
        }
        fprintf(archivo, "\n");
    }
     fclose(archivo);

}



void Sqrt( char * matriz_s)
{
    FILE *archivo;
    int i, j, k,  rows, cols, **matriz, ** matrizOut, prod = 0;

    archivo = fopen(matriz_s, "r");                                      // abrimos el archivo
    fscanf(archivo, "%d", &rows);
    fscanf(archivo, "%d", &cols);
    matriz = (int **)malloc(rows * sizeof(int *));                      //reasignacion de tamaño de la matriz
    matrizOut = (int **)malloc(rows * sizeof(int *));

    for (i=0; i < rows; i++)
    {
         matriz[i] = (int *)malloc(cols * sizeof(int));
         matrizOut[i] = (int *)malloc(cols * sizeof(int));
    }
    for(i=0; i < rows; i++)                                              //leemos la matriz
    {
        for(j = 0; j < cols; j++)
        {
            fscanf(archivo, "%d", &matriz[i][j]);
            printf("%d ", matriz[i][j] );
        };
        printf("\n");
    }
    fclose(archivo);                                                    //cerramos el punterp como buena practica
    printf("Cuadrado de la matriz\n");
    for(i=0; i < rows; i++)
    {
        for(j = 0; j < cols; j++)
        {
            for( k = 0; k < rows; k ++)
            {
                prod += matriz[i][k]*matriz[k][j];
            }
            matrizOut[i][k] = prod;
            printf("%d ", matrizOut[i][k]);
            prod = 0;
        };
  //      printf("\n");
    }

    printMatrix(matrizOut, rows, cols);
}

// Ejercicio 3: función que regrese una cadena con los caracteres de la siguiente cadena "propedeutico cimat" ordenados de menor a mayor
char * sort(char * word)
{
    int i, j, n, maxi = 0;
    char * orden = word;
    printf("palabra final %s",orden);
    n = strlen(word);
    for (i = 0; i < n; i++)
    {

        for(j = 0; j<n; j++)
        {
            if( (int)word[j] > (int)orden[i] )  //la vendita métrica para compara los elementos
            {
                maxi = orden[j] ;
                orden[j] = orden[i];
                orden[i] = maxi;
            }
        }
    }
    return(orden);

}

void main()
{
    // Entradas del primer inciso
    char word[500], path [500], word_r[500];
    printf("Primer palabra:\n");                 //Leemos la palabra
    scanf("%s", word);
    pi(&word);*/
    // Salidas del primer inciso
    printf(" despues del cambio la cadena es: %s", word);

    // Entradas de la segunda inciso
    printf("Path del archivo con matriz: \n");
    scanf("%s", &path);
    Sqrt(path);

     // Entradas del tercer inciso

    printf("Primer palabra:\n");                 //Leemos la palabra
    scanf("%s", word_r);
    // Salidas del primer inciso
    printf(" la palabra ordenada es %s", sort(&word_r));

}


// abeflmnxyuz
