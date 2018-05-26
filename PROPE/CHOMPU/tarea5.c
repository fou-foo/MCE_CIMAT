/* José Antonio García Ramirez fou-foo@hotmail.com*/
# include <stdio.h>
# include <string.h>
# define N 10
# define row 3
# define col 3
//Ejercicio 3: Realiza una función que cambie los valores de 2 arreglos de enteros.
void swapVector(int* vectorA, int* vectorB, int dimension )  // Se piden apuntadores para evitar la referencia por valor y usar menos memoria
{
    int i, temporal;
    for (i = 0; i < dimension; i++)
    {
        temporal = *(vectorB+i);
        *(vectorB+i) = *(vectorA+i);
        *(vectorA+i) = temporal;
    }
}

// Ejercicio 4: Realiza una función que cambie los valores de 2 matrices 3x3 de enteros
void swapMatrix( int (*matrixA)[row], int (*matrixB)[row], int rows, int cols) // Se piden apuntadores para usar menos memoria,
{                                                                  // Se piden dimensiones de las matrices para reurilizar el codígo en un futuro pues el mismo codigo funciona para el caso n*n
    int i, j, temporal;
    for (i = 0; i < rows ; i++)
    {
        for(j = 0; j < cols; j++)
        {
            temporal = *(*(matrixB+i)+j);
            *(*(matrixB+i)+j) = *(*(matrixA+i)+j);
            *(*(matrixA+i)+j) = temporal;
        }
    }

}

//Ejercicio 5: Escribe una función que copie el texto b="dedo" en a="hola"
void swapString( char *a, char * b, char* apuntador_a, char* apuntador_b)
{
    int i, j;
    char temporal;
    for (i=0; a[i]!='\0'; i++)
    {
        temporal = b[i];
        b[i] = a[i];
        a[i] = temporal;
    }

    for(j = 0 ;j < i; j++)
    {
        temporal = *(apuntador_b+j);
        *(apuntador_b+j) = *(apuntador_a+j);
        *(apuntador_a+j) = temporal;
    }
}

//Ejercicio 6:Escribe una función que cuente las veces que aparece cada letra de una palabra recibida utilizando scanf
void Dict(char * word )
{
    int j, contador = 0;
    for(; *word!='\0'; word++)                  //Por cada letra se cuentan sus ocurrencia y se imprime el numero de veces que aparece en la palabra
    {
        for(j = 0; j < strlen(word); j++)       //Se busca en todo el string
            if( word[j] == *word)
            {
                contador++;
            }
        printf("\nLa letra %c aparece %d veces", *word, contador);
        contador = 0;
    }

}


//Ejercicio 7: Escribe una función que multiplique un vector(n=3) y una matriz (3x3), utiliza "vec3.dat" y "mat33.dat" imprima en pantalla el resultado y sea guardado en "res.dat"
void SumaWrite( char * matriz_s, char * vector_s)
{
    FILE *archivo;
    int i, j, matriz[3][3], vector[3], prod = 0;
    archivo = fopen(matriz_s, "r");
    for(i=0; i < 3; i++){
        for(j = 0; j < 3; j++)
        {
            fscanf(archivo, "%d", &matriz[i][j]);

        }printf("\n");
    }
    fclose(archivo);                                                    //cerramos el punterp como buena practica
    archivo = fopen(vector_s, "r");
    for(i=0; i < 3; i++)
        fscanf(archivo,"%d",&vector[i]);
    fclose(archivo);
    archivo = fopen("res.dat", "w");
    for(i=0; i < 3; i++)
    {
        for(j = 0; j < 3; j++)
        {
            prod += matriz[i][j]*vector[j];

        }
        fprintf(archivo,"%d   ",prod);
        printf("\nproducto :%d", prod);
        fprintf(archivo,"\n");
        prod = 0;
    }
    fclose(archivo);


}

void main ()
{
    int i, j, vectorA[N] = { 1,   2,   3,   4,   5,   6,   7,   8,   9,  10} ,  vectorB[N] = { 11,  12,  13,  14,  15,  16,  17,  18,  19,  20},
            matrixA[row][col] = { {1,   2,   3},   {4,   5,   6},   {7,   8,   9}},
            matrixB[row][col] = { {11,   12,   13},   {14,   15,   16},   {17,   18,   19}};
    //Salida del ejericio 3
    char b[]="dedo", a[]="hola", palabraDic[100], pathMatriz[256], pathVector[256];
    char *apuntador_b="dedo", *apuntador_a ="hola";
    printf("Vector A original: \n");
    for (i = 0; i < N; i++)
        printf(" %d ", *(vectorA+i));

    printf("\nVector B original: \n");
    for (i = 0; i < N; i++)
        printf(" %d ", *(vectorB+i));
    swapVector(&vectorA[0], &vectorB[0], N);

    printf("\n \n Vector A intercambiado: \n");
    for (i = 0; i < N; i++)
        printf(" %d ", *(vectorA+i));

    printf("\nVector B intercambiado: \n");
    for (i = 0; i < N; i++)
        printf(" %d ", *(vectorB+i));
    // Salida del ejercicio 4
    printf("\n\n\n\n\nMatriz A original: \n");
    for (i = 0; i < row; i++)
    {
        for(j = 0; j < col; j++)
            printf(" %d ", *(matrixA[i]+j) );
        printf("\n");
    }
    printf("Matriz B original: \n");
    for (i = 0; i < row; i++)
    {
        for(j = 0; j < col; j++)
            printf(" %d ", *( *(matrixB+i) + j));
        printf("\n");
    }
    swapMatrix(&matrixA[0][0] , &matrixB[0][0], row, col);

    printf("\nMatriz A cambiada: \n");
    for (i = 0; i < row; i++)
    {
        for(j = 0; j < col; j++)
            printf(" %d ", *(*(matrixA+i)+j) );
        printf("\n");
    }
    printf("\nMatriz B cambiada: \n");
    for (i = 0; i < row; i++)
    {
        for(j = 0; j < col; j++)
            printf(" %d ", *( *(matrixB+i) + j));
        printf("\n");
    }

    // Salida del ejercicio 5
    printf("\n\n\n\n\nTexto original en b: %s", b);
    printf("\nTexto original en a: %s", a);
    printf("\nTexto original en b (apuntador): %s", b);
    printf("\nTexto original en a (apuntador): %s", a);
    swapString(&b, &a, &apuntador_b, &apuntador_a );
    printf("\nTexto cambiado en b (arreglo): %s", b);
    printf("\nTexto cambiado en a (arreglo): %s", a);
    printf("\nTexto cambiado en b (apuntador): %s", apuntador_b);
    printf("\nTexto cambiado en a (apuntador): %s", apuntador_a);

    // Salida del ejercicio 6
    printf("\n\n\n\n\n Entre una palabra para contar sus letras:");
    scanf("%s", palabraDic);
    Dict(&palabraDic);

    // Salida del ejercicio 7
    printf("\n\n\n\n\n Entre la ruta del archivo de la matriz: ");
    scanf("%s", pathMatriz);
    printf("\n Entre la ruta del archivo para el vector: ");
    scanf("%s", pathVector);
    SumaWrite(pathMatriz, pathVector);

}



