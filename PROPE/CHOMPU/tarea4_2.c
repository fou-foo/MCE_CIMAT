// Jose Antonio Garc�a Ramirez
# include <stdio.h>
# include <math.h>
/*  Escribe un programa que recibe un n�mero y determina si es primo o no. */
/*  Aunque dista de ser una soluci�n optima (por tiempo de ejecuci�n y uso de meoria) implemente la soluci�n de la criba de Erastostenes por cuestiones de tiempo*/

void main()
{
    int i,j, numeros[10000], x = 1;
    for (i = 2;i < 10000; i++)
        numeros[i] = 1;                 //se inicializa el arreglo de numeros

    for (i = 2;i < 10000; i++)          // se recorre para marcar los que son multiplo de otro
    {
        if (numeros[i])                 // si no es primo entonces
        {
            for (j = i; i * j < 10000; j++) // basta con hacerlo sqrt(100000) veces por propiedades de algebra
                numeros[i * j] = 0;         //no es primo
        }
    }

    for (i = 2;i < 10000; i++)
    {
        if (numeros[i])
            printf("El numero %d es primo \n", i);
    }

}
