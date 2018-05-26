// Jose Antonio García Ramirez
# include <stdio.h>
# include <math.h>
/*  Realiza una función que calcule los primeros n números de la serie fibonacci*/
/*  Aunque este problema se puede resolver iterativamente opte por la solución cerrada
    que hace uso de funciones generadoras pues me agrada más en vista de que no requerimos
    calcular los primeros (n-1)-esimos resultados para obtener el n-esimo*/
void primos()
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
void fibo()
{
        // fibo;
    int i, fibo;
    printf("Ingrese numero de termino hasta el que desea conocer la serie de Fibonacci:\n");
    scanf("%d",&fibo);
    printf("in : %d", fibo);
    for (i = 1; i <= fibo; i++)
    {
        printf("\nTermino %d de Fibo es: %lf ",i, round( (1/sqrt(5))*pow(( ( 1+sqrt(5) ) /2),i) - pow((1/sqrt(5))*( ( 1-sqrt(5) ) /2),i)));     //calculo el valor de la forma cerrada y lo imprimo
    }

}
void main()
{
    primos();
    fibo();
}
