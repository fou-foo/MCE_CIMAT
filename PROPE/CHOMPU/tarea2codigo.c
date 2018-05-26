/* J. Antonio García: fou-foo@hotmail.com */
#include <stdio.h>

void main()
{
	int opt;
	int entero;
	float flotante;
	char foo[3] = "foo";
	printf("De regreso a C :( \n");
	printf("Ingrese 2 numeros enteros a sumar separados por un espacio:\n");
	printf("Vamos a leer un entero y un flotante:\n");
	printf("Ingresa el entero...\n");
	scanf("%d", &entero);
    printf("Ingresa el flotante...\n");
	scanf("%f", &flotante);
	printf("La suma de %d + %f es: %f \n", entero, flotante, entero + flotante);
    printf("Como un tema aparte mis amigos me dicen %s veamos como se corre el entero %d  usando el operador '>>' :\n", foo, entero);
    printf(" %d", entero >> 1);

}

