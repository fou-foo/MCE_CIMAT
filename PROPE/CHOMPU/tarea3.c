
// García Ramirez José Antonio fou-foo@hotmail.com
# include <stdio.h>
# include <math.h>
# include <string.h>
# include <stdlib.h>

void leer_cadena(char linea1[])
{
    char caracter;
    int i = 0;
    caracter = getchar();
    while( caracter != '\n'  )
    {
        linea1[i] = caracter;
        caracter = getchar();
        i++;
    }

}

void primero( )
{
    int a1, a2, swap;                                       // se leen e imprimen los valores originales
    printf("Ingrese el primer numero entero:\n");
    scanf("%d", &a1);
    printf("Ingrese el segundo numero entero:\n");
    scanf("%d", &a2);
    printf("El primer numero es :%d\n", a1);
    printf("El segundo numero es :%d\n", a2);
    swap = a2;                                              // se intercambian los numeros y se imprimen
    a2 = a1;
    a1 = swap;
    printf("Despues del intercambio el primer numero es: %d\n ", a1);
    printf("Despues del intercambio el segundo numero es %d\n", a2);
}

void segundo()
{
    char binario1_char[8], binario2_char[8], a;
    float binario1 = 0, binario2 = 0;
    int i;
    printf("Introduzca el primer numero binario:");         // Se leen los números de binarios a int
    scanf("%s", &binario1_char);
    printf("Introduzca el segundo numero binario:");
    scanf("%s", &binario2_char);
    for(  i = 0 ; i < sizeof(binario1_char) ; i++  )               //se parsean los números
    {
        binario1 += (pow(2, 8 - (i + 1) ) * (float)(binario1_char[i] - '0'));
        printf("%c  :  %f:\n",binario1_char[i],(pow(2, 8- i ) * (float)(binario1_char[i] - '0')));
        binario2 += (pow(2, 8 - (i + 1) ) * (float)(binario2_char[i] - '0'));
    }
    printf("El primer numero en decimal es: %d\n", (int)binario1 );
    printf("El segundo numero en decimal es: %d\n", (int)binario2 );
    printf("La suma de los numeros es: %d", (int)(binario1 + binario2));

}

void tercero()
{
    char linea1[256], linea2[256], concadenacion[256*2];
    int i = 0;
    printf("Digite la primer linea:");
    getchar();
    leer_cadena(linea1);                                   // Se leen ambas cadenas
    printf("Digite la segunda linea:");
    leer_cadena(linea2);
    printf("La concadenacion de la primer cadena con la segunda es:");
    printf("%s", strcat(linea1, linea2));                 // se concadena con ayuda de una funcion en la libreria 'string.h'
}

void cuarto()
{
    char inicio[7] = "1^2*3+4";
    char res[7];
    int i = 0, resultado_parcial, posiciones[1];                /* Arreglos para guardar la posicion de los operadores de mayor a menor presedencia en la cadena
                                                                  aunque en mi ejemplo solo uso una para cada orden de presedencia una implementacion general deberia cubrir todos los operadores y como se
                                                                  desconoce el tamaño de la expresion los arreglos deberian ser dinamicos*/
    printf("Consideremos la expresion %s", inicio);            /*Partimos de una expresión sin parentesis y con las siguientes simplificaciones:
                                                                1) Los operandos son enteros singletones (tienen un solo digito) para facilitar el casteo de char a int
                                                                2) no hay espacios en blanco entre los operadores y los operandos */
    getchar();
    for( i = 0; i < sizeof(inicio); i++ )                       // se busca por la posicion de los operadores de menor procedencia
    {
        if( inicio[i] == '^')                                   // Una mejora seria cachar las expresiones 'pow'
        {
            posiciones[0] = i;
        }
    }
    resultado_parcial = pow(inicio[posiciones[0] - 1] - '0', inicio[posiciones[0] + 1] - '0');                     // en vista de que suponemos solo una operacion de cada orden
    for(i = 0; i <= posiciones[0]; i++)
    {
        inicio[i] = ' ';
    }
    inicio[posiciones[0] + 1] = resultado_parcial+'0';
    printf("\n   resultado parcial %s\n", inicio);
    i = 0;
    for( i = 0; i < 7; i++ )                       // se busca por la posicion de los operadores de segunda procedencia
    {
        if( inicio[i] == '*')
        {
            posiciones[0] = i;
        }
        else if ( inicio[i] == '/')
        {
            posiciones[0] = i;
        }
    }
    i = 0;
    resultado_parcial = (int)(inicio[posiciones[0] - 1] - '0') * (int)( inicio[posiciones[0] + 1] - '0' );
    for(i = 0; i <= posiciones[0]; i++)
    {
        inicio[i] = ' ';
    }
    inicio[posiciones[0] + 1] = resultado_parcial + '0';
    printf("   resultado parcial %s\n", inicio );

    i = 0;
    for( i = 0; i < 7; i++ )                       // se busca por la posicion de los operadores de mayor procedencia
    {
        if( inicio[i] == '+')
        {
            posiciones[0] = i;
        }
        else if ( inicio[i] == '-')
        {
            posiciones[0] = i;
        }
    }
    i = 0;
    resultado_parcial = (int)(inicio[posiciones[0] - 1] - '0') + (int)( inicio[posiciones[0] + 1] - '0' );
    for(i = 0; i <= posiciones[0]; i++)
    {
        inicio[i] = ' ';
    }
    inicio[posiciones[0] + 1] = resultado_parcial + '0';
    printf("   resultado final %s", inicio );
}

void quinto()
{
    int octal;
	printf("Ingrese un numero en formato octal: ");
	scanf("%o", &octal);                                        // Leemos con 'scan' el formato octal
	printf("El numero en decimal es: %d\n", octal);             // el casteo lo hace el compilador
}

void sexto()
{
    int a1 = 1 , a2 = 2;
	printf("Tenemos dos variables...\n");
	printf("La primera se llama a y es un entero que vale: %d\n", a1);
	printf("La segunda se llama b y es otro entero que vale %d\n", a2);
	printf("La operacion 'a==b' de comparacion se valua como una condicion logica, valiendo en este caso: %d \n", a1==a2 );
	printf("La operacion 'a=b' de asignacion hace que 'a' valga ahora: %d\n", a1=a2 );
}

void septimo()
{
    char cadena1[256];
    int  i = 0, bandera = 1;
	printf("Ingrese una cadena para comprobar si es Palindromo:\n");        //Leemos la palabra, pues el problema especifica 'cadena' y no frase
	scanf("%s", &cadena1);
	printf("%s\n", cadena1);
	for(i = 0; i < strlen(cadena1); i++)
	{
		if(cadena1[i] != cadena1[(strlen(cadena1) -1 ) - i])                //Identificamos las diferencias si hay alguna ya no es palindromo
        {
            printf("%c  %c\n",cadena1[i] , cadena1[(strlen(cadena1)-1) - i]);
            bandera= 0;
        }
	}
	if(bandera == 1)
    {
        printf("La cadena  es palindromo\n");
    }
    else
    {
         printf("La cadena no es palindromo\n");
    }

}

void octavo()
{
    char cadena[256], n, last, j, caracter, salida[256];
    int i=0;
	printf("Digite cadena para invertir palabras: ");
	getchar();
	while((caracter = getchar()) != '\n')
	{
		cadena[i] = caracter;
		i++;
	}
	n = strlen(cadena);
	last = -1;
	printf("La cadena original es: %s", cadena);
    printf("El resultado de invertir las palabras de la cadena es:\n\n");
	for(i = 0; i < n; i++)
	{
		if(cadena[i] == ' ')                                // se buscan los espacios, para asegurar que una palabra termino
        {
			for(j = i-1; j > last; j--)
            {
                printf("%c", cadena[j]);
            }
			printf(" ");
			last = i;                                       //se guarda la ultima posicion de las palabras
		}
	}
	for(j = i-1; j > last; j--)                             // El caso de la ultima palabra de la frase
				printf("%c", cadena[j]);
	printf("\n\n");


}

void noveno()
{
	int numero, numero_arreglo[20] = {0}, i, x, b, l, suma;
	printf("Ingrese un numero a verificar que es Armstrong:\n es decir que la suma de cada uno de sus mismos digitos elevado al numero total de digitos es igual a el.\n");
	scanf("%d", &numero);
	x = numero;
	l = 0;
	suma = 0;
	for(i = 0; x > 0; i++)                                          //Pasamos el número a un arreglo
	{
		numero_arreglo[i] = x % 10;
		x /= 10;
		l++;                                                        //guardamos el tamaño del arreglo
	}
	for (i = 0; i < l; i++)
	{
		suma += pow(numero_arreglo[l -1 - i], l);                   // se evalua la definicion

	}
	if(suma == numero)
    {
        printf("El numero es Armstrong \n \n");
    }
    else
    {
        printf("El numero no lo es\n");
    }
}

void doceavo()
{
    int entero, i;
	printf("Ingresa un entero : ");
	scanf("%d", &entero);
	for (i = 1; i <= 5; ++i)
	{
		printf("%d**2 = %d y %d**3 = %d\n", entero + i, (entero + i)*(entero + i), entero + i, (entero + i)*(entero + i)*(entero + i));
	}

}

void trece()
{
    int numero, x, suma, dig;
	printf("Introduzca entero: ");
	scanf("%d", &numero);
	x = numero;                                             //copiamos el numero para hacer operaciones sobre él
	suma = 0;
	while(x != 0)
	{
		suma += (x % 10);
		x = x / 10;
	}
	printf("La suma de los digitos de %d es:  %d\n", numero, suma);
}

void catorce()
{
    char linea1[200];
	printf("Ingrese dia de la semana en palabra sin acentos y en minusculas: ");
	getchar();
	scanf("%s", &linea1);
	if(strcmp(linea1, "domingo") == 0 || strcmp(linea1, "sabado") == 0)
		printf("%s NO es laboral.\n", linea1);
	else
		printf("%s SI es laboral\n", linea1);
}

void diez_seis()
{
    int cota, suma = 0;
	for (cota = 1; cota <= 50; cota++)
	{
		suma += cota;
		//printf("%i \n", cota);
	}
	printf("La suma de los 50 numeros naturales (1 al 50): %d\n", suma);
}

void diez_siete()
{
    int b, c, n = 3;
	float lados[3];
	printf("Ingrese el primer lado: \n");                       // leemos los lados
	scanf("%f", &lados[0]);
	printf("Ingrese el segundo lado;\n");
	scanf("%f",&lados[1]);
	printf("Ingrese el tercer lado: \n", &lados[2]);
	scanf("%f", &lados[2]);
	if(lados[0] <= lados[1] + lados[2] && lados[1] <= lados[0] + lados[2] && lados[2] <= lados[0] + lados[1])  //se verifica que los lados cumplan la desigualdad del triangulo, la igualdad representa los casos de triangulos degenerados
	{
	    printf("Triangulo correcto\n \n");
		if(fabs(lados[0] - lados[1]) < .0001 && fabs(lados[0] - lados[2]) < .0001)
        {
            printf("Triangulo equilatero\n \n");
        }
		else if(fabs(lados[0] - lados[1]) < .0001 || fabs(lados[0] - lados[2]) < .0001)
        {
            printf("Triangulo isosceles\n \n");
        }
		else
        {
            printf("Es un triangulo escaleno\n");
        }
	}
	else
	{
		printf("NO es un triangulo \n \n");
	}
}

void diez_ocho()
{
    int inicio = 0 ;
	for(inicio; inicio <= 256; inicio++)
    {
        printf("El caracter '%c' en ASCII se corresponde con el entero %d\n \n", inicio, inicio);
    }
}

void diez_nueve()
{
    int dia, mes, anio;
    printf("Digite el entero para dia: \n");
    scanf("%d", &dia);
    printf("Digite el entero para mes: \n");
    scanf("%d", &mes);
    printf("Digite el entero para año: \n");
    scanf("%d", &anio);
    if((dia < 1 || dia > 31) || (mes < 1 || mes > 12) || (anio < 1))
    {
        printf("Fecha no valida");
    }
    else
    {
        if(mes == 2 && dia > 29)
        {
            printf("Fecha invalida!");
        }
        else if(mes == 2 && dia == 29 && (anio%4 != 0) )            //checamos años bisiestos
        {
            printf("Fecha invalida");
        }
        else if((mes == 4 || mes == 6 || mes == 9 || mes == 11) && dia > 30)
        {
           printf("Fecha invalida");
        }
        else
        {
            printf("Fecha valida.");
        }
    }
    printf("\n");
}


void veinte()
{
    float base = 75.1;                                                  //suponemos un salario base del salario minimo :(
    float horasFijas = 48, trabajadas, extra = 20;
	float costo_mas = 7, salario, mas_extra;
	printf("Digite las horas trabajadas: ");
	scanf("%f", &trabajadas);

	if(trabajadas <= 48)                                                //el caso base
		salario = base;
	else
	{
		mas_extra = trabajadas - horasFijas - extra;
		if( mas_extra < 0)
        {
			salario = base + costo_mas * (trabajadas - horasFijas);
        }
		else
        {
            salario = base + (costo_mas * extra) + (2 * costo_mas * mas_extra);
        }
	}
	printf("El salario bruto es $%f dolares.\n\n", salario);
}

void veinte_uno()
{
    int semilla, filas, columnas, f1 = 0, f2 = 0, c1 = 6, c2 = 6 ;
    printf("Digite (entero) semilla de inicio:\n");
    scanf("%d", &semilla);
    f2 = f1= semilla;
    for(filas = 1 ; filas <= 6; filas++)                        //Primero se dibuja la parte superior del diamente simulando dos putntos que se mueven
    {
        for (columnas = 1; columnas <= 11; columnas++)
            {
                if ( ((filas == semilla  ) && (columnas == 6 ) )  )         //Dibujo dos puntos encimados que se mueven en direcciones diferentes
                {
                    printf("*");
                    f1++;
                    c1--;
                    f2++;
                    c2++;
                }
                if ( ((filas == f1 ) && (columnas == c1 ) )  )              // Primer punto desplazandose
                {
                    printf("*");
                    f1++;
                    c1--;
                }
                else if ( ((filas == f2 ) && (columnas == c2 ) )  )         //Segundo punto desplazandose
                {
                    printf("*");
                    f2++;
                    c2++;
                }
                else{
                    printf("o");
                }

            }
            printf("\n");
    }
    f2--;
    c2--;
    f1 = 6;
    c1 = 1;
    for(filas = 6 ; filas <= 11; filas++)                        //Posteriormente se dibuja la parte inferior del diamente simulando dos putntos que se mueven
    {
        for (columnas = 1; columnas <= 11; columnas++)
            {
                if ( ((filas == f1 ) && (columnas == c1 ) )  )              // Primer punto desplazandose
                {
                    printf("*");
                    f1++;
                    c1++;
                }
                else if ( ((filas == f2 ) && (columnas == c2 ) )  )         //Segundo punto desplazandose
                {
                    printf("*");
                    f2++;
                    c2--;
                }
                else{
                    printf("o");
                }

            }
            printf("\n");
    }
}

char *inputString(FILE* fp, size_t size){
//The size is extended by the input with the value of the provisional
    char *str;
    int ch;
    size_t len = 0;
    str = realloc(NULL, sizeof(char)*size);//size is start size
    if(!str)return str;
    while(EOF!=(ch=fgetc(fp)) && ch != '\n'){
        str[len++]=ch;
        if(len==size){
            str = realloc(str, sizeof(char)*(size+=16));
            if(!str)return str;
        }
    }
    str[len++]='\0';

    return realloc(str, sizeof(char)*len);
}

void once_2()                                               // Se implemnta el algoritmo de Knuth Morris Pratt
{
    char *texto, *padron;
    int salida, n, m, i, q, k , *pi ;
    printf("Ingrese padron a buscar cadena:\n");            // Se llen dinamicamente las cadenas
    getchar();
    padron =  inputString(stdin, 10);
    printf("Ingrese texto en el cual buscar :\n");
    texto = inputString(stdin, 10);
    printf("texto: %s\n", texto);
    printf("padron: %s\n", padron);
    printf("tamanio texto %d, tamanio padron %d", strlen(texto), strlen(padron));
    // bacbababaabcbab
    // ababaca
    pi = (int) malloc(sizeof(int) * strlen(padron));
    m = strlen(padron);
    n = strlen(padron);
    k = -1;
    pi[0] = -1;
    for (q = 1 ; q < m; q++)                                 // se preproceda para encontrar los indices de repeticion del texto a buscar en si mismo
    {
        while( (k > -1) && (padron[k+1]!=padron[q]))
        {
            k = pi[k];
        }

        if(padron[k + 1] == padron[q])
        {
            k = k+1;
        }
        pi[q] = k;

    }
    k = -1;
    for (i = 0;i < n; i++)                                  // utilizando los indices validos del paso anterior se buscan el 'adron' en el 'texto'
    {
        while( k > -1 && padron[k+1] != texto[i])
        {
            k = pi[k];
        }
        if(texto[i] == padron[k+1])
        {
            k++;
        }
        if(k==(m-1))
        {
            salida = i-k;
        }
    }
    if( i >= 0)
    {
        printf("\nEl padron es subcadena del texto: %s", texto+salida);
    }
    else
    {
        printf("\nEl padron NO ES subcadena del texto: %s", texto+salida);
    }

}

void quince()
{
    char *numero1, *numero2, a;
    int n1 = 0, n2 = 0, i = 0 ;
    printf("Ingrese primer numero romano usando los caracteres 'I,V,X,L,C,D,M':\n");            // Se llen dinamicamente las cadenas
    getchar();
    numero1 =  inputString(stdin, 10);
    printf("Ingrese segundo numero :\n");
    getchar();
    numero2 = inputString(stdin, 10);
    //CAST DE PRIMER NUMERO
    for (i = 0; i < (strlen(numero1) - 1); i++)                 // se supone que el numero romano es valido
    {
        if( numero1[i] == 'I' )                                 //se buscan restas
        {

            if(numero1[i + 1] == 'V')
            {
                numero1[i] =numero1[i + 1] = ' ';
                n1+=4;
            }
            if(numero1[i + 1] == 'X')
            {
                numero1[i] =numero1[i + 1] = ' ';
                n1+=9;
            }
        }
         else if( numero1[i] == 'X' )                                 //se buscan restas
        {
            if(numero1[i + 1] == 'L')
            {
                numero1[i] =numero1[i + 1] = ' ';
                n1+=40;
            }
            if(numero1[i + 1] == 'C')
            {
                numero1[i] =numero1[i + 1] = ' ';
                n1+=90;
            }
        }
                 else if( numero1[i] == 'C' )                                 //se buscan restas
        {
            if(numero1[i + 1] == 'D')
            {
                numero1[i] =numero1[i + 1] = ' ';
                n1+=400;
            }
            if(numero1[i + 1] == 'M')
            {
                numero1[i] =numero1[i + 1] = ' ';
                n1+=900;
            }
        }

    }
    for (i = 0; i < (strlen(numero1) - 1); i++)                 // se supone que el numero romano es valido
    {
        if(numero1[i] == 'I')
        {
            printf("\n%c", numero1[i]);
            n1+=1;
            numero1[i]=' ';
        }
        else if(numero1[i] == 'V')
        {
            n1+=5;
            numero1[i]=' ';
        }
                else if(numero1[i] == 'X')
        {
            n1+=10;
            numero1[i]=' ';
        }
                else if(numero1[i] == 'L')
        {
            n1+=50;
            numero1[i]=' ';
        }
        else if(numero1[i] == 'C')
        {
            n1+=100;
            numero1[i]=' ';
        }
        else if(numero1[i] == 'D')
        {
            n1+=500;
            numero1[i]=' ';
        }
                else if(numero1[i] == 'M')
        {
            n1+=1000;
            numero1[i]=' ';
        }
    }
    // CAST DE SEGUNDO NUMERO
     for (i = 0; i < (strlen(numero2) - 1); i++)                 // se supone que el numero romano es valido
    {
        if( numero2[i] == 'I' )                                 //se buscan restas
        {

            if(numero2[i + 1] == 'V')
            {
                numero2[i] =numero2[i + 1] = ' ';
                n2+=4;
            }
            if(numero2[i + 1] == 'X')
            {
                numero2[i] =numero2[i + 1] = ' ';
                n2+=9;
            }
        }
         else if( numero2[i] == 'X' )                                 //se buscan restas
        {
            if(numero2[i + 1] == 'L')
            {
                numero2[i] =numero2[i + 1] = ' ';
                n2+=40;
            }
            if(numero2[i + 1] == 'C')
            {
                numero2[i] =numero2[i + 1] = ' ';
                n2+=90;
            }
        }
                 else if( numero2[i] == 'C' )                                 //se buscan restas
        {
            if(numero2[i + 1] == 'D')
            {
                numero2[i] =numero2[i + 1] = ' ';
                n2+=400;
            }
            if(numero2[i + 1] == 'M')
            {
                numero2[i] =numero2[i + 1] = ' ';
                n2+=900;
            }
        }

    }
    for (i = 0; i < (strlen(numero2) - 1); i++)                 // se supone que el numero romano es valido
    {
        if(numero2[i] == 'I')
        {
            n2+=1;
            numero2[i]=' ';
        }
        else if(numero2[i] == 'V')
        {
            n2+=5;
            numero2[i]=' ';
        }
                else if(numero2[i] == 'X')
        {
            n2+=10;
            numero2[i]=' ';
        }
                else if(numero2[i] == 'L')
        {
            n2+=50;
            numero2[i]=' ';
        }
        else if(numero2[i] == 'C')
        {
            n2+=100;
            numero2[i]=' ';
        }
        else if(numero2[i] == 'D')
        {
            n2+=500;
            numero2[i]=' ';
        }
                else if(numero2[i] == 'M')
        {
            n2+=1000;
            numero2[i]=' ';
        }
    }
    printf("\n pRImer numero %d: %s", n1, numero1);
    printf("\n  segundo numero %d: %s", n2, numero2);

    printf("\nLa suma de los numeros es %d", n1+n2);
    printf("\nLa diferencia de los numeros es %d", n1-n2);
    printf("\nEl producto de los numeros es %d", n1*n2);
    printf("\nLa división de los numeros es %d", n1/n2);




    }



void main()
{
    int opcion;                                               // Se construye el menu
    printf("Tarea 3: Tester\n Elija un numero para ver la opcion correpondiente al inciso de la tarea: \n\n#####################################################################\n\n\n");
    printf("1 ) Programa que ingrese dos enteros y cambiar sus valores. Imprima valores originales y los  modificados.\n \n");
    printf("2 ) Programa que suma dos numeros binarios e imprima el resultado asi como la conversion a decimal del resultado (suponga binarios de 8 bits)\n \n");
    printf("3 ) Programa para leer dos cadenas y concatenar las cadenas.\n \n");
    printf("4 ) Un programa que verifique las reglas de prioridad evaluacion de expresiones a igualdad de prioridades, en el caso de parentesis. Comprobar que la evaluacion se efectua de izquierda a derecha.  (explique procedimiento y resultado, dentro del programa)\n \n");
    printf("5 ) Programa para convertir octal a binario.\n \n");
    printf("6 ) Programa para comprobar los resultados de una confusion entre el operador '=' y el operador '==' (explique procedimiento y resultado, dentro del programa)\n \n ");
    printf("7 ) Programa que comprueban si la cadena dada es un Palindromo\n \n ");
    printf("8 ) Programa para invertir cada palabra de la cadena dada.\n \n");
    printf("9 ) Comprobar si un determinado número es Armstrong.\n \n");
    //printf("10 )  ");
    printf("11 ) Programa para  comprobar si la subcadena esta presente en una cadena. Ingresar 2 cadenas y realizar a busqueda de la primera en la segunda.\n \n");
    printf("12 ) Solicitar un numero entero, calcula el Cuadrado y el Cubo de los 5 siguientes numeros.\n \n");
    printf("13 ) Programa que calcula la suma de los dígitos en un entero dado, debera sumar todos los digitos del entero dado e imprimir el numero dado y el resultado\n \n");
    printf("14 ) Solicitar un dia de la semana y que nos diga si es un dia laboral o no (semana laborar de lunes-viernes). Usa 2 diferentes estructuras secuenciales.\n \n");
    //printf("15 ) ");
    printf("16 ) Encontrar la suma de los primeros 50 Numeros naturales usando For. \n \n");
    printf("17 ) Solicitar tres numeros e indique el tipo de triangulo que forman (isosceles, equilatero, escaleno). Comprobar que los numeros realmente formen un triangulo, sino emitir el error.\n \n");
    printf("18 ) Que imprima todo el codigo ASCII y su equivalente en caracter (el codigo ASCII va de 0 a 255 y representa el numero con el que los ordenadores almacenan los caracteres).\n \n");
    printf("19 ) Solicitar 3 numeros los cuales significan una fecha (dia, mes, año). Comprobar que sea valida la fecha, si no es valido que imprima un mensaje de error, y si es valida imprimir el mes con su nombre.\n \n");
    printf("20 ) Calcular el salario semanal de los empleados, cada hora extra se debera pagar 7 Dolar, si las horas extras no superen las 20 horas. Por cada hora por encima de 20 se debe pagar 14 dolar.\n \n  ");
    printf("21 ) Programa para imprimir un padron de diamante, donde el usuario ingrese el numero de fila a realizar el diamante.\n \n ");
    printf("\n \n \nDigite su eleccion:\n");
    fflush(stdin);
    scanf("%d", &opcion);
                                                            // se matchea la opcion
    switch(opcion)
    {
        case 1:
            primero();
        case 2:
            segundo();
        case 3:
            tercero();
        case 4:
            cuarto();
        case 5:
            quinto();
        case 6:
            sexto();
        case 7:
            septimo();
        case 8:
            octavo();
        case 9:
            noveno();
        case 12:
            doceavo();
        case 13:
            trece();
        case 14:
            catorce();
        case 16:
            diez_seis();
        case 17:
            diez_siete();
        case 18:
            diez_ocho();
        case 19:
            diez_nueve();
        case 20:
            veinte();
        case 21:
            veinte_uno();
        case 11:
            once_2();
        case 15:
            quince();


    }

}


