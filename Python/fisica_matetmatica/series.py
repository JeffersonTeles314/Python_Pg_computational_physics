import math



inter = int(input("Insira o Número de Iterações:"))
i = int(input("Insira o Número Inicial:"))

def series_calcuale(i,inter, func):
    soma = 0

    for i in range(i,inter+i):
        soma += func(i)
    return soma
    
func1 = lambda n: 1/(n**2)
func2 = lambda n: (1/5)**n
func3 = lambda n: (math.log(n)/n)

print(series_calcuale(i,inter, func2))