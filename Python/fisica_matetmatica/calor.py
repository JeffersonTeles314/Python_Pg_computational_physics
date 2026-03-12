import math

inter = int(input("Insira o Número de Iterações:"))
i0 = int(input("Insira o Número Inicial:"))

def edp_calculate(x,t,i0,inter, func):

    soma = 0

    for n in range(i0, inter+i0):

        soma += func(x,t,n)

    return soma


def func1(x,t,n):

    prod1 = (200* (1-((-1)**n)))/((n*math.pi))

    prod2 = math.sin(((n*math.pi*x)/(50)))

    prod3 = math.exp(-0.005*((n**2)*math.pi**2 * t / 2500))

    return prod1*prod2*prod3


print(edp_calculate(25,1800,i0,inter, func1))
