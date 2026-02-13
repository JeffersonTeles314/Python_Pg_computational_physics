! Escreva e execute um código em FORTRAN que faça a soma A+B e a diferença A-B
! usando uma sub-rotina. Ou seja, você vai enviar para a sub-rotona os valores de A
! e B e a sub-rotina vai lhe retornar a soma e a diferença. Considere A=6 e B=4.

program exemplo
implicit real*8 (a-h,o-z)
open(15, file="results.txt")
a = 4
b = 6
call algeb(a,b,soma,subtracao)
write(15,*) soma, subtracao
end program

subroutine algeb(a,b,som,sub)
implicit real*8(a-h,o-z)
som = a+b
sub = a-b
end
