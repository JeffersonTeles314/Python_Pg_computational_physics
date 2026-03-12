! Calcual nos pontos x2 = 2.2 e x3 = 3.3, o valor
! da fun‡Æo f(x) = x^2 + sin(x)
program principal
implicit none
real*8 x2, x3,funcao
open(13, file="funcao.txt")
x2 = 2.2d0
x3 = 3.3d0
write(13,*) funcao(x2) , funcao(x3)
close(13)
end program

real*8 function funcao(x)
       real*8 x
       funcao = x**2+dsin(x)
       return
       end
