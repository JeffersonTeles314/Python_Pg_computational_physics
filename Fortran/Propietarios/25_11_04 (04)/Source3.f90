! Calcula nos pontos x2 = 3.1 e x3 = 4.5, o valor da fun‡Æo
!f(x) = x^3 + cos(x) + ln(x) + vx
program principal
implicit none
real*8 x2, x3,funcao
open(13, file="funcao.txt")
x2 = 3.1d0
x3 = 4.5d0
write(13,*) funcao(x2) , funcao(x3)
close(13)
end program

real*8 function funcao(x)
       real*8 x
       funcao = x**3 + dcos(x)+ dlog(x) + dsqrt(x)
       return
       end
