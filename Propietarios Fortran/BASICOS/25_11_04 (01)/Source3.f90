! Crie um Programa, que se ultilisa dos Dupla Precis∆o.

program precise
real x
real*8 y
open(11, file="escreve.txt")
x = 1.0
y = 1.0d0
do i=1,1000000
   x = x + 0.000001
   y = y + 0.000001d0
end do
! printando na tela !
write(*,*) x
write(*,*) y
pause
! printando na no Arquivo !
write(11,*) x
write(11,*) y
close(11)
end program
