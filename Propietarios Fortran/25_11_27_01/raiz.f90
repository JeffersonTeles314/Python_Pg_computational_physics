program raiz
implicit none
! Definindo as Vari veis
real*8 idade
integer i
dimension idade(0:100)
open(20,file="idade.txt")

idade(1) = 30.4d0
idade(2) = 36.7d0
idade(3) = 38.9d0
idade(4) = 43.7d0
idade(5) = 51.3d0


do i=1,5
   write(20,*) idade(i)
end do
close(20)
end program
