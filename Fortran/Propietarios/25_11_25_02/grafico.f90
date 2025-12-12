program graphraiz
implicit none
! Definindo as Vari veis
real*8 x, fx,f_linha,delta,erro, inicial, final, dx
integer N, i


open(13,file="results.txt")
N = 1000
inicial = 0.0d0
final = 3.0d0
dx = (final-inicial)/N
do i = 0,N
   x = inicial + (dx * i)
   write(13,*) x, fx(x)
end do
close(13)
end program

real*8 function fx(x)
real*8 x
fx = 3.0d0*x**2.0d0-8.4d0*x+5.88d0
return
end

real*8 function  f_linha(x)
real*8 x
f_linha = 6.0d0*x-8.4d0
return
end
