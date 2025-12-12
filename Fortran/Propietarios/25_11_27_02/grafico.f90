program graphfunc
implicit none
! Definindo as Vari veis
real*8 x, fx,delta,erro, inicial, final, dx
integer N, i


open(13,file="results.txt")
N = 1000
inicial = 0.0d0
final = 1.0d0
dx = (final-inicial)/N
do i = 0,N
   x = inicial + (dx * i)
   write(13,*) x, fx(x)
end do
close(13)
end program

real*8 function fx(x)
real*8 x
real*8 e
e = 2.718281828
fx = (x**4.0d0-10.0d0*x**2.0)*+e**(-x)+1
return
end

