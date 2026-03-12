program graphraiz
implicit none
! Definindo as Variáveis

real*8 alfa,beta,f_impar, f_par
real*8 x,delta,erro, inicial, final, dx,hquadcort, e,m, vzero
integer N, i

N = 1000
inicial = 0.0d0
final = 10.0d0

open(13,file="resultspar.txt")
dx = (final-inicial)/N

do i = 0,N
   x = inicial + (dx * i)
   write(13,*) x, f_par(x)
end do
close(13)

open(11,file="resultsimpar.txt")
dx = (final-inicial)/N

do i = 0,N
   x = inicial + (dx * i)
   write(11,*) x, f_impar(x)
end do
close(11)
end program

real*8 function f_par(e)
real*8 e
real*8 a
real*8 vzero
real*8 m
real*8 aux1
real*8 aux2
a = 3.0d0
m = 1.0d0
vzero = 10.0d0
aux1 = beta(vzero,m,e)*dcos(alfa(e,m)*a)
aux2 = alfa(e,m)*dsin(alfa(e,m)*a)
f_par = aux1 - aux2
return
end

real*8 function f_impar(e)
real*8 e
real*8 a
real*8 vzero
real*8 m
real*8 aux1
real*8 aux2
a = 3.0d0
m = 1.0d0
vzero = 10.0d0
aux1 = alfa(e,m)*dcos(alfa(e,m)*a)
aux2 = beta(vzero,m,e)*dsin(alfa(e,m)*a)
f_impar = aux1 + aux2
return
end


real*8 function  alfa(e,m)
real*8 m
real*8 e
real*8 hquadcort
hquadcort = 7.619974694d0
alfa = dsqrt((2.0d0*m*e)/hquadcort)
return
end

real*8 function  beta(vzero,m,e)
real*8 vzero
real*8 m
real*8 e
real*8 hquadcort
hquadcort = 7.619974694d0
beta = dsqrt((2.0d0*m*(vzero-e))/hquadcort)
return
end
