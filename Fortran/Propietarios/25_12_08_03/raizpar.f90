program graphraiz
implicit none
! Definidno as Fun‡äes
real*8 acha_raiz,f_impar,f_par, alfa, beta
! Definindo as Vari veis
real*8 x, delta, inicial, final, dx,hquadcort, e,m, vzero
integer N, i

N = 1000
inicial = 2.5d0
final = 4.5d0

open(11,file="graphpar.txt")
dx = (final-inicial)/N

do i = 0,N
   x = inicial + (dx * i)
   write(11,*) x, acha_raiz(x)
end do
close(11)
end program



! Fun‡Æo usando o M‚todo da Secante
real*8 function acha_raiz(a)
real*8 a, TOL, xvals, varaux,varaux1, varaux2,erro
integer icont

dimension xvals(0:100)
TOL = 1.0d-08
xvals(0) = 2.0d0
xvals(1) = 2.1d0
icont = 1 !Contador
100 continue
varaux1 = (xvals(icont)-xvals(icont-1))
varaux2 = (f_par(xvals(icont),a) - f_par(xvals(icont-1),a))
varaux = f_par(xvals(icont),a) * (varaux1/varaux2)

xvals(icont + 1) = xvals(icont)- varaux
erro = dabs(varaux/xvals(icont))

icont = icont + 1

if(erro.gt.TOL) goto 100

acha_raiz = xvals(icont)
return
end

real*8 function f_par(e,a)
real*8 e
real*8 a
real*8 vzero
real*8 m
real*8 aux1
real*8 aux2
m = 1.0d0
vzero = 10.0d0
aux1 = beta(vzero,m,e)*dcos(alfa(e,m)*a)
aux2 = alfa(e,m)*dsin(alfa(e,m)*a)
f_par = aux1 - aux2
return
end

real*8 function f_impar(e,a)
real*8 e
real*8 a
real*8 vzero
real*8 m
real*8 aux1
real*8 aux2
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
hquadcort = 7.6199682d0
alfa = dsqrt((2.0d0*m*e)/hquadcort)
return
end

real*8 function  beta(vzero,m,e)
real*8 vzero
real*8 m
real*8 e
real*8 hquadcort
hquadcort = 7.6199682d0
beta = dsqrt((2.0d0*m*(vzero-e))/hquadcort)
return
end
