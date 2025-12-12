program graphraiz
implicit none

! Definindo as Funá‰es
real*8 find_raiz,f_impar,f_par

! Definindo as Vari†veis
real*8 x, TOL, xvals, varaux, varaux1, varaux2, delta,erro, inicial, final, dx,hquadcort, e,m, vzero
integer N, i


open(13,file="results.txt")
N = 1000
inicial = 2.5d0
final = 4.5d0
dx = (final-inicial)/N
do i = 0,N
   x = inicial + (dx * i)
   write(13,*) x, find_raiz(x)
end do
close(13)
end program

! ================================================
real*8 function find_raiz(a)
real*8 tolx, x, func, finicial, dx
integer iter
tolx = 1.d-08
x = 0.6d0

finicial = f_impar(x,a)

dx = 0.5d0 ! Definimos o Passo
iter = 0

10 continue
      iter = iter + 1
      x = x+dx
      if(finicial*f_impar(x,a).LT.0) then ! O Programa Verifica se o Valor de X est† do outro lado da Abssisa
                x = x-dx
                dx = dx/2.d0
      end if
      if(ABS(dx).GT.tolx) goto 10 ! Caso esta Condiá∆o n∆o seja verdadeira o Loop Se Quebra!
find_raiz = x
return
end

! ================================================
real*8 function f_par(e, a)
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

! ================================================
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

! ================================================
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
