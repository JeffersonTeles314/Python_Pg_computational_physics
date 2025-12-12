program raiz
implicit none
! Definindo as Vari veis
real*8 x, fx,f_linha,delta,erro,TOL
integer icont
open(20,file="raiz_newton.txt")
TOL = 1.0d-08
icont = 0 !Contador
x = 0.6d0


100 continue
delta = -fx(x)/f_linha(x)
icont = icont + 1

write(20,*) "icont", icont
x = x + delta

erro = dabs(delta/x)
if(erro.gt.TOL) goto 100
write(20,*) "Raiz Encontrada x= ", x
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
