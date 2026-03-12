program raiz
implicit none
! Definindo as Vari veis
real*8 x, fx,f_linha,delta,erro,TOL
integer icont
open(20,file="raiz_newton.txt")
TOL = 1.0d-08
icont = 0 !Contador
x = 0.167d0


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
fx = (6435.0d0*x**8.0d0 - 12012.0d0*x**6.0d0 + 6930.0d0*x**4.0d0 - 1260.0d0*x**2.0d0 + 35.0d0)/128.0d0
return
end

real*8 function  f_linha(x)
real*8 x
f_linha = (8.0d0*6435.0d0*x**7.0d0 - 6.0d0*12012.0d0*x**5.0d0 + 4.0d0*6930.0d0*x**3.0d0 - 2.0d0*1260.0d0*x)/128.0d0
return
end
