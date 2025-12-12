program raiz
implicit none
! Definindo as Vari veis
real*8 x, fx,f_linha,erro,TOL, xvals, varaux,varaux1, varaux2, e
integer icont
dimension xvals(0:100)
open(20,file="raiz_newton.txt")
TOL = 1.0d-08
xvals(0) = 0.20d0
xvals(1) = 0.35d0
icont = 1 !Contador
100 continue
varaux1 = (xvals(icont)-xvals(icont-1))
varaux2 = (fx(xvals(icont)) - fx(xvals(icont-1)))
varaux = fx(xvals(icont)) * (varaux1/varaux2)

xvals(icont + 1) = xvals(icont)- varaux
erro = dabs(varaux/xvals(icont))

write(20,*) "icont-1", icont-1, "icont", icont, "icont+1", icont+1
write(20,*) "Valor da Iteracao= ", xvals(icont-1), xvals(icont), xvals(icont+1),varaux


icont = icont + 1

if(erro.gt.TOL) goto 100
write(20,*) "Raiz Encontrada x= ", xvals(icont)
end program

real*8 function fx(x)
real*8 x, e
e = 2.718281828d0
fx = (x**4.0d0-10.0d0*x**2.0)*+e**(-x)+1
return
end


