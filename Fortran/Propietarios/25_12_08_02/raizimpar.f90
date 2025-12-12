program graphraiz
implicit none
! Definindo as Vari veis
real*8 x, f_impar,f_par,delta,erro, inicial, final, dx,hquadcort, e,m, vzero
real*8 TOL, xvals, varaux,varaux1, varaux2
integer icont

dimension xvals(0:100)
open(20,file="raiz_impar.txt")
TOL = 1.0d-08
xvals(0) = 1.5d0
xvals(1) = 1.6d0
icont = 1 !Contador
100 continue
varaux1 = (xvals(icont)-xvals(icont-1))
varaux2 = (f_impar(xvals(icont)) - f_impar(xvals(icont-1)))
varaux = f_impar(xvals(icont)) * (varaux1/varaux2)

xvals(icont + 1) = xvals(icont)- varaux
erro = dabs(varaux/xvals(icont))

write(20,*) "icont-1", icont-1, "icont", icont, "icont+1", icont+1
write(20,*) "Valor da Iteracao= ", xvals(icont-1), xvals(icont), xvals(icont+1),varaux


icont = icont + 1

if(erro.gt.TOL) goto 100
write(20,*) "Raiz Encontrada x= ", xvals(icont)

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
