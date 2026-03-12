Program Int1DTrapz
COMMON/xrange/x1,x2
COMMON/NN/N
PARAMETER (PI = ACOS(-1.0))
Real xmin, xmax, PI
dimension ICLOCK(3)
open(13, file="Resultado.txt")
call itime(ICLOCK)
write(13,*)"hora inicial",ICLOCK(1),ICLOCK(2),ICLOCK(3)
N = 
X1 = 
X2 =  
call int1Dtrapz(X1,X2,S1)
EXATA = 
ERROR = ABS(EXATA - S1)/EXATA*100
Write(13,*)"Valor numerico pela Integral Trapezio:", S1
Write(13,*)"Valor Exato:", EXATA
Write(13,*)"ERRO RELATIVO", ERROR, "%"
write(13,*)"Reparticoes (N):", N
call itime(ICLOCK)
write(13,*)"hora final",ICLOCK(1),ICLOCK(2),ICLOCK(3)
end program

Subroutine int1Dtrapz(X1,X2,S1)
real S1,x1,x2,func
external func
call trapz(func,x1,x2,S1)
return
end

Subroutine trapz(func,A,B,S1)
Real S1, func, soma
COMMON/NN/N
External func
h = (B-A)/N
soma = 0.0
do i = 1, N-1
soma = soma + func(A+i*h)
enddo
s1 = h/2.0 * (func(A) + 2.0*soma + func(B))
return
end

real function func(x)
real x
func = 
return
end
