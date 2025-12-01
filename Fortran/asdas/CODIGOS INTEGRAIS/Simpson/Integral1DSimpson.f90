Program Int1DSimpson
COMMON/NN/N
COMMON/xrange/x1,x2
PARAMETER (PI = ACOS(-1.0))
Real x1,x2
dimension ICLOCK(3)
open(13, file="Resultado.txt")
call itime(ICLOCK)
write(13,*)"hora inicial",ICLOCK(1),ICLOCK(2),ICLOCK(3)
N =
X1 = 
X2 = 
if(MOD(N,2) .NE. 0) N = N+1
call int1DSimpson(X1,X2,S1)
EXATA = 
ERROR = ABS(EXATA - S1)/EXATA*100
Write(13,*)"Valor numerico pela Integral de Simpson:", S1
Write(13,*)"Valor Exato:", EXATA
Write(13,*)"ERRO RELATIVO", ERROR, "%"
write(13,*)"Reparticoes (N):", N
call itime(ICLOCK)
write(13,*)"hora final",ICLOCK(1),ICLOCK(2),ICLOCK(3)
end program

Subroutine int1DSimpson(x1,x2,S1)
real s1,x1,x2,f
external f
call simpson(f,x1,x2,S1)
return
end

subroutine simpson(func,a,b,S1)
COMMON/NN/N
h = (b-a)/N
soma = func(a)
fator = 2
do i = 1, N-1
    if (fator == 2.) then
        fator = 4
    else
        fator = 2
    end if
    x = a+i*h
    soma = soma + fator*func(x)
enddo
soma = soma + func(b)
S1 = soma * h/3
end

real function func(x)
real x
func = 
return
end
