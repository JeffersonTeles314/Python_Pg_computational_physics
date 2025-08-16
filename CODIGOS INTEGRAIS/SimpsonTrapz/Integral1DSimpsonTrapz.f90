Program I1DSimpsonTrapz
COMMON/NN/N
COMMON/xrange/x1,x2
PARAMETER (PI = ACOS(-1.0))
Real x1,x2,s1,s2
INTEGER N
dimension ICLOCK(3)
open(13, file="Resultado.txt")
call itime(ICLOCK)
write(13,*)"hora inicial",ICLOCK(1),ICLOCK(2),ICLOCK(3)
N =
X1 = 
X2 =  
EXATA =
if(MOD(N,2) .NE. 0) N = N+1
call int1DSimpson(X1,X2,S1)
ERROR1 = ABS(EXATA - S1)/EXATA*100
Write(13,*)"Valor numerico pela Integral de Simpson:", S1
Write(13,*)"Valor Exato:", EXATA
Write(13,*)"ERRO RELATIVO", ERROR1, "%"
write(13,*)"Reparticoes (N):", N
call itime(ICLOCK)
write(13,*)"hora final",ICLOCK(1),ICLOCK(2),ICLOCK(3)
write(13,*)"-------------------------------------------------"
call itime(ICLOCK)
write(13,*)"hora inicial",ICLOCK(1),ICLOCK(2),ICLOCK(3)
call int1DTrapz(X1,X2,S2)
ERROR2 = ABS(EXATA - S2)/EXATA*100
Write(13,*)"Valor numerico pela Integral Trapezio:",S2
Write(13,*)"Valor Exato:", EXATA
Write(13,*)"ERRO RELATIVO", ERROR2, "%"
write(13,*)"Reparticoes (N):", N
call itime(ICLOCK)
write(13,*)"hora final",ICLOCK(1),ICLOCK(2),ICLOCK(3)
end program

Subroutine int1DSimpson(x1,x2,S1)
real s1,x1,x2,func
external func
call simpson(func,x1,x2,S1)
return
end

subroutine simpson(func,a,b,S1)
COMMON/NN/N,NPOINT
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

Subroutine int1Dtrapz(X1,X2,S2)
real S2,x1,x2,h
external h
call trapz(h,x1,x2,S1)
return
end

Subroutine trapz(func,A,B,S2)
Real S2, func, soma
COMMON/NN/N
External func
h = (B-A)/N
soma = 0.0
do i = 1, N-1
soma = soma + func(A+i*h)
enddo
S2 = h/2.0 * (func(A) + 2.0*soma + func(B))
return
end

real function func(x)
real x
func = 
return
end
