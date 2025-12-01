Program I3DSimpsonTrapz
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
call int3DSimpson(X1,X2,S1)
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
call int3DTrapz(X1,X2,S2)
ERROR2 = ABS(EXATA - S2)/EXATA*100
Write(13,*)"Valor numerico pela Integral Trapezio:",S2
Write(13,*)"Valor Exato:", EXATA
Write(13,*)"ERRO RELATIVO", ERROR2, "%"
write(13,*)"Reparticoes (N):", N
call itime(ICLOCK)
write(13,*)"hora final",ICLOCK(1),ICLOCK(2),ICLOCK(3)
end program

Subroutine int3DSimpson(x1,x2,S1)
real s1,x1,x2,h
external h
call simpson(h,x1,x2,S1)
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

Subroutine int3Dtrapz(X1,X2,S2)
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

real function z1(x,y)
COMMON/xrange/x1,x2
real x, y
z1 = 
end

real function z2(x,y)
COMMON/xrange/x1,x2
real x, y
z2 = 
end

real function y1(x)
COMMON/xrange/x1,x2
real x
y1 = 
end

real function y2(x)
COMMON/xrange/x1,x2
real x
y2 = 
end

function h(xx)
real h, xx, k, y1, y2, s1
external k
COMMON/xyz/x,y,z
x = xx
call simpson(k, y1(x), y2(x), s1)
h = s1
return
end

function g(xx)
real g, xx, j, y1, y2, s2
external j
COMMON/xyz/x,y,z
x = xx
call trapz(j, y1(x), y2(x), s2)
g = s2
return
end

function k(yy)
real k, yy, f, z1, z2, s1
external f
COMMON/xyz/x,y,z
y = yy
call simpson(f, z1(x,y), z2(x,y), s1)
k = s1
return
end

function j(yy)
real j, yy, f, s2
external f
COMMON/xyz/x,y,z
y = yy
call trapz(f, z1(x,y), z2(x,y), s2)
j = s2
return
end

function f(zz)
real f, zz, x, y, z
COMMON/xyz/x,y,z
z = zz
f = func(x,y,z)
return
end

real function func(x,y,z)
real x,y,z
func = 
return
end
