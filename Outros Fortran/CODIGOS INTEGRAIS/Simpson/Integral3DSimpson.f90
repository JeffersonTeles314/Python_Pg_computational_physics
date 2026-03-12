Program Int3DSimpson
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
call int3DSimpson(X1,X2,S1)
EXATA = 
ERROR = ABS(EXATA - S1)/EXATA*100
Write(13,*)"Valor numerico pela Integral de Simpson:", S1
Write(13,*)"Valor Exato:", EXATA
Write(13,*)"ERRO RELATIVO", ERROR, "%"
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
real h, xx, g, y1, y2, s1
external g
COMMON/xyz/x,y,z
x = xx
call simpson(g, y1(x), y2(x), s1)
h = s1
return
end

function g(yy)
real g, yy, f, z1, z2, s1
external f
COMMON/xyz/x,y,z
y = yy
call simpson(f, z1(x,y), z2(x,y), s1)
g = s1
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