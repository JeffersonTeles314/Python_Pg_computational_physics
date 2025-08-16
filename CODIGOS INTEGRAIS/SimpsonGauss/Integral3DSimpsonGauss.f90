Program I3DSimpsonGauss
COMMON/NN/N,NPOINT
COMMON/xrange/x1,x2
PARAMETER (PI = ACOS(-1.0))
Real x1,x2,s1,s2
INTEGER N, NPOINT
dimension ICLOCK(3)
open(13, file="Resultado.txt")
call itime(ICLOCK)
write(13,*)"hora inicial",ICLOCK(1),ICLOCK(2),ICLOCK(3)
N =
NPOINT = 
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
call int3DGauss(X1,X2,NPOINT,S2)
ERROR2 = ABS(EXATA - S2)/EXATA*100
Write(13,*)"Valor numerico pela Integral de Gauss-Legendre:",S2
Write(13,*)"Valor Exato:", EXATA
Write(13,*)"ERRO RELATIVO", ERROR2, "%"
write(13,*)"Reparticoes (N):", NPOINT
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

SUBROUTINE int3DGauss(X1,X2,N,S2)
real g,x1,x2
external g
call xgauleg(g,x1,x2,N,S2)
return
END

SUBROUTINE xgauleg(func,x1,x2,n,S2)
REAL func,S2,xx,x1,x2,x(n),w(n)
INTEGER i
call gauleg(x1,x2,x,w,n)
xx=0.0
do 12 i=1,n
xx=xx+w(i)*func(x(i))
12 continue
s2 = xx
return
END

SUBROUTINE gauleg(x1,x2,x,w,n)
INTEGER n
REAL x1,x2,x(n),w(n)
DOUBLE PRECISION EPS
PARAMETER (EPS=3.d-14)
INTEGER i,j,m
DOUBLE PRECISION p1,p2,p3,pp,xl,xm,z,z1
m=(n+1)/2
xm=0.5d0*(x2+x1)
xl=0.5d0*(x2-x1)
do 12 i=1,m
  z=cos(3.141592654d0*(i-.25d0)/(n+.5d0))
1 continue
  p1=1.d0
  p2=0.d0
do 11 j=1,n
   p3=p2
   p2=p1
   p1=((2.d0*j-1.d0)*z*p2-(j-1.d0)*p3)/j
11 continue
   pp=n*(z*p1-p2)/(z*z-1.d0)
   z1=z
   z=z1-p1/pp
if(abs(z-z1).gt.EPS)goto 1
   x(i)=xm-xl*z
   x(n+1-i)=xm+xl*z
   w(i)=2.d0*xl/((1.d0-z*z)*pp*pp)
   w(n+1-i)=w(i)
12  continue
return
END

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
COMMON/NN/N,NPOINT
COMMON/xyz/x,y,z
x = xx
call xgauleg(j, y1(x), y2(x), NPOINT, s2)
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
COMMON/NN/N,NPOINT
COMMON/xyz/x,y,z
y = yy
call xgauleg(f, z1(x,y), z2(x,y), NPOINT, s2)
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
