Program I1DSimpsonGauss
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
call int1DGauss(X1,X2,NPOINT,S2)
ERROR2 = ABS(EXATA - S2)/EXATA*100
Write(13,*)"Valor numerico pela Integral de Gauss-Legendre:",S2
Write(13,*)"Valor Exato:", EXATA
Write(13,*)"ERRO RELATIVO", ERROR2, "%"
write(13,*)"Reparticoes (N):", NPOINT
call itime(ICLOCK)
write(13,*)"hora final",ICLOCK(1),ICLOCK(2),ICLOCK(3)
end program

Subroutine int1DSimpson(x1,x2,S1)
real x1,x2,func
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

SUBROUTINE int1DGauss(X1,X2,N,S2)
real func,x1,x2
external func
call xgauleg(func,x1,x2,N,S2)
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

real function func(x)
real x
func = 
return
end
