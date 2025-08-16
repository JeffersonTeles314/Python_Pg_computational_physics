program EDO-KUTTA-FEHLBERG
PARAMETER (NNN = 100000)
implicit real*8 (a-h,o-z)
real*8 K0,K1,K2,K3,K4,K5,DIFF,EXATA,X
Dimension Y(0:NNN)
open(14,file="KUTTA-FEHLBERG-result.txt")
open(15,file="erro-Kutta-Fehlberg.txt")
H = 0.001d0
NSTEP = --/H
Y(0) = --
DO IX = 0, NSTEP-1
   X = IX*H
   K0 = FUNC(X,Y(IX))
   K1 = FUNC((X + H/4.d0),(Y(IX) + H*K0/4.d0))
   K2 = FUNC((X + 3.d0*H/8.d0),(Y(IX) + 3.d0*H*K0/32.d0 + 9.d0*H*K1/32.d0))
   K3 = FUNC((X + 12.d0*H/13.d0),(Y(IX) + 1932.d0*H*K0/2197.d0 - 7200.d0*H*K1/2197.d0 + 7296.d0*H*K2/2197.d0))
   K4 = FUNC((X + H),(Y(IX) + 439.d0*H*K0/216.d0 - 8.d0*H*K1 + 3680.d0*H*K2/513.d0 - 845.d0*H*K3/4104.d0))
   K5 = FUNC((X + H/2.d0),(Y(IX) - 8.d0*H*K0/27.d0 + 2.d0*H*K1 - 3544.d0*H*K2/2565.d0 + 1859.d0*H*K3/4104.d0 - 11.d0*H*K4/40.d0))
   Y(IX+1) = Y(IX) + H*(16.d0*K0/135.d0 + 6656.d0*K2/12825.d0 + 28561.d0*K3/56430.d0 - 9.d0*K4/50.d0 + 2.d0*k5/55.d0)
   DIFF = EXATA(X+H)-Y(IX+1)
   erro = dabs(DIFF/EXATA(X+H))
   write(14,*)X+H,Y(IX+1),EXATA(X+H)
   write(15,*)erro
enddo
end

real*8 function FUNC(X,Y)
implicit real*8 (a-h,o-z)
     FUNC = --
end

real*8 function exata(x)
implicit real*8 (a-h,o-z)
     exata = --
end

