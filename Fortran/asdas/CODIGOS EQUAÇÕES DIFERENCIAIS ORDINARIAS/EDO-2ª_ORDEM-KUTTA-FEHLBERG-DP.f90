program EDO2EULER
PARAMETER (NNN = 100000)
implicit real*8 (a-h,o-z)
PARAMETER (PI = ACOS(-1.0))
real*8 L0,L1,L2,L3,L4,L5,K0,K1,K2,K3,K4,K5,DIFF,EXATA,X
Dimension Y(0:NNN)
Dimension Z(0:NNN)
open(14,file="dados-13do03exerc02.txt")
open(15,file="erro-13do03exerc02.txt")
H = 0.001d0
NSTEP = int(--/H)
Y(0) = --.d0
Z(0) = --.d0
DO 10 IX = 0, NSTEP-1
   X = IX*H
   K0 = H*Z(IX)
   L0 = H*FUNC(X,Y(IX),Z(IX))
   K1 = H*(Z(IX) + L0/4.d0)
   L1 = H*FUNC((X + H/4.d0),(Y(IX) + K0/4.d0),(Z(IX) + L0/4.d0))
   K2 = H*(Z(IX) + 3.d0*L0/32.d0 + 9.d0*L1/32.d0)
   L2 = H*FUNC((X + 3.d0*H/8.d0),(Y(IX) + 3.d0*K0/32.d0 + 9.d0*K1/32.d0),(Z(IX) + 3.d0*L0/32.d0 + 9.d0*L1/32.d0))
   K3 = H*(Z(IX) + 1932.d0*L0/2197.d0 - 7200.d0*L1/2197.d0 + 7296.d0*L2/2197.d0)
   L3 = H*FUNC((X + 12.d0*H/13.d0),(Y(IX) + 1932.d0*K0/2197.d0 - 7200.d0*K1/2197.d0 + 7296.d0*K2/2197.d0),&
   (Z(IX) + 1932.d0*L0/2197.d0 - 7200.d0*L1/2197.d0 + 7296.d0*L2/2197.d0))
   K4 = H*(Z(IX) + 439.d0*L0/216.d0 - 8.d0*L1 + 3680.d0*L2/513.d0 - 845.d0*L3/4104.d0)
   L4 = H*FUNC((X + H),(Y(IX) + 439.d0*K0/216.d0 - 8.d0*K1 + 3680.d0*K2/513.d0 - 845.d0*K3/4104.d0),&
   (Z(IX) + 439.d0*L0/216.d0 - 8.d0*L1 + 3680.d0*L2/513.d0 - 845.d0*L3/4104.d0))
   K5 = H*(Z(IX) - 8.d0*L0/27.d0 + 2.d0*L1 - 3544.d0*L2/2565.d0 + 1859.d0*L3/4104.d0 - 11.d0*L4/40.d0)
   L5 = H*FUNC((X + H/2.d0),(Y(IX) - 8.d0*K0/27.d0 + 2.d0*K1 - 3544.d0*K2/2565.d0 + 1859.d0*K3/4104.d0 - 11.d0*K4/40.d0),&
   (Z(IX) - 8.d0*L0/27.d0 + 2.d0*L1 - 3544.d0*L2/2565.d0 + 1859.d0*L3/4104.d0 - 11.d0*L4/40.d0))
   Y(IX+1) = Y(IX) + (25.0d0/216.0d0*K0 + 1408.0d0/2565.0d0*K2 + 2197.0d0/4104.0d0*K3 - 1.0d0/5.0d0*K4)
   Z(IX+1) = Z(IX) + (25.0d0/216.0d0*L0 + 1408.0d0/2565.0d0*L2 + 2197.0d0/4104.0d0*L3 - 1.0d0/5.0d0*L4)
   DIFF = EXATA(X+H)-Y(IX+1)
   erro = dabs(DIFF/EXATA(X+H))
   write(14,*)X+H,Y(IX+1),EXATA(X+H)
   WRITE(15,*)erro
10 CONTINUE
end

real*8 function FUNC(X,Y,Z)
     implicit real*8 (a-h,o-z)
     FUNC = --
end

real*8 function exata(x)
     implicit real*8 (a-h,o-z)
     PARAMETER (PI = ACOS(-1.0))
     exata = --
end
