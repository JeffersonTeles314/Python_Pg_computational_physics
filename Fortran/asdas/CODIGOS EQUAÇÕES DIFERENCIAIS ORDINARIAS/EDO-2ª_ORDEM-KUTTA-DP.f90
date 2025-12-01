program EDOSEGUNDADP
PARAMETER (NNN = 100000)
implicit real*8 (a-h,o-z)
real*8 L1,L2,L3,L4,K1,K2,K3,K4,DIFF,EXATA,X
Dimension Y(0:NNN)
Dimension Z(0:NNN)
open(14,file="dados-kutta-edo2.txt")
open(15,file="erro-kutta-edo2.txt")
H = 0.001d0
NSTEP = --/H
Y(0) = --.d0
Z(0) = --.d0
DO 10 IX = 0, NSTEP-1
   X = IX*H
   K1 = H*Z(IX)
   L1 = H*func(X,Y(IX),Z(IX))
   K2 = H*(Z(IX) + L1/2.0d0)
   L2 = H*func((X + H/2.d0), (Y(IX) + K1/2.d0), (Z(IX) + L1/2.d0))
   K3 = H*(Z(IX) + L2/2.0d0)
   L3 = H*func((X + H/2.d0), (Y(IX) + K2/2.d0), (Z(IX) + L2/2.d0))
   K4 = H*(Z(IX) + L3)
   L4 = H*func((X + H), (Y(IX) + K3), (Z(IX) + L3))
   Y(IX+1) = Y(IX) + (K1 + 2*K2 + 2*K3 + K4)/6.0d0
   Z(IX +1) = Z(IX) + (L1 + 2*L2 + 2*L3 + L4)/6.0d0
   DIFF = EXATA(X+H)-Y(IX+1)
   erro = dabs(DIFF/EXATA(X+H))
   write(14,*)X+H,Y(IX+1),EXATA(X+H)
   write(15,*)erro
10 CONTINUE
end

real*8 function func(X,Y,Z)
implicit real*8 (a-h,o-z)
     func =  -
end

real*8 function exata(x)
implicit real*8 (a-h,o-z)
     exata =  --
end

