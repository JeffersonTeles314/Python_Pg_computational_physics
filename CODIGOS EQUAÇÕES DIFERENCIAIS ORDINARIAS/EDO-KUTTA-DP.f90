program teste
PARAMETER (NNN = 100000)
implicit real*8 (a-h,o-z)
PARAMETER (PI = ACOS(-1.0))
real*8 K1,K2,K3,K4,DIFF,EXATA,X
Dimension Y(0:NNN)
open(14,file="KUTTA-DP-resulteste.txt")
open(15,file="erro-kutta-DPteste.txt")
H = 0.001d0
NSTEP = 20.d0/H
Y(0) = PI/180.d0
DO 10 IX = 0, NSTEP-1
   X = IX*H
   K1 = H*FUNC(X, Y(IX))
   K2 = H*FUNC((X + H/2.0d0), (Y(IX) + K1/2.0d0))
   K3 = H*FUNC((X + H/2.0d0), (Y(IX) + K2/2.0d0))
   K4 = H*FUNC((X + H), (Y(IX) + K3))
   Y(IX+1) = Y(IX) + (K1 + 2*K2 + 2*K3 + K4)/6.0d0
   DIFF = EXATA(X+H)-Y(IX+1)
   erro = dabs(DIFF/EXATA(X+H))
   write(14,*)X+H,Y(IX+1),EXATA(X+H)
   write(15,*)erro
10 CONTINUE
end

real*8 function FUNC(X,Y)
implicit real*8 (a-h,o-z)
     FUNC = -((9.8d0)/1.5d0)*Y
end

real*8 function exata(x)
implicit real*8 (a-h,o-z)
     exata = (PI/180.d0)*dcos(dsqrt(9.8d0/1.5d0)*x)
end


