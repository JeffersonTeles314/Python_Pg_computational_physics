program EDO2EULER
PARAMETER (NNN = 100000)
implicit real*8 (a-h,o-z)
PARAMETER (PI = ACOS(-1.0))
real*8 DIFF,EXATA,X
Dimension Y(0:NNN)
Dimension Z(0:NNN)
open(14,file="dados-Verlet-20do03exe01.txt")
open(15,file="erro-Verlet-20do03exe01.txt")
H = 0.001d0
NSTEP = int(--/H)
Y(0) = --.d0
Z(0) = --.d0
Y(1) = Y(0) + H*Z(0) + 0.5d0*H**2*FUNC(X,Y(0))
DO 10 IX = 1, NSTEP-1
   X = IX*H 
   Y(IX+1) = 2.d0*Y(IX) - Y(IX-1) + H**2*FUNC(X,Y(IX))
   DIFF = EXATA(X+H)-Y(IX+1)
   erro = dabs(DIFF/EXATA(X+H))
   write(14,*)X+H,Y(IX+1),EXATA(X+H)
   WRITE(15,*)erro
10 CONTINUE
end

real*8 function FUNC(X,Y)
     implicit real*8 (a-h,o-z)
     FUNC =  --
end

real*8 function exata(x)
     implicit real*8 (a-h,o-z)
     exata =  --
end
