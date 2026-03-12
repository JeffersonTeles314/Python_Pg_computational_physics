program EDO2EULER
PARAMETER (NNN = 100000)
implicit real*8 (a-h,o-z)
Dimension Y(0:NNN)
Dimension Z(0:NNN)
open(14,file="dados-euler2nd.txt")
open(15,file="erro-euler2nd.txt")
H = 0.001d0
NSTEP = --/H
y(0) = --.d0
Z(0) = --.d0
DO 10 IX = 0, NSTEP-1
   X = IX*H
   Y(IX + 1) = Y(IX) + H*Z(IX)
   Z(IX + 1) = Z(IX) + H*func(X,Y(IX),Z(IX))
   DIFF = EXATA(X+H)-Y(IX+1)
   erro = dabs(DIFF/EXATA(X+H))
   write(14,*)X+H,Y(IX+1),EXATA(X+H)
   WRITE(15,*)erro
10 CONTINUE
end


real*8 function func(X,Y,Z)
implicit real*8 (a-h,o-z)
     func = --
end

real*8 function exata(x)
implicit real*8 (a-h,o-z)
     exata = --
end

