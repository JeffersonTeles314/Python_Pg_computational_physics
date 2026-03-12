program EDO_KUTTA
PARAMETER (NNN = 100000)
real K1, K2, K3, K4
Dimension y(0:NNN)
open(14,file="KUTTA-result.txt")
open(15,file="erro-kutta.txt")
H = 0.001
NSTEP = --/H
y(0) = --
DO IX = 0, NSTEP-1
   X = IX*H
   K1 = H*FUNC(X, Y(IX))
   K2 = H*FUNC((X + 1/2*H), (Y(IX) + 1/2*K1))
   K3 = H*FUNC((X + 1/2*H), (Y(IX) + 1/2*K2))
   K4 = H*FUNC((X + H), (Y(IX) + K3))
   Y(IX+1) = Y(IX) + 1./6.*(K1 + 2*K2 + 2*K3 + K4)
   DIFF = EXATA(X+H)-Y(IX+1)
   erro = abs(DIFF/EXATA(X+H))
   write(14,*)X+H,Y(IX+1),EXATA(X+H)
   WRITE(15,*)erro
enddo
end

real function FUNC(X,Y)
     FUNC = --
end

real function exata(x)
     exata = --
end
