program EDO_EULER
open(14,file="euler-result.txt")
open(15,file="erro-euler.txt")
H = 0.001
NSTEP = --/H
y = --
DO IX = 0, NSTEP-1
   X = IX*H
   Y = Y + H*func(X,Y)
   DIFF = EXATA(X+H)-Y
   erro = abs(DIFF/EXATA(X+H))
   write(14,*)X+H,Y,EXATA(X+H)
   WRITE(15,*)erro
enddo
end

real function func(X,Y)
     func = --
end

real function exata(x)
     exata = --
end
