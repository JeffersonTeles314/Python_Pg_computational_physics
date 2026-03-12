program EDO_EULER-DP
implicit real*8 (a-h,o-z)
open(14,file="euler-dp-result.txt")
open(15,file="erro-euler-dp.txt")
H = 0.001d0
NSTEP = --/H
y = --
DO IX = 0, NSTEP-1
   X = IX*H
   Y = Y + H*func(X,Y)
   DIFF = EXATA(X+H)-Y
   erro = dabs(DIFF/EXATA(X+H))
   write(14,*)X+H,Y,EXATA(X+H)
   WRITE(15,*)erro
enddo
end

real*8 function func(X,Y)
implicit real*8 (a-h,o-z)
     func = --
end

real*8 function exata(x)
implicit real*8 (a-h,o-z)
     exata = --
end
