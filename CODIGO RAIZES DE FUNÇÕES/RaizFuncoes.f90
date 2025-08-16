program raiz
open(15,file="raizresultado.txt")
TOLX = 1.E-6
X = .1
FOLD = func(x)
DX = .5
ITER = 0
EXATA =
10 continue
   ITER = ITER + 1
   X = X + DX
   ERROR = ABS(EXATA-X)/EXATA *100
   write(15,*)ITER, X, ERROR,"%"
   if ((FOLD*FUNC(X)) .LT. 0) then
      X = X - DX
      DX = DX/2
   END IF
   if (ABS(DX) .GT. TOLX) GOTO 10
STOP
END

real function func(x)
func =
end
