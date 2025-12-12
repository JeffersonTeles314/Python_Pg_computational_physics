program dervseg
implicit none
! Definindo as Vari†veis
real*8 tolx, x, func, finicial, dx
integer iter


open(13,file="results.txt")
tolx = 1.d-08
x = 0.6d0

finicial = func(x)

dx = 0.5d0 ! Definimos o Passo
iter = 0

10 continue
      iter = iter + 1
      x = x+dx
      if(finicial*func(x).LT.0) then ! O Programa Verifica se o Valor de X est† do outro lado da Abssisa
                x = x-dx
                dx = dx/2.d0
      end if
      if(ABS(dx).GT.tolx) goto 10 ! Caso esta Condiá∆o n∆o seja verdadeira o Loop Se Quebra!
write(13,*) x, iter
close(13)
end program

real*8 function func(x)
      real*8 x
      func = dcos(x)-x
      return
      end


