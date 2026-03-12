program kdraiz
implicit none
! Definindo as Vari veis
real*8 PI, x, func,func2, func3, inicial, final, dx
integer N, i


open(13,file="results.txt")
N = 1000
PI=4.D0*DATAN(1.D0)
inicial = 0.0d0
final = 10.0d0
dx = (final-inicial)/N
do i = 0,N
   x = inicial + (dx * i)
   write(13,*) x, func2(x), func3(x)
end do
close(13)
end program

real*8 function func(x)
      real*8 x
      func = dcos(x)-x
      return
      end

real*8 function func2(x)
      real*8 x
      func2 = dcos(x)
      return
      end


real*8 function func3(x)
      real*8 x
      func3 = x
      return
      end
