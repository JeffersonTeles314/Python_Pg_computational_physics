program dervseg
implicit none
! Definindo as Vari veis
real*8 h, x, func, triponts, pentponts
x = 0.5d0
open(13,file="results.txt")

!
h = 0.1d0
write(13,*) triponts(x,h),pentponts(x,h)

!
h = 0.01d0
write(13,*) triponts(x,h),pentponts(x,h)

!
h = 0.001d0
write(13,*) triponts(x,h),pentponts(x,h)

!
h = 0.0001d0
write(13,*) triponts(x,h),pentponts(x,h)

!
h = 0.00001d0
write(13,*) triponts(x,h),pentponts(x,h)

close(13)
end program

real*8 function func(x)
      real*8 x
      func = dsin(x)
      return
      end

real*8 function triponts(x,h)
      real*8 x, h, help
      help = -2d0*func(x)+func(x+h)+func(x-h)
      triponts = help/(h**2d0)
      return
      end

real*8 function pentponts(x,h)
      real*8 x, h ,help
      help = -func(x-2d0*h)+16d0*func(x-h)-30d0*func(x)+16d0*func(x+h)-func(x+2d0*h)
      pentponts = help/(12d0*(h**2d0))
      return
      end


