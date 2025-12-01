program principal
implicit none
real*8 x, h, func, teoric, reg2, prog2, pont3, pont5
open(13, file="resultados1.txt")
open(11, file="resultados2.txt")
x = 0.5d0

write(11,*) teoric(x)


h = 0.1d0
write(13,*) reg2(x,h) , prog2(x,h), pont3(x,h),  pont5(x,h)
h = 0.01d0
write(13,*) reg2(x,h) , prog2(x,h), pont3(x,h),  pont5(x,h)
h = 0.001d0
write(13,*) reg2(x,h) , prog2(x,h), pont3(x,h),  pont5(x,h)
h = 0.0001d0
write(13,*) reg2(x,h) , prog2(x,h), pont3(x,h),  pont5(x,h)
h = 0.00001d0
write(13,*) reg2(x,h) , prog2(x,h), pont3(x,h),  pont5(x,h)



close(13)
end program

real*8 function func(x)
       real*8 x
       func = (dcosh(x))/(2d0+x+dlog(x))
       return
       end

real*8 function teoric(x)
       real*8 x
       real*8 helper
       helper = ((x+dlog(x)+2d0)*dsinh(x))- (((1d0/x)+1d0)*dcosh(x))
       teoric = helper /((x+dlog(x)+2d0)**2d0)
       return
       end

real*8 function reg2(x,h)
       real*8 x
       real*8 h
       reg2 = (func(x) - func(x-h))/h
       return
       end

real*8 function prog2(x,h)
       real*8 x
       real*8 h
       prog2 = (func(x+h)-func(x))/h
       return
       end
       
real*8 function pont3(x,h)
       real*8 x
       real*8 h
       pont3 = (func(x+h)-func(x-h))/(2d0*h)
       return
       end
       
real*8 function pont5(x,h)
       real*8 x
       real*8 h
       pont5 = (func(x-(2d0*h))-8d0*func(x-h)+8d0*func(x+h)-func(x+(2d0*h)))/(12d0*h)
       return
       end
