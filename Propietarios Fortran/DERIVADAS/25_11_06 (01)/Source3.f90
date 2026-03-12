program principal
implicit none
real*8 x, h, func, reg2, prog2, pont3, pont5
open(13, file="resultados.txt")
x = 0.5

h = 0.1
write(13,*) reg2(x,h) , prog2(x,h), pont3(x,h),  pont5(x,h)
h = 0.01
write(13,*) reg2(x,h) , prog2(x,h), pont3(x,h),  pont5(x,h)
h = 0.001
write(13,*) reg2(x,h) , prog2(x,h), pont3(x,h),  pont5(x,h)
h = 0.0001
write(13,*) reg2(x,h) , prog2(x,h), pont3(x,h),  pont5(x,h)
h = 0.00001
write(13,*) reg2(x,h) , prog2(x,h), pont3(x,h),  pont5(x,h)



close(13)
end program

real*8 function func(x)
       real*8 x
       func = sin(x)
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
       pont3 = (func(x+h)-func(x-h))/(2*h)
       return
       end
       
real*8 function pont5(x,h)
       real*8 x
       real*8 h
       pont5 = (func(x-(2*h))-8*func(x-h)+8*func(x+h)-func(x+(2*h)))/(12*h)
       return
       end
