program edos
implicit none

! Definindo as Fun‡äes
real*8 func, exato,kah_1, kah_2so, kah_1so
real*8 euler, rk_so

! Definindo as Vari veis
real*8 x_pvi, y_pvi, x_inicial, x_final, h ,yvals, x_icont
integer i_total, icont
dimension yvals(0:1000)

x_pvi = 0.0d0
y_pvi = 1.0d0

x_inicial = 0.0d0
x_final = 3.0d0
icont = 0 !Contador

h = 0.05d0

i_total = (x_final - x_inicial)/h

open(13,file="results.txt")

yvals(0) = y_pvi

100 continue
x_icont = x_inicial+h*icont
!yvals(icont+1) = euler(x_icont , yvals(icont) , h)
yvals(icont+1) = rk_so(x_icont,yvals(icont),h)

write(13,*) "icont", icont, "x", x_inicial+h*icont, "y", yvals(icont), "exato", exato(x_icont)

icont = icont + 1

if(icont.lt.(i_total+2)) goto 100
close(13)
end program

! ================================================
real*8 function exato(x)
real*8 x
exato = 2.71828d0**(-x**(2.d0)/2.d0)
return
end

! ================================================
real*8 function func(x,y)
real*8 x
real*8 y
func = -x*y
return
end

! ================================================
real*8 function kah_1so(x,y,h)
real*8 x
real*8 y
real*8 h
kah_1so =  h*func(x,y)
return
end

! ================================================
real*8 function kah_2so(x,y,h)
real*8 x
real*8 y
real*8 h
real*8 aux1
real*8 aux2
aux1 = x+h
aux2 = y+kah_1so(x,y,h)
kah_2so = h*func(aux1,aux2)
return
end

! ================================================
real*8 function euler(x,y,h)
real*8 x
real*8 y
real*8 h
euler = h*func(x,y) + y
return
end
! ================================================
real*8 function rk_so(x,y,h)
real*8 x
real*8 y
real*8 h
real*8 aux1
aux1 = kah_1so(x,y,h) + kah_2so(x,y,h)
rk_so = y + (aux1/2.0d0)
return
end

