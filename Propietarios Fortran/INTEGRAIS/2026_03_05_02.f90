! INTEGRAIS - Método de Simpson
! Use a regra de Simpson para calcular a integral ∫ e^x dx. Use h = 0,01 e depois
! h = 0,001. Compare seu resultado numérico usando a regra de Simpson com o valor
! exato e com o valor numérico usando a regra do trapézio. O valor exato é = e^4 - e^2.

module meu_modulo
  implicit none
  real*8, parameter :: hbar2_2m = 7.6199682d0
contains

    real*8 function func(x)
        real*8, intent(in) :: x
                func= 2.718281828d0**x
    end function

end module meu_modulo

program edos
    use meu_modulo
    implicit none

    real*8 :: a, b, h
    real*8 :: sum, fator, integral, x
    integer :: n , i
    
    a = 2.0d0
    b = 4.0d0
    h =0.01d0
    n = 200
    
    open(13, file="results.txt")
    ! =====LOOP BISSEÇÃO=====

    if(mod(n,2).EQ.0)then
    fator = 2.0d0
    sum = func(a) + func(b)
    do i = 1, n-1
       x = a +i*h
       if(fator.EQ.2.0d0)then
           fator = 4.0d0
       else
           fator = 2.0d0
       endif
       sum = sum +fator*func(x)
    end do
       integral = (sum*h)/3.d0
       write(13,*) integral
       close(13)
   endif
end program
