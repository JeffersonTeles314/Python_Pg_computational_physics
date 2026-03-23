! INTEGRAIS - Normalisação de uma função de onda do Poço de Potencial Finito
! FUNÇÃO DE ONDA PAR - Use a regra de Simpson para calcular a integral ∫ ψ^2 dx, onde ψ é a função de onda par do poço de potencial finito. Use h = 0,01 e depois h = 0,001. Compare seu resultado numérico usando a regra de Simpson com o valor exato e com o valor numérico usando a regra do trapézio. O valor exato é 1.

module meu_modulo
  implicit none
  real*8, parameter :: hbar2_2m = 7.6199682d0
  real*8, parameter :: e_1 = 0.71545249748560857
  real*8, parameter :: e_2 = 2.8213918647369041
  real*8, parameter :: e_3 = 6.1486373402178058
contains

    real*8 function func(cons_d,cons_f, var_a, var_alfa, var_beta, x)
        real*8, intent(in) :: x
        real*8, intent(in) :: cons_d, cons_f, var_a, var_alfa, var_beta
                if ( x < -var_a) then
                    func = (cons_f * 2.718281828d0**(x*var_beta))**2.0d0
                else if (x >= -var_a .and. x <= var_a) then
                    func = (cons_d * dcos(var_alfa * x))**2.0d0
                else if (x > var_a) then
                    func = (cons_f * 2.718281828d0**(-x*var_beta))**2.0d0
                end if
    end function

    real*8 function alfa(e)
        real*8, intent(in) :: e
                alfa = dsqrt((2.0d0 * e)/hbar2_2m)
    end function

    real*8 function beta(e, v0)
        real*8, intent(in) :: e, v0
                beta= dsqrt((2.0d0 * (v0 - e))/hbar2_2m)
    end function      

end module meu_modulo

program edos
    use meu_modulo
    implicit none
    real*8 :: sum, fator, integral, x
    integer :: n , i
    real*8 :: var_alfa, var_beta, var_a, v0
    real*8 :: cons_d, cons_f
    ! Parâmetros para a integração
    real*8 :: inicio, fim, h
    inicio = -6.0d0
    fim = 6.0d0
    h = 0.001d0
    n = 12000
    
    ! Parâmetros para a normalização da função de onda
    v0 = 10.0d0
    var_a = 3.0d0
    var_alfa = alfa(e_1)
    var_beta = beta(e_1, v0)


    print *, "Alfa: ", var_alfa
    print *, "Beta: ", var_beta


    cons_d = 1.0d0 / dsqrt((1.0d0/var_beta) + var_a)
    cons_f =  (2.718281828d0**(var_beta * var_a)) * dcos(var_alfa * var_a) * cons_d

    print *, "Constante D: ", cons_d
    print *, "Constante F: ", cons_f
    
    open(13, file="results.txt")
    ! =====LOOP BISSEÇÃO=====

    if(mod(n,2).EQ.0)then
    fator = 2.0d0
    sum = func(cons_d, cons_f, var_a, var_alfa, var_beta, inicio) + func(cons_d, cons_f, var_a, var_alfa, var_beta, fim)
    do i = 1, n-1
       x = inicio +i*h
       if(fator.EQ.2.0d0)then
           fator = 4.0d0
       else
           fator = 2.0d0
       endif
       sum = sum +fator*func(cons_d, cons_f, var_a, var_alfa, var_beta, x)
    end do
       integral = (sum*h)/3.d0
       write(13,*) integral
       close(13)
   endif
end program
