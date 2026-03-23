! INTEGRAIS - Normalisação de uma função de onda do Poço de Potencial Finito
! Comparando os resultados numéricos usando a regra de Simpson e a regra do trapézio para a normalização da função de onda ímpar do poço de potencial finito. O valor exato é 1. Use h = 0,01 e depois h = 0,001.

module meu_modulo
  implicit none
  real*8, parameter :: hbar2_2m = 7.6199682d0
  real*8, parameter :: e_0 = 0.71545249748560857
  real*8, parameter :: e_1 = 2.8213918647369041
  real*8, parameter :: e_2 = 6.1486373402178058
contains

    real*8 function func_impar(cons_d,cons_f, var_a, var_alfa, var_beta, x)
        real*8, intent(in) :: x
        real*8, intent(in) :: cons_d, cons_f, var_a, var_alfa, var_beta
                if ( x < -var_a) then
                    func_impar = (cons_f * 2.718281828d0**(x*var_beta))**2.0d0
                else if (x >= -var_a .and. x <= var_a) then
                    func_impar = (cons_d * dsin(var_alfa * x))**2.0d0
                else if (x > var_a) then
                    func_impar = (cons_f * 2.718281828d0**(-x*var_beta))**2.0d0
                end if
    end function

    real*8 function func_par(cons_d,cons_f, var_a, var_alfa, var_beta, x)
        real*8, intent(in) :: x
        real*8, intent(in) :: cons_d, cons_f, var_a, var_alfa, var_beta
                if ( x < -var_a) then
                    func_par = (cons_f * 2.718281828d0**(x*var_beta))**2.0d0
                else if (x >= -var_a .and. x <= var_a) then
                    func_par = (cons_d * dcos(var_alfa * x))**2.0d0
                else if (x > var_a) then
                    func_par = (cons_f * 2.718281828d0**(-x*var_beta))**2.0d0
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

    real*8 function volt(x,a)
        real*8, intent(in) :: x, a
        if (abs(x) < a) then
            volt = 0.0d0
        else
            volt = 10.0d0
        end if
    end function

    real*8 function func_f(x,y,z,e,a)
        real*8, intent(in) :: x, y, z, e, a
        func_f = -(2.0d0*(e - volt(x,a))*y)/hbar2_2m
    end function

end module meu_modulo


program edos
    use meu_modulo
    implicit none
    real*8, allocatable :: yvals(:)
    real*8, allocatable :: zvals(:)
    real*8 :: sum, fator, integral
    integer :: n , i
    ! Parâmetros para o potencial de poço quadrado
    real*8 :: e, a, v0
    ! Parâmetros para as funções de onda
    real*8 :: var_alfa, var_beta
    reaL*8 :: cons_d, cons_f_impar, cons_f_par
    real*8 :: x_inicial, x_final, h, x_atual
    real*8 :: k1y, k1z, k2y, k2z, k3y, k3z, k4y, k4z

    
    a = 3.0d0

        x_inicial = -6.0d0
        x_final = 6.0d0
        h = 0.001d0
        n = 12000
        v0 = 10.0d0
        e = e_1
        
        var_alfa = alfa(e)
        var_beta = beta(e, v0)

        cons_d = 1.0d0 / dsqrt((1.0d0/var_beta) + a)
        cons_f_impar =  (2.718281828d0**(var_beta * a)) * dsin(var_alfa * a) * cons_d
        cons_f_par =  (2.718281828d0**(var_beta * a)) * dcos(var_alfa * a) * cons_d


        ! Alocação dinâmica para evitar estouro de memória
        allocate(zvals(0:20000))
        allocate(yvals(0:20000))

        yvals(0) = 0.0d0 ! Condição inicial (y_pvi)
        zvals(0) = 0.0000001d0 ! Condição inicial (z_pvi)

        open(20, file="graph.txt")
        x_atual =  x_inicial
        i = 0
        do i = 0, n
            x_atual =  x_inicial + i*h
            write(*,*)  x_atual, yvals(i)
            write(20,*)  x_atual, yvals(i)
            
            k1y = h * zvals(i)
            k1z = h * func_f(x_atual,yvals(i),zvals(i),e,a)

            k2y = h * (zvals(i) + k1z/2.0d0)
            k2z = h * func_f(x_atual + h/2.0d0 ,yvals(i) + k1y/2.0d0,zvals(i)+ k1z/2.0d0,e,a)

            k3y = h * (zvals(i) + k2z/2.0d0)
            k3z = h * func_f(x_atual + h/2.0d0 ,yvals(i) + k2y/2.0d0,zvals(i)+ k2z/2.0d0,e,a)

            k4y = h * (zvals(i) + k3z)
            k4z = h * func_f(x_atual + h ,yvals(i) + k3y,zvals(i)+ k3z,e,a)

            ! C�lculo do
            yvals(i+1) = yvals(i) + (k1y+2.0d0*k2y+2.0d0*k3y+k4y)/6.0d0
            ! C�lculo do
            zvals(i+1) = zvals(i) + (k1z+2.0d0*k2z+2.0d0*k3z+k4z)/6.0d0

            end do
            close(20)

    if(mod(n,2).EQ.0)then
    fator = 2.0d0
    sum = (yvals(0))**2.0d0 + (yvals(n))**2.0d0
    do i = 1, n-1
       if(fator.EQ.2.0d0)then
           fator = 4.0d0
       else
           fator = 2.0d0
       endif
       sum = sum +fator*(yvals(i))**2.0d0
    end do
       integral = (sum*h)/3.d0

   endif
   
   open(13, file="results.txt")
   do i = 0, n
      x_atual =  x_inicial + i*h
      yvals(i) = yvals(i) / dsqrt(integral)
    ! Calcula a função de onda normalizada e escreve os resultados no arquivo
    ! Comentei a função par, mas é só descomentar para calcular a função de onda par e Comentar a função ímpar
    write(13,*) x_atual , (yvals(i))**2.0d0, func_impar(cons_d, cons_f_impar, a, var_alfa, var_beta, x_atual)
    !   write(13,*) x_atual , (yvals(i))**2.0d0, func_par(cons_d, cons_f_par, a, var_alfa, var_beta, x_atual)
   end do
   close(13)
end program






