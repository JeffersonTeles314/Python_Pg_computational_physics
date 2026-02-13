! Use o método de Numerov, analisando o número de nós, para encontrar as
! três menores energias dos estados estacionários de uma partícula em um
! poço quadrado infinito de largura 6 Å. Considere h = 0,01. Coloque uma precisão de 10^(−10).
! Compare os valores que você encontrou com os valores exatos.



module meu_modulo
  implicit none
  real*8, parameter :: hbar2_2m = 7.6199682d0
contains

    real*8 function volt(x,a)
        real*8, intent(in) :: x, a
            volt = 0.0d0
    end function

    real*8 function func_g(x,e,a)
        real*8, intent(in) :: x, e, a
        func_g = (2.0d0*(volt(x,a)-e))/hbar2_2m    !Massa Igual a 1
    end function

    integer*8 function numerov(e,a)
        real*8, intent(in) :: e, a
        real*8 :: x_inicial, x_final, h, x_atual
        real*8, allocatable :: yvals(:)
        real*8 :: k1y, k1z, k2y, k2z, k3y, k3z, k4y, k4z
        real*8 ::  numerador, denominador
        integer ::  i

        numerov = 0
        x_inicial = -3.0d0
        x_final = 3.0d0
        h = 0.01d0

        ! Alocação dinâmica para evitar estouro de memória
        allocate(yvals(0:10000))

        yvals(0) = 0.0d0 ! Condição inicial (y_pvi)
        yvals(1) = 0.0000001d0 ! Condição inicial (y_pvi)

        open(20, file="graph.txt")
        x_atual =  x_inicial + 2.0d0 * h
        i = 1
        do while(x_atual.LE.x_final)

            write(*,*)  x_atual, yvals(i), e

            numerador = 2.00d0 * yvals(i) - yvals(i-1)
            numerador = numerador + (5.00d0 * func_g(x_atual,e,a) * yvals(i) * (h**2.0d0))/6.0d0
            numerador = numerador + (func_g((x_atual - h),e, a) * yvals(i-1) * h**2.0d0)/12.0d0
            denominador = 1 - (func_g(x_atual+h,e,a) * (h**2.0d0))/12.0d0
            ! C�lculo do pr�ximo Valor
            yvals(i+1) = numerador/denominador

            if(yvals(i+1)*yvals(i).LT.0) then
            numerov = numerov + 1
            end if
            x_atual = x_atual + h
            i = i + 1

        end do
        close(20)
    end function

end module meu_modulo

program edos
    use meu_modulo
    implicit none

    real*8 :: e_inicial, delta_e, e_atual ,tol_e, a
    integer :: nn_atual, nn_buscado, bissec_iterations
    
    a = 3.0d0
    e_inicial = 0.1d0
    delta_e = 0.01d0
    tol_e = 0.0000000001d0
    e_atual = e_inicial
    nn_buscado = 3
    bissec_iterations = 0
    
    open(13, file="results.txt")
    ! =====LOOP BISSEÇÃO=====

    10 continue
    e_atual = e_atual + delta_e
    nn_atual = numerov(e_atual,a)

        if(nn_atual.EQ.nn_buscado) then
                write(13,*) 'Valor de e encontrado: ', e_atual
                write(13,*) 'Número de nós: ', nn_atual
                write(13,*) 'Número de iterações da bisseção: ', bissec_iterations
                e_atual = e_atual - delta_e
                delta_e = delta_e/2.d0
        end if
        bissec_iterations = bissec_iterations + 1
        if(ABS(delta_e).GT.tol_e) goto 10
   close(13)
end program
