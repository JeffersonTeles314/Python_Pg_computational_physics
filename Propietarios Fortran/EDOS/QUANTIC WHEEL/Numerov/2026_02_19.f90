! Use o método de Numerov, considerando o ponto de união (matching point) em x = −a, para encontrar as três menores energias dos estados estacionários de uma
! partícula em um poço quadrado finito de largura 6 Å e barreira de energia potencial
! V = 10 Ev. Considere h = 0,01. Coloque uma precisão de 10^−10. Compare os valores que você encontrou com os valores exatos.

module meu_modulo
  implicit none
  real*8, parameter :: hbar2_2m = 7.6199682d0
contains

    real*8 function volt(x,a)
        real*8, intent(in) :: x, a
        if (abs(x) < a) then
            volt = 0.0d0
        else
            volt = 10.0d0
        end if
    end function

    real*8 function func_g(x,e,a)
        real*8, intent(in) :: x, e, a
        func_g = (2.0d0*(volt(x,a)-e))/hbar2_2m    !Massa Igual a 1
    end function

     real*8 function numerov_sobe(e,a,x_ini,x_fim)
        real*8, intent(in) :: e, a,x_ini,x_fim
        real*8 :: x_inicial, x_final, h, x_atual
        real*8, allocatable :: yvals(:)
        real*8, allocatable :: zvals(:)
        real*8, parameter :: match = 0.0d0
        real*8 ::  numerador, denominador
        integer ::  i

        x_inicial = x_ini
        x_final = x_fim
        h = 0.01d0
        x_atual = x_inicial


        ! Alocação dinâmica para evitar estouro de memória
        allocate(yvals(0:10000))
        allocate(zvals(0:10000))

        yvals(0) = 0.0d0 ! Condição inicial (y_pvi)
        yvals(1) = 0.0000001d0 ! Condição inicial (y_pvi)
        zvals(0) = 0.0d0
        
        
        open(20, file="graph.txt")
        x_atual =  x_inicial + 2.0d0 * h
        i = 1

        do while (x_atual.LE.match)
            write(*,*)  x_atual, yvals(i), e
            numerador = 2.00d0 * yvals(i) - yvals(i-1)
            numerador = numerador + (5.00d0 * func_g(x_atual,e,a) * yvals(i) * (h**2.0d0))/6.0d0
            numerador = numerador + (func_g((x_atual - h),e, a) * yvals(i-1) * h**2.0d0)/12.0d0
            denominador = 1 - (func_g(x_atual+h,e,a) * (h**2.0d0))/12.0d0
            ! C�lculo do pr�ximo Valor
            yvals(i+1) = numerador/denominador
            zvals(i) = (yvals(i+1) - yvals(i-1))/2*h

            x_atual = x_atual + h
            i = i + 1
        end do

            numerov_sobe = zvals(i-1)/yvals(i-1)
    end function

    real*8 function numerov_desce(e,a,x_ini,x_fim)
        real*8, intent(in) :: e, a,x_ini,x_fim
        real*8 :: x_inicial, x_final, h, x_atual
        real*8, allocatable :: yvals(:)
        real*8, allocatable :: zvals(:)
        real*8, parameter :: match = 0.0d0
        real*8 ::  numerador, denominador
        integer ::  i

        x_inicial = x_ini
        x_final = x_fim
        h = -0.01d0
        ! Alocação dinâmica para evitar estouro de memória
        allocate(yvals(0:10000))
        allocate(zvals(0:10000))
        
        yvals(0) = 0.0d0 ! Condição inicial (y_pvi)
        yvals(1) = 0.0000001d0 ! Condição inicial (y_pvi)
        zvals(0) = 0.0d0

        open(20, file="graph.txt")
        x_atual =  x_inicial + 2.0d0 * h
        i = 1
        do while (x_atual.GE.match)

            write(*,*)  x_atual, yvals(i), e

            numerador = 2.00d0 * yvals(i) - yvals(i-1)
            numerador = numerador + (5.00d0 * func_g(x_atual,e,a) * yvals(i) * (h**2.0d0))/6.0d0
            numerador = numerador + (func_g((x_atual - h),e, a) * yvals(i-1) * h**2.0d0)/12.0d0
            denominador = 1 - (func_g(x_atual+h,e,a) * (h**2.0d0))/12.0d0
            ! C�lculo do pr�ximo Valor
            yvals(i+1) = numerador/denominador
            zvals(i) = (yvals(i+1) - yvals(i-1))/2*h

            x_atual = x_atual + h
            i = i + 1
        end do
            
            numerov_desce = zvals(i-1)/yvals(i-1)
    
    
end function
end module

program edos
    use meu_modulo
    implicit none
    real*8 ::  x_inicial,x_final, a,  valsobe,valdesce
    real*8 :: comp_atual, comp_anterior
    real*8 :: e_inicial, delta_e, e_atual ,tol_e
    integer ::  bissec_iterations
    
        a = 3.0d0
        x_inicial = -10.0d0
        x_final = 10.0d0

        
    e_inicial = 3.0d0
    delta_e = 0.1d0
    tol_e = 0.0000000001d0
    e_atual = e_inicial
    bissec_iterations = 0
    
    open(13, file="results.txt")
    open(14, file="ratio.txt")
    ! =====LOOP BISSEÇÃO=====
    valsobe = numerov_sobe(e_atual, a, x_inicial, x_final)
    valdesce = numerov_desce(e_atual, a, x_final, x_inicial)
    comp_atual = valsobe - valdesce
    10 continue
    e_atual = e_atual + delta_e
    comp_anterior = comp_atual

    valsobe = numerov_sobe(e_atual, a, x_inicial, x_final)
    valdesce = numerov_desce(e_atual, a, x_final, x_inicial)
    comp_atual = valsobe - valdesce
    write(14,*) bissec_iterations, valsobe, valdesce, comp_atual


        if(comp_anterior*comp_atual.LT.0.0d0) then
                e_atual = e_atual - delta_e
                comp_atual = comp_anterior
                delta_e = delta_e/2.d0
        end if
        write(13,*) bissec_iterations, e_atual
        bissec_iterations = bissec_iterations + 1
        if(dabs(delta_e).GT.tol_e) goto 10

   close(13)
   close(14)
end program

