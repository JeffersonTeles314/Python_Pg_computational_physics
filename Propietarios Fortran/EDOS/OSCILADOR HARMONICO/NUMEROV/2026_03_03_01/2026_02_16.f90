module meu_modulo
  implicit none
  real*8, parameter :: hbar2_2m = 7.6199682d0
contains

    real*8 function volt(x,a)
        real*8, intent(in) :: x, a
        volt = (x**2.0d0)/4.0d0
    end function

real*8 function func_k2(x, e, a)
    real*8, intent(in) :: x, e, a
    ! k^2 = (2m/hbar^2) * (E - V(x))
    func_k2 = (2.0d0 * (e - volt(x, a))) / hbar2_2m
end function

integer function numerov(e, a)
    real*8, intent(in) :: e, a
    real*8 :: x, h, g_prev, g_curr, g_next, y_next
    real*8 :: y_prev, y_curr
    integer :: n_pontos, i

    numerov = 0
    h = 0.01d0
    x = -7.0d0 ! Aumentado para o decaimento ser visível
    n_pontos = 10000 ! (7 - (-7)) / 0.01
    
    y_prev = 0.0d0        ! Condição de contorno no infinito
    y_curr = 0.000001d0   ! Pequeno valor inicial
    
    do i = 1, n_pontos
        ! g(x) para Numerov: (h^2 / 12) * k^2
        g_prev = (h**2 / 12.0d0) * func_k2(x - h, e, a)
        g_curr = (h**2 / 12.0d0) * func_k2(x, e, a)
        g_next = (h**2 / 12.0d0) * func_k2(x + h, e, a)

        ! Fórmula de Numerov correta:
        ! y_{n+1} * (1 + g_{n+1}) = 2*y_n * (1 - 5*g_n) - y_{n-1} * (1 + g_{n-1})
        y_next = (2.0d0 * y_curr * (1.0d0 - 5.0d0 * g_curr) - y_prev * (1.0d0 + g_prev)) / (1.0d0 + g_next)

        ! Conta cruzamento de zero (nós)
        if (y_next * y_curr < 0.0d0) then
            numerov = numerov + 1
        end if

        ! Atualiza para o próximo passo
        y_prev = y_curr
        y_curr = y_next
        x = x + h
    end do
end function

end module meu_modulo

program edos
    use meu_modulo
    implicit none

    real*8 :: e_inicial, delta_e, e_atual ,tol_e, a
    integer :: nn_atual, nn_buscado, bissec_iterations
    
    a = 3.0d0
    e_inicial = 2.0d0
    delta_e = 0.01d0
    tol_e = 0.0000000001d0
    e_atual = e_inicial
    nn_buscado = 2
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
