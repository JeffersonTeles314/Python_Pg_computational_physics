module meu_modulo
  implicit none ! Necessária para Evitar atribuições de Tipo Por Causa da Inicialização Implícita
contains

    real*8 function func_f(x,y,z,e)
            real*8, intent(in) :: x, y, z, e
            func_f = -(2.0d0*e*y)/7.6199682d0
    end function

    integer*8 function runge(e)
        real*8, intent(in) :: e
        real*8 :: x_inicial, x_final, h, x_atual
        real*8, allocatable :: yvals(:)
        real*8, allocatable :: zvals(:)
        real*8 :: k1y, k1z, k2y, k2z, k3y, k3z, k4y, k4z
        integer :: i_total, i

        runge = 0
        x_inicial = -3.0d0
        x_final = 3.0d0
        h = 0.01d0
        i_total = nint((x_final - x_inicial) / h) + 100

        ! Alocação dinâmica para evitar estouro de memória
        allocate(zvals(0:i_total))
        allocate(yvals(0:i_total))

        yvals(0) = 0.0d0 ! Condição inicial (y_pvi)
        zvals(0) = 0.0000001d0 ! Condição inicial (z_pvi)


        do i = 0, i_total - 1
            x_atual = x_inicial + i * h
            k1y = h * zvals(i)
            k1z = h * func_f(x_atual,yvals(i),zvals(i),e)

            k2y = h * (zvals(i) + k1z/2.0d0)
            k2z = h * func_f(x_atual + h/2.0d0 ,yvals(i) + k1y/2.0d0,zvals(i)+ k1z/2.0d0,e)

            k3y = h * (zvals(i) + k2z/2.0d0)
            k3z = h * func_f(x_atual + h/2.0d0 ,yvals(i) + k2y/2.0d0,zvals(i)+ k2z/2.0d0,e)

            k4y = h * (zvals(i) + k3z)
            k4z = h * func_f(x_atual + h ,yvals(i) + k3y,zvals(i)+ k3z,e)

            ! C�lculo do
            yvals(i+1) = yvals(i) + (k1y+2.0d0*k2y+2.0d0*k3y+k4y)/6.0d0
            ! C�lculo do
            zvals(i+1) = zvals(i) + (k1z+2.0d0*k2z+2.0d0*k3z+k4z)/6.0d0

            if(yvals(i+1)*yvals(i).LT.0) then
            runge = runge + 1
            end if
        end do
    end function

end module meu_modulo


program edos
    use meu_modulo
    implicit none

    real*8 :: e_inicial, delta_e, e_atual ,tol_e
    integer :: nn_atual, nn_buscado, bissec_iterations
    
    e_inicial = 0.1d0
    delta_e = 0.1d0
    tol_e = 0.0000000001d0
    e_atual = e_inicial
    nn_buscado = 3
    bissec_iterations = 0
    
    open(13, file="results.txt")
    ! =====LOOP BISSEÇÃO=====


    10 continue
    e_atual = e_atual + delta_e
    nn_atual = runge(e_atual)

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






