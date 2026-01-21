module metodos_edos
    implicit none
contains
    real*8 function exato(x)
        real*8, intent(in) :: x
        exato = exp(-(x**2)/2.0d0)
    end function

    real*8 function func_f(x,y,z)
        real*8, intent(in) :: x, y, z
        func_f = -y -(x * z)
    end function

    real*8 function euler_y(h,y,z)
        real*8, intent(in) :: h, y, z
        euler_y = y + h*z
    end function

    real*8 function euler_z(h,x,y,z)
        real*8, intent(in) :: h, x, y, z
        euler_z = z + h* func_f(x,y,z)
    end function
end module

program edos
    use metodos_edos
    implicit none

    real*8 :: x_inicial, x_final, h, x_atual
    integer :: i_total, i
    real*8, allocatable :: yvals(:)
    real*8, allocatable :: zvals(:)

    x_inicial = 0.0d0
    x_final = 5.0d0
    h = 0.20d0
    i_total = nint((x_final - x_inicial) / h)

    ! Alocação dinâmica para evitar estouro de memória
    allocate(zvals(0:i_total))
    allocate(yvals(0:i_total))

    open(13, file="results.txt")
    
    yvals(0) = 1.0d0 ! Condição inicial (y_pvi)
    zvals(0) = 0.0d0 ! Condição inicial (z_pvi)

    ! =====LOOP=====

    do i = 0, i_total - 1
        x_atual = x_inicial + i * h

        ! Cálculo do
        yvals(i+1) = euler_y(h, yvals(i), zvals(i))
        ! Cálculo do
        zvals(i+1) = euler_z(h, x_atual, yvals(i), zvals(i))

        ! Escrita dos resultados
        write(13, *) &
            "Passo:", i , " x:" , x_atual ," y:" , yvals(i) ," z:", zvals(i)," exato:", exato(x_atual)
    end do

    ! Escreve o último ponto calculado
    write(13, *) &
        "Passo:", i, " x:", x_atual, " y:", yvals(i)," z:", zvals(i)," exato:", exato(x_atual)

    close(13)
    print*, "Concluido. Verifique results.txt"
end program
