module metodos_edos
    implicit none
contains
    real*8 function exato(x)
        real*8, intent(in) :: x
        exato = 1.5d0 * dcos(x*dsqrt(9.8d0))
    end function

    real*8 function func_f(x,y,z)
        real*8, intent(in) :: x, y, z
        func_f = -9.8d0 * dsin(y)
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
    real*8 :: k1y, k1z, k2y, k2z, k3y, k3z, k4y, k4z
    integer :: i_total, i
    real*8, allocatable :: yvals(:)
    real*8, allocatable :: zvals(:)

    x_inicial = 0.0d0
    x_final = 10.0d0
    h = 0.001d0
    i_total = nint((x_final - x_inicial) / h)

    ! Alocação dinâmica para evitar estouro de memória
    allocate(zvals(0:i_total))
    allocate(yvals(0:i_total))

    open(13, file="results.txt")
    
    yvals(0) = 1.5d0 ! Condição inicial (y_pvi)
    zvals(0) = 0.0d0 ! Condição inicial (z_pvi)

    ! =====LOOP=====

    do i = 0, i_total - 1
        x_atual = x_inicial + i * h
        k1y = h * zvals(i)
        k1z = h * func_f(x_atual,yvals(i),zvals(i))

        k2y = h * (zvals(i) + k1z/2.0d0)
        k2z = h * func_f(x_atual + h/2.0d0 ,yvals(i) + k1y/2.0d0,zvals(i)+ k1z/2.0d0)

        k3y = h * (zvals(i) + k2z/2.0d0)
        k3z = h * func_f(x_atual + h/2.0d0 ,yvals(i) + k2y/2.0d0,zvals(i)+ k2z/2.0d0)

        k4y = h * (zvals(i) + k3z)
        k4z = h * func_f(x_atual + h ,yvals(i) + k3y,zvals(i)+ k3z)

        ! Cálculo do
        yvals(i+1) = yvals(i) + (k1y+2.0d0*k2y+2.0d0*k3y+k4y)/6.0d0
        ! Cálculo do
        zvals(i+1) = zvals(i) + (k1z+2.0d0*k2z+2.0d0*k3z+k4z)/6.0d0

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
