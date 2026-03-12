module metodos_edos_qo
    implicit none
contains
    real*8 function exato(x)
        real*8, intent(in) :: x
        exato = exp(-(x**2)/4.0d0)
    end function

    real*8 function func(x,y)
        real*8, intent(in) :: x, y
        func = 9.80d0
    end function

    real*8 function kah_1qo(x,y,h)
        real*8, intent(in) :: x, y, h
        kah_1qo = h * func(x,y)
    end function

    real*8 function kah_2qo(x,y,h)
        real*8, intent(in) :: x, y, h
        kah_2qo = h * func(x + (h/2.0d0), y + (kah_1qo(x,y,h)/2.0d0))
    end function
    
    real*8 function kah_3qo(x,y,h)
        real*8, intent(in) :: x, y, h
        kah_3qo = h * func(x + (h/2.0d0), y + (kah_2qo(x,y,h)/2.0d0))
    end function

    real*8 function kah_4qo(x,y,h)
        real*8, intent(in) :: x, y, h
        kah_4qo = h * func(x + h, y + kah_3qo(x,y,h))
    end function

    real*8 function rk_qo(x,y,h)
        real*8, intent(in) :: x, y, h
        rk_qo = y + (kah_1qo(x,y,h) + 2.d0 * kah_2qo(x,y,h) + 2.d0 * kah_3qo(x,y,h) + kah_4qo(x,y,h)) / 6.0d0
    end function
end module

program edos
    use metodos_edos_qo
    implicit none

    real*8 :: x_inicial, x_final, h, x_atual
    integer :: i_total, i
    real*8, allocatable :: yvals(:)

    x_inicial = 0.0d0
    x_final = 10.0d0
    h = 0.10d0
    i_total = nint((x_final - x_inicial) / h)

    ! Alocação dinâmica para evitar estouro de memória
    allocate(yvals(0:i_total))

    open(13, file="results.txt")
    yvals(0) = 0.0d0 ! Condição inicial (y_pvi)

    do i = 0, i_total - 1
        x_atual = x_inicial + i * h

        ! Cálculo do próximo ponto
        yvals(i+1) = rk_qo(x_atual, yvals(i), h)

        ! Escrita dos resultados
        write(13, *) &
            "Passo:", i, " x:", x_atual, " y:", yvals(i)
    end do

    ! Escreve o último ponto calculado
    write(13, *) &
        "Passo:", i_total, " x:", x_final, " y:", yvals(i_total)

    close(13)
    print*, "Concluido. Verifique results.txt"
end program
