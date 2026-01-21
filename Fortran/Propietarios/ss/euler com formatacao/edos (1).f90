module metodos_edos
    implicit none
contains
    real*8 function exato(x)
        real*8, intent(in) :: x
        exato = exp(-(x**2)/4.0d0)
    end function

    real*8 function func(x,y)
        real*8, intent(in) :: x, y
        func = -(x * y)/2.0d0
    end function

    real*8 function kah_1e(x,y,h)
        real*8, intent(in) :: x, y, h
        kah_1e = h * func(x,y)
    end function

    real*8 function euler(x,y,h)
        real*8, intent(in) :: x, y, h
        euler = y + kah_1e(x,y,h)
    end function
end module

program edos
    use metodos_edos
    implicit none

    real*8 :: x_inicial, x_final, h, x_atual
    integer :: i_total, i
    real*8, allocatable :: yvals(:)

    x_inicial = 0.0d0
    x_final = 3.0d0
    h = 0.05d0
    i_total = nint((x_final - x_inicial) / h)

    ! Alocação dinâmica para evitar estouro de memória
    allocate(yvals(0:i_total))

    open(13, file="results.txt")
    yvals(0) = 1.0d0 ! Condição inicial (y_pvi)

    do i = 0, i_total - 1
        x_atual = x_inicial + i * h

        ! Cálculo do próximo ponto
        yvals(i+1) = euler(x_atual, yvals(i), h)

        ! Escrita dos resultados
        write(13, "(A,I3,A,F8.4,A,F8.4,A,F8.4)") &
            "Passo:", i, " x:", x_atual, " y:", yvals(i), " exato:", exato(x_atual)
    end do

    ! Escreve o último ponto calculado
    write(13, "(A,I3,A,F8.4,A,F8.4,A,F8.4)") &
        "Passo:", i_total, " x:", x_final, " y:", yvals(i_total), " exato:", exato(x_final)

    close(13)
    print*, "Concluido. Verifique results.txt"
end program
