! Use o método de Runge-Kutta, analisando o número de nós, para encontrar
! as três menores energias para o potencial V(x) = 1 2 kx 2 , do oscilador harmônico quântico (veja as figuras a seguir), onde k = 0,5 eV/Å2 . Considere uma energia máxima igual a Emáx = 6,25 eV, h = 0,01 , xmín =
! −1,6 a, xmáx = 1,6 a, ħ 2 = 7,6199682 meeVÅ 2 e uma precisão de 10−10.Compare os valores que você encontrou com os valores exatos.

module meu_modulo
  implicit none
  real*8, parameter :: hbar2_2m = 7.6199682d0
contains

    real*8 function volt(x,a)
        real*8, intent(in) :: x, a
        volt = (x**2.0d0)/4.0d0
    end function

    real*8 function func_f(x,y,z,e,a)
        real*8, intent(in) :: x, y, z, e, a
        func_f = -(2.0d0*(e - volt(x,a))*y)/hbar2_2m
    end function

    integer*8 function runge(e,a)
        real*8, intent(in) :: e, a
        real*8 :: x_inicial, x_final, h, x_atual
        real*8, allocatable :: yvals(:)
        real*8, allocatable :: zvals(:)
        real*8 :: k1y, k1z, k2y, k2z, k3y, k3z, k4y, k4z
        integer ::  i

        runge = 0
        x_inicial = -10.0d0
        x_final = 10.0d0
        h = 0.01d0

        ! Alocação dinâmica para evitar estouro de memória
        allocate(zvals(0:10000))
        allocate(yvals(0:10000))

        yvals(0) = 0.0d0 ! Condição inicial (y_pvi)
        zvals(0) = 0.0000001d0 ! Condição inicial (z_pvi)

        open(20, file="graph.txt")
        x_atual =  x_inicial
        i = 0
        do while(x_atual.LE.x_final)

            write(*,*)  x_atual, zvals(i)
            
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

            if(yvals(i+1)*yvals(i).LT.0) then
            runge = runge + 1
            end if
            x_atual = x_atual + h
            i = i+ 1

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
    e_inicial = 4.0d0
    delta_e = 0.01d0
    tol_e = 0.0000000001d0
    e_atual = e_inicial
    nn_buscado = 3
    bissec_iterations = 0
    
    open(13, file="results.txt")
    ! =====LOOP BISSEÇÃO=====


    10 continue
    e_atual = e_atual + delta_e
    nn_atual = runge(e_atual,a)

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






