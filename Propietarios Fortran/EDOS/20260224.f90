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

     real*8 function runge_sobe(e,a,x_ini,x_fim)
        real*8, intent(in) :: e,a,x_ini,x_fim
        real*8 :: h, x
        real*8 :: y, z
        real*8 :: k1y,k2y,k3y,k4y
        real*8 :: k1z,k2z,k3z,k4z
        real*8, parameter :: match = -1.6d0

        h = 0.01d0
        x = x_ini
        y = 1.0d-8
        z = 1.0d-8

        do while (x < match)
            k1y = h*z
            k1z = h*func_f(x,y,z,e,a)

            k2y = h*(z + 0.5d0*k1z)
            k2z = h*func_f(x+0.5d0*h, y+0.5d0*k1y, z+0.5d0*k1z, e, a)

            k3y = h*(z + 0.5d0*k2z)
            k3z = h*func_f(x+0.5d0*h, y+0.5d0*k2y, z+0.5d0*k2z, e, a)

            k4y = h*(z + k3z)
            k4z = h*func_f(x+h, y+k3y, z+k3z, e, a)

            y = y + (k1y + 2*k2y + 2*k3y + k4y)/6.d0
            z = z + (k1z + 2*k2z + 2*k3z + k4z)/6.d0
            x = x + h
        end do

        runge_sobe = z/y
    end function

    real*8 function runge_desce(e,a,x_ini,x_fim)
        real*8, intent(in) :: e,a,x_ini,x_fim
        real*8 :: h, x
        real*8 :: y, z
        real*8 :: k1y,k2y,k3y,k4y
        real*8 :: k1z,k2z,k3z,k4z
        real*8, parameter :: match = -1.6d0

        h = -0.01d0
        x = x_fim
        y = 1.0d-8
        z = -1.0d-8

        do while (x > match)
            k1y = h*z
            k1z = h*func_f(x,y,z,e,a)

            k2y = h*(z + 0.5d0*k1z)
            k2z = h*func_f(x+0.5d0*h, y+0.5d0*k1y, z+0.5d0*k1z, e, a)

            k3y = h*(z + 0.5d0*k2z)
            k3z = h*func_f(x+0.5d0*h, y+0.5d0*k2y, z+0.5d0*k2z, e, a)

            k4y = h*(z + k3z)
            k4z = h*func_f(x+h, y+k3y, z+k3z, e, a)

            y = y + (k1y + 2*k2y + 2*k3y + k4y)/6.d0
            z = z + (k1z + 2*k2z + 2*k3z + k4z)/6.d0
            x = x + h
        end do

        runge_desce = z/y
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

        
    e_inicial = 4.0d0
    delta_e = 0.1d0
    tol_e = 0.0000000001d0
    e_atual = e_inicial
    bissec_iterations = 0
    
    open(13, file="results.txt")
    open(14, file="ratio.txt")
    ! =====LOOP BISSEÇÃO=====
    valsobe = runge_sobe(e_atual, a, x_inicial, x_final)
    valdesce = runge_desce(e_atual, a, x_inicial, x_final)
    comp_atual = valsobe - valdesce
    10 continue
    e_atual = e_atual + delta_e
    comp_anterior = comp_atual

    valsobe = runge_sobe(e_atual, a, x_inicial, x_final)
    valdesce = runge_desce(e_atual, a, x_inicial, x_final)
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

