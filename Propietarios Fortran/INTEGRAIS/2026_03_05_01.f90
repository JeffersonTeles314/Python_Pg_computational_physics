module meu_modulo
  implicit none
  real*8, parameter :: hbar2_2m = 7.6199682d0
contains

    real*8 function func(x)
        real*8, intent(in) :: x
                func= 2.718281828d0**x
    end function

end module meu_modulo

program edos
    use meu_modulo
    implicit none

    real*8 :: a, b, h
    real*8 :: sum
    integer :: n , i
    
    a = 2.0d0
    b = 4.0d0
    h = 0.01d0
    n = (b-a)/ h
    
    open(13, file="results.txt")
    ! =====LOOP BISSEÇÃO=====


    sum = (func(a)+func(b))*h/2
    do i = 1, n
    sum = sum + func(a + (i * h)) *h
    end do
    write(13,*) sum
   close(13)
end program
