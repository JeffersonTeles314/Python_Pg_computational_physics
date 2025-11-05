program escreve
open (11, file="escreve.txt")
    do i = 5,11
        write (11,*) "valor de i=",i
    end do
    close (11)
end program