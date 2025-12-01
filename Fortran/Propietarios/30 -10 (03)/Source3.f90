! Crie um Programa, que escrve em um txt os n£meros de 5 a 11.

program graph
open(11, file="escreve.txt")
xmin = 0
xmax =  6.283185
N = 2000
deltax = (xmax-xmin)/N
do i=0,N
   x = xmin+i*deltax
   write(11,*)x, cos(x)
end do
close(11)
end program
