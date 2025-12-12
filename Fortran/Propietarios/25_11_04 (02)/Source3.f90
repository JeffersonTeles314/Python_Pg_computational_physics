program seguro
implicit none
integer a
real m
open(13, file="implicit.txt")
m = 2.3
a = 5
write(13,*) m , a
close(13)
end program
