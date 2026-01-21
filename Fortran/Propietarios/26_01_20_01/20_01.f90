program exemplo
implicit real*8 (a-h,o-z)
open(15, file="results.txt")
a = 4
b = 6
call algeb(a,b,soma,subtracao)
write(15,*) soma, subtracao
end program

subroutine algeb(a,b,som,sub)
implicit real*8(a-h,o-z)
som = a+b
sub = a-b
end
