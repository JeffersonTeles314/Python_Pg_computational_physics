PROGRAM r_int3d
! programa principal para a rotina int3d
      common/blc1/xmax
      REAL xmax
      REAL PI
      PARAMETER (PI=3.1415926)
      REAL s, xmin

      open (30, file='integral.txt')
      write(30,*) 'Integral de r^2 sobre um volume esferico'
      write(30,*) '   Raio', '           int3d', '             real'

      xmax=9.0d0
      xmin=0.0d0
      call int3d(xmin, xmax, s)
      write(30,*) xmax, s, 4.0*PI*(xmax**5)/5.0
      END

!***********************************************************************

      REAL FUNCTION func(x, y, z) ! função a ser integrada
      REAL x, y, z
      func = z ! Digite aqui a função a ser integrada
      END

!***********************************************************************

      REAL FUNCTION z1(x, y) ! limite inferior de integração na direção z
      REAL x, y
      common /blc1/xmax
      REAL xmax
      z1=  0  ! Digite aqui limite inferior de integração na direção z
      END

!***********************************************************************

      REAL FUNCTION z2(x, y) ! limite superior de integração na direção z
      REAL x, y
      common /blc1/xmax
      REAL xmax
      z2=  dsqrt(dabs((x**2.0d0)-9.0d0*(y**2.0d0)))   ! Digite aqui limite superior de integração na direção z
      END

!***********************************************************************

      REAL FUNCTION y1(x) ! limite inferior de integração na direção y
      REAL x
      common /blc1/xmax
      REAL xmax
      y1= 0   ! Digite aqui limite inferior de integração na direção y
      END

!***********************************************************************

      REAL FUNCTION y2(x) ! limite superior de integração na direção y
      REAL x
      common /blc1/xmax
      REAL xmax
      y2= x/3.0d0  ! Digite aqui limite superior de integração na direção y
      END

!***********************************************************************

 SUBROUTINE simpson_x(func, a, b, ss)
      REAL func, a, b, ss, h, sum
      INTEGER n, i
      EXTERNAL func

      n = 1000 ! Número de divisões (deve ser par)
      h = (b - a) / n
      sum = func(a) + func(b)

      do i = 1, n - 1
         if (mod(i, 2) .ne. 0) then
            sum = sum + 4.0 * func(a + i * h)
         else
            sum = sum + 2.0 * func(a + i * h)
         end if
      end do
      ss = (sum * h) / 3.0
      END

!***********************************************************************

      SUBROUTINE simpson_y(func, a, b, ss)
      REAL func, a, b, ss, h, sum
      INTEGER n, i
      EXTERNAL func

      n = 1000 ! Número de divisões (deve ser par)
      h = (b - a) / n
      sum = func(a) + func(b)

      do i = 1, n - 1
         if (mod(i, 2) .ne. 0) then
            sum = sum + 4.0 * func(a + i * h)
         else
            sum = sum + 2.0 * func(a + i * h)
         end if
      end do
      ss = (sum * h) / 3.0
      END

!***********************************************************************

      SUBROUTINE simpson_z(func, a, b, ss)
      REAL func, a, b, ss, h, sum
      INTEGER n, i
      EXTERNAL func

      n = 1000 ! Número de divisões (deve ser par)
      h = (b - a) / n
      sum = func(a) + func(b)

      do i = 1, n - 1
         if (mod(i, 2) .ne. 0) then
            sum = sum + 4.0 * func(a + i * h)
         else
            sum = sum + 2.0 * func(a + i * h)
         end if
      end do
      ss = (sum * h) / 3.0
      END


!***********************************************************************

      SUBROUTINE int3d(x1, x2, ss)
      REAL ss, x1, x2, hh
      EXTERNAL hh
!USA hh,simpson_x
      call simpson_x(hh, x1, x2, ss)
      return
      END

      FUNCTION f(zz)
      REAL f, zz, func, x, y, z
      COMMON /xyz/ x, y, z
! USA func
      z=zz
      f=func(x, y, z)
      return
      END

!***********************************************************************

      FUNCTION g(yy)
      REAL g, yy, f, z1, z2, x, y, z
      EXTERNAL f
      COMMON /xyz/ x, y, z
! USA f, simpson_z, z1, z2
      REAL ss
      y=yy
      call simpson_z(f, z1(x, y), z2(x, y), ss)
      g=ss
      return
      END

!***********************************************************************

      FUNCTION hh(xx)
      REAL hh, xx, g, y1, y2, x, y, z
      EXTERNAL g
      COMMON /xyz/ x, y, z
! USA g, simpson_y, y1, y2
      REAL ss
      x=xx
      call simpson_y(g, y1(x), y2(x), ss)
      hh=ss
      return
      END
