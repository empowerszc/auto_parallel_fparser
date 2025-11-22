program demo
  implicit none
  integer :: i, n
  real :: a(100), b(100), s
  n = 100
  s = 0.0
!$omp parallel do private(i) reduction(+:s) schedule(static)
  do i = 1, n
    a(i) = b(i) + 1.0
    s = s + a(i)
  end do
!$omp end parallel do
end program demo
