program nested
  implicit none
  integer :: i, j, n, m
  real :: a(100,100), b(100,100), c(100,100), s
  n = 100; m = 100
  s = 0.0
!$omp parallel do private(i, j) schedule(static) collapse(2)
  do i = 1, n
!$omp parallel do private(j) reduction(+:s) schedule(static)
    do j = 1, m
      c(i,j) = a(i,j) + b(i,j)
      s = s + c(i,j)
    end do
!$omp end parallel do
  end do
end program nested
