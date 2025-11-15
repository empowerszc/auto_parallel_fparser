program nested
  implicit none
  integer :: i, j, n, m
  real :: a(100,100), b(100,100), c(100,100), s
  n = 100; m = 100
  s = 0.0
  do i = 1, n
    do j = 1, m
      c(i,j) = a(i,j) + b(i,j)
      s = s + c(i,j)
    end do
  end do
end program nested