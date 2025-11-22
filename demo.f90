program demo
  implicit none
  integer :: i, n
  real, dimension(:), allocatable :: a, b
  n = 1000
  allocate(a(n), b(n))
  a = 1.0
  b = 2.0
  do i = 1, n
     a(i) = a(i) + b(i)
  end do
  print *, a(1)
  deallocate(a, b)
end program demo