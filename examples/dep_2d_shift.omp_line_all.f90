program dep_2d_shift
implicit none
integer :: i, j
integer, dimension(10,10) :: A
do i = 1, 10
  do j = 1, 10
    A(i,j) = A(i-1,j+1)
  end do
end do
end program dep_2d_shift