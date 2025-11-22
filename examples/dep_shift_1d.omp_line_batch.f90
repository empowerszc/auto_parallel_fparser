program dep_shift_1d
implicit none
integer :: i
integer, dimension(10) :: A
do i = 1, 10
  A(i) = A(i-1)
end do
end program dep_shift_1d