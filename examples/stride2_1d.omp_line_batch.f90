program stride2_1d
implicit none
integer :: i
integer, dimension(12) :: A
do i = 1, 11, 2
  A(i) = A(i-2)
end do
end program stride2_1d