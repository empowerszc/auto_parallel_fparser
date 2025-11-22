program equal_1d
implicit none
integer :: i
integer, dimension(10) :: A
do i = 1, 10
  A(i) = A(i)
end do
end program equal_1d