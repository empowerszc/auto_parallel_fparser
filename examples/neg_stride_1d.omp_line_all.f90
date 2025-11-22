program neg_stride_1d
implicit none
integer :: i
integer, dimension(10) :: A
do i = 10, 1, -1
  A(i) = A(i+1)
end do
end program neg_stride_1d