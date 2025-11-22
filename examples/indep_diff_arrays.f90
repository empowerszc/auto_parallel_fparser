program indep_diff_arrays
implicit none
integer :: i
integer, dimension(10) :: A, B
do i = 1, 10
  A(i) = B(i-1)
end do
end program indep_diff_arrays