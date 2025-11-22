program indep_diff_arrays
implicit none
integer :: i
integer, dimension(10) :: A, B
!$omp parallel do private(i) schedule(static)
do i = 1, 10
  A(i) = B(i-1)
end do
!$omp end parallel do
end program indep_diff_arrays
