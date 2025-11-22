program dep_2d_shift
implicit none
integer :: i, j
integer, dimension(10,10) :: A
!$omp parallel
!$omp do private(i, j) firstprivate(a) schedule(static)
do i = 1, 10
  do j = 1, 10
    A(i,j) = A(i-1,j+1)
!$omp end do
!$omp end parallel
  end do
end do
end program dep_2d_shift
