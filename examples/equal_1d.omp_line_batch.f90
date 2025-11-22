program equal_1d
implicit none
integer :: i
integer, dimension(10) :: A
!$omp parallel do private(i) schedule(static)
do i = 1, 10
  A(i) = A(i)
end do
!$omp end parallel do
end program equal_1d
