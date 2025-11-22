subroutine case5
    implicit none
    integer :: i, j, n
    real :: A(100,100)
    n = 100
do i = 2, 4
    do j = 2, 4
        A(i*j) = A((i-1)*(j+1))  ! 索引：i*j 和 (i-1)(j+1)
    end do
end do
end subroutine case5
