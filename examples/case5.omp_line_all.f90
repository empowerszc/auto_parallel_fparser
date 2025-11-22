subroutine case10
    implicit none
    integer :: i, j, n
    real :: A(100,100)
    n = 100
do i = 1, 4
    do j = 1, 4
        A(idx(i,j)) = A(idx(i-1,j+1))  ! 索引：k = i+2j 和 k+1 = (i-1)+2(j+1)
    end do
end do
end subroutine case10

