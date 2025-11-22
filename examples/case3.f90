
subroutine case7
    implicit none
    integer :: i, j, k, t, n
    real :: C(3,3,3)
    n = 3
    do t = 1, 3
do k = 1, 3
    do j = 1, 3
        do i = 1, 3
            C(i,j,k,t) = C(i-1,j,k,t) + C(i,j-1,k,t) + C(i,j,k-1,t)
        end do
    end do
end do
end do
end subroutine case7
