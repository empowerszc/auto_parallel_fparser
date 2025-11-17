subroutine case1
    implicit none
    integer :: i, n
    real :: A(100), B(100)
    n = 100
do i = 2, 10
    A(i) = A(i-1)  ! 流依赖（Flow Dependency）：i-1 → i（前向，读先于写）
    B(i-1) = B(i)  ! 反依赖（Anti-Dependency）：i → i-1（后向，写先于读）
end do
end subroutine case1

subroutine case2
    implicit none
    integer :: i, n
    real :: A(100)
    n = 100
do i = 1, 10, 2  ! 步长=2，迭代：1,3,5,7,9
    A(i) = 0       ! S1：写A(i)
    A(i+1) = A(i)  ! S2：流依赖 S1→S2（i → i+1）
    A(i) = 1       ! S3：输出依赖（Output Dependency）S1→S3（同位置重复写）
end do
end subroutine case2

subroutine case3
    implicit none
    integer :: i, j, n
    real :: A(100,100), B(100,100)
    n = 100
do i = 1, 5
    do j = 1, 5
        A(i,j) = A(i-1,j) + A(i,j-1)  ! 流依赖：(i-1,j)→(i,j) 和 (i,j-1)→(i,j)
        B(i,j) = B(i+1,j-1)           ! 流依赖：(i+1,j-1)→(i,j)（需判断边界）
    end do
end do
end subroutine case3


subroutine case4
    implicit none
    integer :: i, n
    real :: A(100)
    n = 100
do i = 1, 10
    if (mod(i,2) == 0) then  ! 仅偶数i执行
        A(i) = A(i-1)        ! 流依赖：i-1→i（条件性）
    else                     ! 仅奇数i执行
        A(i+1) = A(i)        ! 流依赖：i→i+1（条件性）
    end if
end do
end subroutine case4

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


subroutine case6
    implicit none
    integer :: i, n
    real :: A(100)
    n = 100
do i = 10, 1, -1  ! 步长=-1，迭代：10,9,...,1
    A(i) = A(i+1)  ! 流依赖：i+1 → i（因i递减，i+1先执行）
end do
end subroutine case6


subroutine case7
    implicit none
    integer :: i, j, k, n
    real :: C(3,3,3)
    n = 3
do k = 1, 3
    do j = 1, 3
        do i = 1, 3
            C(i,j,k) = C(i-1,j,k) + C(i,j-1,k) + C(i,j,k-1)
        end do
    end do
end do
end subroutine case7


subroutine case7_2
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
end subroutine case7_2

subroutine case8
    implicit none
    integer :: i, n, k
    real :: A(100), B(100)
    n = 100
    k = 2
do i = 1, 5
    A(i) = B(i)    ! S1：写A(i)
    B(i) = A(i+k)  ! S2：反依赖（A(i+1)的读依赖于后续i+1的S1写）
    A(i+1) = 2     ! S3：输出依赖（S3写A(i+1)与i+1的S1写冲突）
end do
end subroutine case8

subroutine case8_2
    implicit none
    integer :: i, n, k
    real :: A(100), B(100)
    n = 100
    k = 2
do i = 1, 5
    A(i) = B(i)    ! S1：写A(i)
    B(i) = A(i+k)  ! S2：反依赖（A(i+1)的读依赖于后续i+1的S1写）
    A(i+1) = 2     ! S3：输出依赖（S3写A(i+1)与i+1的S1写冲突）
end do
end subroutine case8_2

subroutine case9
    implicit none
    integer :: i, j, n
    real :: A(100), B(100)
    n = 100
do i = 1, 10
    A(i) = A(i-1)  ! S1：流依赖i-1→i（外层相邻依赖）
    do j = 1, 5
        B(i,j) = B(i,j+1)  ! S2：流依赖j+1→j（内层反向依赖）
    end do
    A(i+2) = A(i)  ! S3：流依赖i→i+2（外层间隔2的依赖）
end do
end subroutine case9


integer function idx(i,j) result(res)
    integer, intent(in) :: i,j
    res = i + 2*j  ! 索引表达式：i+2j
end function

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