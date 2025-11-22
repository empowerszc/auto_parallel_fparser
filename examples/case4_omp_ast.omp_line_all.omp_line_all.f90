SUBROUTINE case8
  IMPLICIT NONE
  INTEGER :: i, n, k
  REAL :: A(100), B(100)
  n = 100
  k = 2
  DO i = 1, 5
    A(i) = B(i)
    ! S1：写A(i)
    B(i) = A(i + k)
    ! S2：反依赖（A(i+1)的读依赖于后续i+1的S1写）
    A(i + 1) = 2
    ! S3：输出依赖（S3写A(i+1)与i+1的S1写冲突）
  END DO
END SUBROUTINE case8