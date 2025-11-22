SUBROUTINE case1
  IMPLICIT NONE
  INTEGER :: i, n
  REAL :: A(100), B(100)
  n = 100
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  ALLOCATE(apf_tmp_i(SIZE(A % i)))
  ALLOCATE(apf_tmp_i(SIZE(B % i)))
  apf_tmp_i = A % i
  apf_tmp_i = B % i
  DO i = 2, 10
    A(i) = A(i - 1)
    ! 流依赖（Flow Dependency）：i-1 → i（前向，读先于写）
    B(i - 1) = B(i)
    ! 反依赖（Anti-Dependency）：i → i-1（后向，写先于读）
  END DO
  A % i = apf_tmp_i
  B % i = apf_tmp_i
  DEALLOCATE(apf_tmp_i)
  DEALLOCATE(apf_tmp_i)
END SUBROUTINE case1

SUBROUTINE case2
  IMPLICIT NONE
  INTEGER :: i, n
  REAL :: A(100)
  n = 100
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  ALLOCATE(apf_tmp_i(SIZE(A % i)))
  apf_tmp_i = A % i
  DO i = 1, 10, 2
    ! 步长=2，迭代：1,3,5,7,9
    A(i) = 0
    ! S1：写A(i)
    A(i + 1) = A(i)
    ! S2：流依赖 S1→S2（i → i+1）
    A(i) = 1
    ! S3：输出依赖（Output Dependency）S1→S3（同位置重复写）
  END DO
  A % i = apf_tmp_i
  DEALLOCATE(apf_tmp_i)
END SUBROUTINE case2

SUBROUTINE case3
  IMPLICIT NONE
  INTEGER :: i, j, n
  REAL :: A(100, 100), B(100, 100)
  n = 100
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  ALLOCATE(apf_tmp_i(SIZE(A % i)))
  ALLOCATE(apf_tmp_i(SIZE(B % i)))
  apf_tmp_i = A % i
  apf_tmp_i = B % i
  DO i = 1, 5
    INTEGER, ALLOCATABLE :: apf_tmp_i(:)
    INTEGER, ALLOCATABLE :: apf_tmp_i(:)
    ALLOCATE(apf_tmp_i(SIZE(A % i)))
    ALLOCATE(apf_tmp_i(SIZE(B % i)))
    apf_tmp_i = A % i
    apf_tmp_i = B % i
    DO j = 1, 5
      A(i, j) = A(i - 1, j) + A(i, j - 1)
      ! 流依赖：(i-1,j)→(i,j) 和 (i,j-1)→(i,j)
      B(i, j) = B(i + 1, j - 1)
      ! 流依赖：(i+1,j-1)→(i,j)（需判断边界）
    END DO
    A % i = apf_tmp_i
    B % i = apf_tmp_i
    DEALLOCATE(apf_tmp_i)
    DEALLOCATE(apf_tmp_i)
  END DO
  A % i = apf_tmp_i
  B % i = apf_tmp_i
  DEALLOCATE(apf_tmp_i)
  DEALLOCATE(apf_tmp_i)
END SUBROUTINE case3


SUBROUTINE case4
  IMPLICIT NONE
  INTEGER :: i, n
  REAL :: A(100)
  n = 100
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  ALLOCATE(apf_tmp_i(SIZE(A % i)))
  apf_tmp_i = A % i
  DO i = 1, 10
    IF (MOD(i, 2) == 0) THEN
      ! 仅偶数i执行
      A(i) = A(i - 1)
      ! 流依赖：i-1→i（条件性）
    ELSE
      ! 仅奇数i执行
      A(i + 1) = A(i)
      ! 流依赖：i→i+1（条件性）
    END IF
  END DO
  A % i = apf_tmp_i
  DEALLOCATE(apf_tmp_i)
END SUBROUTINE case4

SUBROUTINE case5
  IMPLICIT NONE
  INTEGER :: i, j, n
  REAL :: A(100, 100)
  n = 100
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  ALLOCATE(apf_tmp_i(SIZE(A % i)))
  apf_tmp_i = A % i
  DO i = 2, 4
    INTEGER, ALLOCATABLE :: apf_tmp_i(:)
    ALLOCATE(apf_tmp_i(SIZE(A % i)))
    apf_tmp_i = A % i
    DO j = 2, 4
      A(i * j) = A((i - 1) * (j + 1))
      ! 索引：i*j 和 (i-1)(j+1)
    END DO
    A % i = apf_tmp_i
    DEALLOCATE(apf_tmp_i)
  END DO
  A % i = apf_tmp_i
  DEALLOCATE(apf_tmp_i)
END SUBROUTINE case5


SUBROUTINE case6
  IMPLICIT NONE
  INTEGER :: i, n
  REAL :: A(100)
  n = 100
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  ALLOCATE(apf_tmp_i(SIZE(A % i)))
  apf_tmp_i = A % i
  DO i = 10, 1, - 1
    ! 步长=-1，迭代：10,9,...,1
    A(i) = A(i + 1)
    ! 流依赖：i+1 → i（因i递减，i+1先执行）
  END DO
  A % i = apf_tmp_i
  DEALLOCATE(apf_tmp_i)
END SUBROUTINE case6


SUBROUTINE case7
  IMPLICIT NONE
  INTEGER :: i, j, k, n
  REAL :: C(3, 3, 3)
  n = 3
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  ALLOCATE(apf_tmp_i(SIZE(C % i)))
  apf_tmp_i = C % i
  DO k = 1, 3
    INTEGER, ALLOCATABLE :: apf_tmp_i(:)
    ALLOCATE(apf_tmp_i(SIZE(C % i)))
    apf_tmp_i = C % i
    DO j = 1, 3
      INTEGER, ALLOCATABLE :: apf_tmp_i(:)
      ALLOCATE(apf_tmp_i(SIZE(C % i)))
      apf_tmp_i = C % i
      DO i = 1, 3
        C(i, j, k) = C(i - 1, j, k) + C(i, j - 1, k) + C(i, j, k - 1)
      END DO
      C % i = apf_tmp_i
      DEALLOCATE(apf_tmp_i)
    END DO
    C % i = apf_tmp_i
    DEALLOCATE(apf_tmp_i)
  END DO
  C % i = apf_tmp_i
  DEALLOCATE(apf_tmp_i)
END SUBROUTINE case7


SUBROUTINE case7_2
  IMPLICIT NONE
  INTEGER :: i, j, k, t, n
  REAL :: C(3, 3, 3)
  n = 3
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  ALLOCATE(apf_tmp_i(SIZE(C % i)))
  apf_tmp_i = C % i
  !$omp parallel
  !$omp do private(t, k, j, i) schedule(static)
  DO t = 1, 3
    INTEGER, ALLOCATABLE :: apf_tmp_i(:)
    ALLOCATE(apf_tmp_i(SIZE(C % i)))
    apf_tmp_i = C % i
    DO k = 1, 3
      INTEGER, ALLOCATABLE :: apf_tmp_i(:)
      ALLOCATE(apf_tmp_i(SIZE(C % i)))
      apf_tmp_i = C % i
      DO j = 1, 3
        INTEGER, ALLOCATABLE :: apf_tmp_i(:)
        ALLOCATE(apf_tmp_i(SIZE(C % i)))
        apf_tmp_i = C % i
        DO i = 1, 3
          C(i, j, k, t) = C(i - 1, j, k, t) + C(i, j - 1, k, t) + C(i, j, k - 1, t)
        END DO
        C % i = apf_tmp_i
        DEALLOCATE(apf_tmp_i)
      END DO
      C % i = apf_tmp_i
      DEALLOCATE(apf_tmp_i)
    END DO
    C % i = apf_tmp_i
    DEALLOCATE(apf_tmp_i)
  END DO
  !$omp end do
  !$omp end parallel
  C % i = apf_tmp_i
  DEALLOCATE(apf_tmp_i)
END SUBROUTINE case7_2

SUBROUTINE case8
  IMPLICIT NONE
  INTEGER :: i, n, k
  REAL :: A(100), B(100)
  n = 100
  k = 2
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  ALLOCATE(apf_tmp_i(SIZE(A % i)))
  ALLOCATE(apf_tmp_i(SIZE(B % i)))
  apf_tmp_i = A % i
  apf_tmp_i = B % i
  DO i = 1, 5
    A(i) = B(i)
    ! S1：写A(i)
    B(i) = A(i + k)
    ! S2：反依赖（A(i+1)的读依赖于后续i+1的S1写）
    A(i + 1) = 2
    ! S3：输出依赖（S3写A(i+1)与i+1的S1写冲突）
  END DO
  A % i = apf_tmp_i
  B % i = apf_tmp_i
  DEALLOCATE(apf_tmp_i)
  DEALLOCATE(apf_tmp_i)
END SUBROUTINE case8

SUBROUTINE case8_2
  IMPLICIT NONE
  INTEGER :: i, n, k
  REAL :: A(100), B(100)
  n = 100
  k = 2
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  ALLOCATE(apf_tmp_i(SIZE(A % i)))
  ALLOCATE(apf_tmp_i(SIZE(B % i)))
  apf_tmp_i = A % i
  apf_tmp_i = B % i
  DO i = 1, 5
    A(i) = B(i)
    ! S1：写A(i)
    B(i) = A(i + k)
    ! S2：反依赖（A(i+1)的读依赖于后续i+1的S1写）
    A(i + 1) = 2
    ! S3：输出依赖（S3写A(i+1)与i+1的S1写冲突）
  END DO
  A % i = apf_tmp_i
  B % i = apf_tmp_i
  DEALLOCATE(apf_tmp_i)
  DEALLOCATE(apf_tmp_i)
END SUBROUTINE case8_2

SUBROUTINE case9
  IMPLICIT NONE
  INTEGER :: i, j, n
  REAL :: A(100), B(100)
  n = 100
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  ALLOCATE(apf_tmp_i(SIZE(A % i)))
  ALLOCATE(apf_tmp_i(SIZE(B % i)))
  apf_tmp_i = A % i
  apf_tmp_i = B % i
  DO i = 1, 10
    A(i) = A(i - 1)
    INTEGER, ALLOCATABLE :: apf_tmp_i(:)
    ALLOCATE(apf_tmp_i(SIZE(B % i)))
    apf_tmp_i = B % i
    ! S1：流依赖i-1→i（外层相邻依赖）
      DO j = 1, 5
      B(i, j) = B(i, j + 1)
      ! S2：流依赖j+1→j（内层反向依赖）
    END DO
    B % i = apf_tmp_i
    DEALLOCATE(apf_tmp_i)
    A(i + 2) = A(i)
    ! S3：流依赖i→i+2（外层间隔2的依赖）
  END DO
  A % i = apf_tmp_i
  B % i = apf_tmp_i
  DEALLOCATE(apf_tmp_i)
  DEALLOCATE(apf_tmp_i)
END SUBROUTINE case9


INTEGER FUNCTION idx(i, j) RESULT(res)
  INTEGER, INTENT(IN) :: i, j
  res = i + 2 * j
  ! 索引表达式：i+2j
END FUNCTION

SUBROUTINE case10
  IMPLICIT NONE
  INTEGER :: i, j, n
  REAL :: A(100, 100)
  n = 100
  REAL, ALLOCATABLE :: apf_tmp_idx(:)
  ALLOCATE(apf_tmp_idx(SIZE(A % idx)))
  apf_tmp_idx = A % idx
  DO i = 1, 4
    REAL, ALLOCATABLE :: apf_tmp_idx(:)
    ALLOCATE(apf_tmp_idx(SIZE(A % idx)))
    apf_tmp_idx = A % idx
    DO j = 1, 4
      A(idx(i, j)) = A(idx(i - 1, j + 1))
      ! 索引：k = i+2j 和 k+1 = (i-1)+2(j+1)
    END DO
    A % idx = apf_tmp_idx
    DEALLOCATE(apf_tmp_idx)
  END DO
  A % idx = apf_tmp_idx
  DEALLOCATE(apf_tmp_idx)
END SUBROUTINE case10