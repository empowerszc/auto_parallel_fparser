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