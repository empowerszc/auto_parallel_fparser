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
