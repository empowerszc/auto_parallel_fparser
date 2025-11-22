PROGRAM dep_2d_shift
  IMPLICIT NONE
  INTEGER :: i, j
  INTEGER, DIMENSION(10, 10) :: A
  !$omp parallel
  !$omp do private(i, j) firstprivate(a) schedule(static)
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  ALLOCATE(apf_tmp_i(SIZE(A % i)))
  apf_tmp_i = A % i
  DO i = 1, 10
    INTEGER, ALLOCATABLE :: apf_tmp_i(:)
    ALLOCATE(apf_tmp_i(SIZE(A % i)))
    apf_tmp_i = A % i
    DO j = 1, 10
      A(i, j) = A(i - 1, j + 1)
      !$omp end do
      !$omp end parallel
    END DO
    A % i = apf_tmp_i
    DEALLOCATE(apf_tmp_i)
  END DO
  A % i = apf_tmp_i
  DEALLOCATE(apf_tmp_i)
END PROGRAM dep_2d_shift