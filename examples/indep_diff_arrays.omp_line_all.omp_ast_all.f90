PROGRAM indep_diff_arrays
  IMPLICIT NONE
  INTEGER :: i
  INTEGER, DIMENSION(10) :: A, B
  !$omp parallel do private(i) schedule(static)
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  ALLOCATE(apf_tmp_i(SIZE(A % i)))
  apf_tmp_i = A % i
  !$omp parallel do private(i) schedule(static)
  DO i = 1, 10
    A(i) = B(i - 1)
  END DO
  A % i = apf_tmp_i
  DEALLOCATE(apf_tmp_i)
  !$omp end parallel do
  !$omp end parallel do
END PROGRAM indep_diff_arrays