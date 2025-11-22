PROGRAM equal_1d
  IMPLICIT NONE
  INTEGER :: i
  INTEGER, DIMENSION(10) :: A
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  ALLOCATE(apf_tmp_i(SIZE(A % i)))
  apf_tmp_i = A % i
  INTEGER, ALLOCATABLE :: apf_tmp_i_2(:)
  ALLOCATE(apf_tmp_i_2(SIZE(A % i)))
  apf_tmp_i_2 = A % i
  !$omp parallel do private(i) schedule(static)
  !$omp parallel do private(i) schedule(static)
    DO i = 1, 10
    A(i) = A(i)
  END DO
  A % i = apf_tmp_i_2
  DEALLOCATE(apf_tmp_i_2)
  A % i = apf_tmp_i
  DEALLOCATE(apf_tmp_i)
  !$omp end parallel do
  !$omp end parallel do
END PROGRAM equal_1d