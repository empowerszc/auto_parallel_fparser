PROGRAM equal_1d
  IMPLICIT NONE
  INTEGER :: i
  INTEGER, DIMENSION(10) :: A
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  ALLOCATE(apf_tmp_i(SIZE(A % i)))
  apf_tmp_i = A % i
  !$omp parallel do private(i) schedule(static)
  DO i = 1, 10
    A(i) = A(i)
  END DO
!$omp end parallel do
  A % i = apf_tmp_i
  DEALLOCATE(apf_tmp_i)
END PROGRAM equal_1d
