PROGRAM dep_shift_1d
  IMPLICIT NONE
  INTEGER :: i
  INTEGER, DIMENSION(10) :: A
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  ALLOCATE(apf_tmp_i(SIZE(A % i)))
  apf_tmp_i = A % i
  DO i = 1, 10
    A(i) = A(i - 1)
  END DO
  A % i = apf_tmp_i
  DEALLOCATE(apf_tmp_i)
END PROGRAM dep_shift_1d