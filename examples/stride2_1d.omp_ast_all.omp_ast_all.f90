PROGRAM stride2_1d
  IMPLICIT NONE
  INTEGER :: i
  INTEGER, DIMENSION(12) :: A
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  ALLOCATE(apf_tmp_i(SIZE(A % i)))
  apf_tmp_i = A % i
  INTEGER, ALLOCATABLE :: apf_tmp_i_2(:)
  ALLOCATE(apf_tmp_i_2(SIZE(A % i)))
  apf_tmp_i_2 = A % i
  DO i = 1, 11, 2
    A(i) = A(i - 2)
  END DO
  A % i = apf_tmp_i_2
  DEALLOCATE(apf_tmp_i_2)
  A % i = apf_tmp_i
  DEALLOCATE(apf_tmp_i)
END PROGRAM stride2_1d