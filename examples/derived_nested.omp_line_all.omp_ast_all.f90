MODULE nested_type_mod
  IMPLICIT NONE
  TYPE :: Inner
    REAL :: v
  END TYPE Inner
  TYPE :: Outer
    TYPE(Inner) :: in
    REAL, ALLOCATABLE :: arr(:)
  END TYPE Outer
  CONTAINS
  SUBROUTINE work(o, n)
    REAL :: apf_tmp_in
    REAL :: apf_tmp_arr
    TYPE(Outer), INTENT(INOUT) :: o
    INTEGER, INTENT(IN) :: n
    INTEGER :: i
    apf_tmp_arr = o % arr
    apf_tmp_in = o % in
    ALLOCATE(apf_tmp_arr(n))
    REAL :: apf_tmp_v
    REAL, ALLOCATABLE :: apf_tmp_arr_2(:)
    ALLOCATE(apf_tmp_arr_2(SIZE(o % arr)))
    apf_tmp_v = apf_tmp_in % v
    apf_tmp_arr_2 = o % arr
    !$omp parallel do private(i) reduction(+:apf_tmp_v) schedule(static)
    !$omp parallel do private(i) reduction(+:apf_tmp_in % v) schedule(static)
      DO i = 1, n
      apf_tmp_v = apf_tmp_v + REAL(i)
      apf_tmp_arr_2(i) = REAL(i) * 2.0
    END DO
    apf_tmp_in % v = apf_tmp_v
    o % arr = apf_tmp_arr_2
    DEALLOCATE(apf_tmp_arr_2)
    !$omp end parallel do
    !$omp end parallel do
    o % in = apf_tmp_in
    o % arr = apf_tmp_arr
  END SUBROUTINE work
END MODULE nested_type_mod