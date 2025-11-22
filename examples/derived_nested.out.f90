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
    TYPE(Outer), INTENT(INOUT) :: o
    INTEGER, INTENT(IN) :: n
    INTEGER :: i
    REAL :: apf_tmp_in
    ALLOCATE(apf_tmp_arr(SIZE(o % arr)))
    apf_tmp_in = o % in
    apf_tmp_arr = o % arr
    REAL, ALLOCATABLE :: apf_tmp_arr(:)
    ALLOCATE(o % arr(n))
    !$omp parallel do private(i) reduction(+:apf_tmp_in % v) schedule(static)
    DO i = 1, n
      apf_tmp_in % v = apf_tmp_in % v + REAL(i)
      apf_tmp_arr(i) = REAL(i) * 2.0
    END DO
    o % in = apf_tmp_in
    o % arr = apf_tmp_arr
    DEALLOCATE(apf_tmp_arr)
    !$omp end parallel do
  END SUBROUTINE work
END MODULE nested_type_mod