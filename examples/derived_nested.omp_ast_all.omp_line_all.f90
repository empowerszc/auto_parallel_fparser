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
    ALLOCATE(o % arr(n))
    REAL :: apf_tmp_in
    REAL, ALLOCATABLE :: apf_tmp_arr(:)
    ALLOCATE(apf_tmp_arr(SIZE(o % arr)))
    apf_tmp_in = o % in
    apf_tmp_arr = o % arr
    !$omp parallel do private(i) reduction(+:apf_tmp_in % v) schedule(static)
    DO i = 1, n
      apf_tmp_in % v = apf_tmp_in % v + REAL(i)
      apf_tmp_arr(i) = REAL(i) * 2.0
    END DO
!$omp end parallel do
    o % in = apf_tmp_in
    o % arr = apf_tmp_arr
    DEALLOCATE(apf_tmp_arr)
  END SUBROUTINE work
END MODULE nested_type_mod
