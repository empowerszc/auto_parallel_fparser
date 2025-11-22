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
    REAL :: apf_tmp_v
    apf_tmp_v = o % in % v
    REAL :: apf_tmp_arr(SIZE(o % arr))
    apf_tmp_arr = o % arr
    INTEGER, ALLOCATABLE :: apf_tmp_i(:)
    ALLOCATE(apf_tmp_i(SIZE(apf_tmp_arr % i)))
    apf_tmp_i = apf_tmp_arr % i
    !$omp parallel do private(i) reduction(+:apf_tmp_v) schedule(static)
    !$omp parallel do private(i) reduction(+:apf_tmp_v) schedule(static)
      DO i = 1, n
      apf_tmp_v = apf_tmp_v + REAL(i)
      apf_tmp_arr(i) = REAL(i) * 2.0
    END DO
    apf_tmp_arr % i = apf_tmp_i
    DEALLOCATE(apf_tmp_i)
    !$omp end parallel do
    !$omp end parallel do
    o % in % v = apf_tmp_v
    o % arr = apf_tmp_arr
  END SUBROUTINE work
END MODULE nested_type_mod