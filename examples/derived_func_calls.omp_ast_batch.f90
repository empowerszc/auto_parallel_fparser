MODULE func_type_mod
  IMPLICIT NONE
  TYPE :: T1
    REAL :: x
    REAL, ALLOCATABLE :: a(:)
  END TYPE T1
  CONTAINS
  SUBROUTINE update_scalar(v)
    REAL, INTENT(INOUT) :: v
    v = v + 1.0
  END SUBROUTINE update_scalar
  SUBROUTINE update_array(v)
    REAL, INTENT(INOUT) :: v
    v = v * 2.0
  END SUBROUTINE update_array
  SUBROUTINE run(t, n)
    TYPE(T1), INTENT(INOUT) :: t
    INTEGER, INTENT(IN) :: n
    INTEGER :: i
    ALLOCATE(t % a(n))
    DO i = 1, n
      CALL update_scalar(t % x)
      CALL update_array(t % a(i))
    END DO
  END SUBROUTINE run
END MODULE func_type_mod