MODULE m_calls_deep
  IMPLICIT NONE
  TYPE :: Inner
    REAL :: v
    REAL, ALLOCATABLE :: a(:)
  END TYPE Inner
  TYPE :: Mid
    TYPE(Inner) :: in
    REAL :: w
  END TYPE Mid
  TYPE :: Outer
    TYPE(Mid) :: m
    REAL :: z
  END TYPE Outer
  CONTAINS
  SUBROUTINE touch_chain(o, i, alpha)
    IMPLICIT NONE
    TYPE(Outer), INTENT(INOUT) :: o
    INTEGER, INTENT(IN) :: i
    REAL, INTENT(IN) :: alpha
    o % m % in % v = o % m % in % v + alpha
    IF (ALLOCATED(o % m % in % a)) THEN
      o % m % in % a(i) = o % m % in % a(i) + alpha * REAL(i)
    END IF
    o % m % w = o % m % w + 1.0
    o % z = o % z + 2.0
  END SUBROUTINE touch_chain

    SUBROUTINE driver(n)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n
    TYPE(Outer) :: o
    INTEGER :: i
    ALLOCATE(o % m % in % a(n))
    o % m % in % v = 0.0
    o % m % w = 0.0
    o % z = 0.0
    !$omp parallel do private(i) schedule(static)
    DO i = 1, n
      CALL touch_chain(o, i, 0.5)
    END DO
    !$omp end parallel do
  END SUBROUTINE driver
END MODULE m_calls_deep