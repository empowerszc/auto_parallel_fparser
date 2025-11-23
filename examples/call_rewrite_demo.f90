MODULE m_call_rewrite_demo
  IMPLICIT NONE
  TYPE :: TInner
    REAL :: s
    REAL, ALLOCATABLE :: arr(:)
  END TYPE TInner
  TYPE :: TObj
    TYPE(TInner) :: in
  END TYPE TObj
  TYPE(TObj) :: x_global
CONTAINS
  SUBROUTINE callee(obj, i, a)
    IMPLICIT NONE
    TYPE(TObj), INTENT(INOUT) :: obj
    INTEGER, INTENT(IN) :: i
    REAL, INTENT(IN) :: a
    IF (ALLOCATED(obj%in%arr)) THEN
      obj%in%arr(i) = obj%in%arr(i) + a*REAL(i)
    END IF
    obj%in%s = obj%in%s + a
  END SUBROUTINE callee

  SUBROUTINE callee0()
    IMPLICIT NONE
    INTEGER :: i
    IF (ALLOCATED(x_global%in%arr)) THEN
      i = 1
      x_global%in%arr(i) = x_global%in%arr(i) + 1.0
    END IF
    x_global%in%s = x_global%in%s + 1.0
  END SUBROUTINE callee0

  SUBROUTINE driver(n)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n
    TYPE(TObj) :: x
    INTEGER :: i
    ALLOCATE(x%in%arr(n))
    x%in%s = 0.0
    x_global%in%s = 0.0
    ALLOCATE(x_global%in%arr(n))
    DO i = 1, n
      CALL callee(x, i, 0.1)
      CALL callee0
    END DO
  END SUBROUTINE driver
END MODULE m_call_rewrite_demo
