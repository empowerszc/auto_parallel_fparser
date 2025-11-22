MODULE m_calls_args
  IMPLICIT NONE
  TYPE :: T2
    REAL :: s
    REAL, ALLOCATABLE :: arr(:)
    REAL :: t
  END TYPE T2
  CONTAINS
  SUBROUTINE heavy_update(x, y, obj, idx, beta)
    IMPLICIT NONE
    REAL, INTENT(IN) :: x, y
    TYPE(T2), INTENT(INOUT) :: obj
    INTEGER, INTENT(IN) :: idx
    REAL, INTENT(IN) :: beta
    obj % s = obj % s + x + y
    obj % t = obj % t + beta
    IF (ALLOCATED(obj % arr)) THEN
      obj % arr(idx) = obj % arr(idx) + beta * REAL(idx)
    END IF
  END SUBROUTINE heavy_update

    SUBROUTINE run(n)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n
    TYPE(T2) :: obj
    INTEGER :: i
    ALLOCATE(obj % arr(n))
    obj % s = 0.0
    obj % t = 0.0
    !$omp parallel do private(i) schedule(static)
    DO i = 1, n
      CALL heavy_update(1.0, 2.0, obj, i, 0.25)
    END DO
    !$omp end parallel do
  END SUBROUTINE run
END MODULE m_calls_args