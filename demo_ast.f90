PROGRAM demo
  IMPLICIT NONE
  INTEGER :: i, n
  REAL, DIMENSION(:), ALLOCATABLE :: a, b
  n = 1000
  ALLOCATE(a(n), b(n))
  a = 1.0
  b = 2.0
  !$omp parallel do private(i) schedule(static)
  DO i = 1, n
    a(i) = a(i) + b(i)
  END DO
  !$omp end parallel do
  PRINT *, a(1)
  DEALLOCATE(a, b)
END PROGRAM demo