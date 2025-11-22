PROGRAM demo
  IMPLICIT NONE
  INTEGER :: i, n
  REAL :: a(100), b(100), s
  n = 100
  s = 0.0
  INTEGER, ALLOCATABLE :: apf_tmp_i(:)
  ALLOCATE(apf_tmp_i(SIZE(a % i)))
  apf_tmp_i = a % i
  !$omp parallel do private(i) reduction(+:s) schedule(static)
  !$omp parallel do private(i) reduction(+:s)
    DO i = 1, n
    a(i) = b(i) + 1.0
    s = s + a(i)
  END DO
  a % i = apf_tmp_i
  DEALLOCATE(apf_tmp_i)
  !$omp end parallel do
  !$omp end parallel do
END PROGRAM demo