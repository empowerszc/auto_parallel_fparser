PROGRAM demo
  IMPLICIT NONE
  INTEGER :: i, n
  REAL :: a(100), b(100), s
  n = 100
  s = 0.0
  DO i = 1, n
    a(i) = b(i) + 1.0
    s = s + a(i)
  END DO
END PROGRAM demo