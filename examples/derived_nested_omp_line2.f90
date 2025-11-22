module nested_type_mod
implicit none
type :: Inner
  real :: v
end type Inner
type :: Outer
  type(Inner) :: in
  real, allocatable :: arr(:)
end type Outer
contains
subroutine work(o, n)
  type(Outer), intent(inout) :: o
  integer, intent(in) :: n
  integer :: i
  allocate(o%arr(n))
REAL :: apf_tmp_v
apf_tmp_v = o % in % v
REAL :: apf_tmp_arr(SIZE(o % arr))
apf_tmp_arr = o % arr
!$omp parallel do private(i) reduction(+:apf_tmp_v) schedule(static)
  do i = 1, n
    apf_tmp_v = apf_tmp_v + real(i)
    apf_tmp_arr(i) = real(i) * 2.0
  end do
end subroutine work
!$omp end parallel do
o % in % v = apf_tmp_v
o % arr = apf_tmp_arr
end module nested_type_mod
