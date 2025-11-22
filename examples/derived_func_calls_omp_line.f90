module func_type_mod
implicit none
type :: T1
  real :: x
  real, allocatable :: a(:)
end type T1
contains
subroutine update_scalar(v)
  real, intent(inout) :: v
  v = v + 1.0
end subroutine update_scalar
subroutine update_array(v)
  real, intent(inout) :: v
  v = v * 2.0
end subroutine update_array
subroutine run(t, n)
  type(T1), intent(inout) :: t
  integer, intent(in) :: n
  integer :: i
  allocate(t%a(n))
  do i = 1, n
    call update_scalar(t%x)
    call update_array(t%a(i))
  end do
end subroutine run
end module func_type_mod