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
  do i = 1, n
    o%in%v = o%in%v + real(i)
    o%arr(i) = real(i) * 2.0
  end do
end subroutine work
end module nested_type_mod