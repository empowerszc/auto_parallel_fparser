module m_calls_deep
  implicit none
  type :: Inner
    real :: v
    real, allocatable :: a(:)
  end type Inner
  type :: Mid
    type(Inner) :: in
    real :: w
  end type Mid
  type :: Outer
    type(Mid) :: m
    real :: z
  end type Outer
contains
  subroutine touch_chain(o, i, alpha)
    implicit none
    type(Outer), intent(inout) :: o
    integer, intent(in) :: i
    real, intent(in) :: alpha
    o%m%in%v = o%m%in%v + alpha
    if (allocated(o%m%in%a)) then
      o%m%in%a(i) = o%m%in%a(i) + alpha*real(i)
    end if
    o%m%w = o%m%w + 1.0
    o%z = o%z + 2.0
  end subroutine touch_chain

  subroutine driver(n)
    implicit none
    integer, intent(in) :: n
    type(Outer) :: o
    integer :: i
    allocate(o%m%in%a(n))
    o%m%in%v = 0.0
    o%m%w = 0.0
    o%z = 0.0
    do i = 1, n
      call touch_chain(o, i, 0.5)
    end do
  end subroutine driver
end module m_calls_deep