module m_calls
  implicit none
  type :: T1
    real :: sum_val
    real, allocatable :: a(:)
  end type T1
contains
  subroutine update_obj(obj, i)
    implicit none
    type(T1), intent(inout) :: obj
    integer, intent(in) :: i
    obj%sum_val = obj%sum_val + 1.0
    obj%a(i) = obj%a(i) + real(i)
  end subroutine update_obj

  subroutine do_work(n)
    implicit none
    integer, intent(in) :: n
    type(T1) :: obj
    integer :: i
    allocate(obj%a(n))
    obj%sum_val = 0.0
    do i = 1, n
      call update_obj(obj, i)
    end do
  end subroutine do_work
end module m_calls