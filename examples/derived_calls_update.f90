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
    if (mod(i,2) == 0) then
      obj%sum_val = obj%sum_val + 2.0
    else
      obj%sum_val = obj%sum_val + 1.0
    end if
    if (obj%sum_val > 10.0) then
      obj%a(i) = obj%a(i) + real(i)
    else
      obj%a(i) = obj%a(i) + 0.5*real(i)
    end if
  end subroutine update_obj

  subroutine do_work(n)
    implicit none
    integer, intent(in) :: n
    type(T1) :: obj
    integer :: i
    allocate(obj%a(n))
    obj%sum_val = 0.0
    do i = 1, n
      if (i <= n/2) then
        call update_obj(obj, i)
      else
        call update_obj(obj, i)
      end if
    end do
  end subroutine do_work
end module m_calls