module m_calls_args
  implicit none
  type :: T2
    real :: s
    real, allocatable :: arr(:)
    real :: t
  end type T2
contains
  subroutine heavy_update(x, y, obj, idx, beta)
    implicit none
    real, intent(in) :: x, y
    type(T2), intent(inout) :: obj
    integer, intent(in) :: idx
    real, intent(in) :: beta
    obj%s = obj%s + x + y
    obj%t = obj%t + beta
    if (allocated(obj%arr)) then
      obj%arr(idx) = obj%arr(idx) + beta*real(idx)
    end if
  end subroutine heavy_update

  subroutine run(n)
    implicit none
    integer, intent(in) :: n
    type(T2) :: obj
    integer :: i
    allocate(obj%arr(n))
    obj%s = 0.0
    obj%t = 0.0
    do i = 1, n
      call heavy_update(1.0, 2.0, obj, i, 0.25)
    end do
  end subroutine run
end module m_calls_args