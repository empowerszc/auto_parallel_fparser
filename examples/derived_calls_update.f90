module m_calls_update
  implicit none
  type :: TInner
    real :: s
    real, allocatable :: arr(:)
  end type TInner
  type :: TObj
    type(TInner) :: in
    real :: t
  end type TObj
contains
  subroutine upd1(obj, i, alpha)
    implicit none
    type(TObj), intent(inout) :: obj
    integer, intent(in) :: i
    real, intent(in) :: alpha
    if (obj%in%s > 0.0) then
      obj%in%s = obj%in%s + alpha
    else
      obj%in%s = obj%in%s - alpha
    end if
    if (allocated(obj%in%arr)) then
      if (mod(i,2) == 0) then
        obj%in%arr(i) = obj%in%arr(i) + alpha*real(i)
      else
        obj%in%arr(i) = obj%in%arr(i) - alpha*real(i)
      end if
    end if
    obj%t = obj%t + 1.0
  end subroutine upd1

  subroutine upd2(obj, i, beta)
    implicit none
    type(TObj), intent(inout) :: obj
    integer, intent(in) :: i
    real, intent(in) :: beta
    if (beta > 1.0) then
      obj%in%s = obj%in%s + beta
    end if
    if (allocated(obj%in%arr)) then
      obj%in%arr(i) = obj%in%arr(i) + beta
    end if
    obj%t = obj%t + beta
  end subroutine upd2

  subroutine driver(n)
    implicit none
    integer, intent(in) :: n
    type(TObj) :: x
    integer :: i
    allocate(x%in%arr(n))
    x%in%s = 0.0
    x%t = 0.0
    do i = 1, n
      if (i < n/2) then
        call upd1(x, i, 0.5)
      else
        call upd2(x, i, 1.5)
      end if
    end do
  end subroutine driver
end module m_calls_update