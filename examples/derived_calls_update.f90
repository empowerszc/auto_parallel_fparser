module m_calls_update
  implicit none
  type :: TInner
    real :: s
    real, allocatable :: arr(:)
    integer :: k
    double precision :: d
  end type TInner
  type :: TObj
    type(TInner) :: in
    real :: t
    real, allocatable :: buf(:)
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
      obj%in%d = obj%in%d + dble(beta)
    elseif (beta == 1.0) then
      obj%in%s = obj%in%s - beta
    else
      obj%in%k = obj%in%k + i
    end if
    if (allocated(obj%in%arr)) then
      obj%in%arr(i) = obj%in%arr(i) + beta
    end if
    obj%t = obj%t + beta
  end subroutine upd2

  subroutine upd3(obj, i, gamma)
    implicit none
    type(TObj), intent(inout) :: obj
    integer, intent(in) :: i
    real, intent(in) :: gamma
    integer :: j
    do j = 1, 2
      select case (mod(i,4))
      case (0)
        if (allocated(obj%buf)) then
          obj%buf(i) = gamma*real(i+j)
        end if
      case (1)
        if (allocated(obj%in%arr)) then
          obj%in%arr(i) = obj%in%arr(i) + gamma
        end if
      case (2)
        obj%in%s = obj%in%s + gamma
      case default
        obj%in%k = obj%in%k + j
      end select
    end do
  end subroutine upd3

  subroutine driver(n)
    implicit none
    integer, intent(in) :: n
    type(TObj) :: x
    integer :: i, j
    allocate(x%in%arr(n))
    allocate(x%buf(n))
    x%in%s = 0.0
    x%in%k = 0
    x%in%d = 0.0d0
    x%t = 0.0
    do i = 1, n
      call upd1(x, i, 0.5)
      if (i < n/3) then
        call upd2(x, i, 1.5)
      elseif (i < 2*n/3) then
        call upd3(x, i, 2.0)
      else
        call upd2(x, i, 1.0)
        call upd3(x, i, 1.0)
      end if
      do j = 1, 2
        if (mod(i+j, 2) == 0) then
          call upd1(x, i, real(j))
        else
          call upd2(x, i, real(j))
        end if
      end do
      select case (mod(i,3))
      case (0)
        call upd3(x, i, 0.25)
      case (1)
        call upd1(x, i, 0.25)
      case default
        call upd2(x, i, 0.25)
      end select
    end do
  end subroutine driver
end module m_calls_update