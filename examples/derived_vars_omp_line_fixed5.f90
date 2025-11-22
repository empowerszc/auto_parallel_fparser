module custom_type_mod
    implicit none

    ! 定义自定义类型：包含标量（用于归约结果）和数组（用于数据存储）
    type :: MyType
        real :: sum_val    ! 标量：用于求和归约
        real :: max_val    ! 标量：用于最大值归约
        real, allocatable :: data_arr(:)  ! 数组：原始数据
        real, allocatable :: prod_arr(:)  ! 数组：用于乘积归约
    end type MyType

contains

    ! 初始化自定义类型对象：分配数组并设置初始值
    subroutine init_obj(obj, n)
        type(MyType), intent(out) :: obj
        integer, intent(in) :: n
        integer :: i
        allocate(obj%data_arr(n), obj%prod_arr(n))
        ! 初始化原始数据数组（1到n的平方）
!$omp parallel do private(i) schedule(static)
        do i = 1, n
            obj%data_arr(i) = real(i**2)
        end do
        obj%sum_val = 0.0  ! 归约变量初始化
!$omp end parallel do
!$omp end parallel do
!$omp end parallel do
!$omp end parallel do
!$omp end parallel do
        obj%max_val = -huge(0.0)  ! 初始化为极小值
        obj%prod_arr = 1.0  ! 乘积归约初始化（乘法单位元）
    end subroutine init_obj

    ! 对自定义类型成员执行归约计算和依赖赋值
    subroutine process_obj(obj)
        type(MyType), intent(inout) :: obj
        integer :: i, n
        type(MyType) :: temp_obj  ! 临时对象，用于跨对象赋值
        
        n = size(obj%data_arr)
        allocate(temp_obj%data_arr(n), temp_obj%prod_arr(n))
        temp_obj%sum_val = 0.0
        temp_obj%max_val = -huge(0.0)

        ! 1. 标量归约：求和（仅累加正数，条件归约）
        !    sum_val 是归约变量（多次写入，输出依赖）
        do i = 1, n
            if (obj%data_arr(i) > 5.0) then  ! 条件筛选
                obj%sum_val = obj%sum_val + obj%data_arr(i)  ! 归约：输出依赖（i-1 → i）
            end if
        end do

        ! 2. 数组归约：乘积（prod_arr(i) 是前i个元素的乘积）
        !    流依赖：prod_arr(i) 依赖 prod_arr(i-1)
        do i = 1, n
            if (i == 1) then
                obj%prod_arr(i) = obj%data_arr(i)  ! 初始值
            else
                obj%prod_arr(i) = obj%prod_arr(i-1) * obj%data_arr(i)  ! 流依赖（i-1 → i）
            end if
        end do

        ! 3. 标量归约：最大值（跟踪全局最大值）
        !    输出依赖：max_val 可能被多次更新
        do i = 1, n
            if (obj%data_arr(i) > obj%max_val) then
                obj%max_val = obj%data_arr(i)  ! 归约：输出依赖（i-1 → i，条件性）
            end if
        end do

        ! 4. 依赖赋值：基于当前对象成员给临时对象赋值（跨对象依赖）
        !    流依赖：temp_obj 成员依赖 obj 成员
        do i = 1, n
            temp_obj%data_arr(i) = obj%prod_arr(i) + obj%sum_val  ! 流依赖（obj → temp_obj）
        end do

        ! 5. 自依赖赋值：当前对象数组元素依赖自身前序元素（含步长2的跨迭代依赖）
        do i = 3, n, 2  ! 步长为2：i=3,5,7...
            obj%data_arr(i) = obj%data_arr(i-2) * 0.5  ! 流依赖（i-2 → i）
        end do

        ! 6. 输出依赖：同一元素被多次写入（覆盖赋值）
        do i = 1, n
            obj%prod_arr(i) = 2.0 * obj%data_arr(i)  ! 输出依赖（与步骤2的prod_arr(i)冲突）
        end do

        deallocate(temp_obj%data_arr, temp_obj%prod_arr)
    end subroutine process_obj

end module custom_type_mod
