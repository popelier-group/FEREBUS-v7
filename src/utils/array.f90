module utils_array
  use kinds, only: wp
  implicit none
  private

  public :: stack, identity, ones, zeros, print_matrix, euclidean_distance

  integer, parameter :: ROW_MAJOR = 1
  integer, parameter :: COLUMN_MAJOR = 2

  interface zeros
    module procedure zero_array
    module procedure zero_matrix
  end interface zeros

  interface print_matrix
    module procedure print_arr_real
    module procedure print_arr_int
    module procedure print_matrix_real
    module procedure print_matrix_int
    module procedure print_matrix3_real
    module procedure print_matrix3_int
  end interface print_matrix
contains
  function stack(arr1, arr2) result(stacked_arr)
    real(kind=wp), dimension(:), intent(in) :: arr1, arr2
    real(kind=wp), dimension(:), allocatable :: stacked_arr
    integer :: n1, n2

    n1 = size(arr1)
    n2 = size(arr2)

    allocate(stacked_arr(n1+n2))
    stacked_arr(1:n1) = arr1
    stacked_arr(n1+1:n1+n2) = arr2
  end function stack

  function identity(n) result(identity_matrix)
    integer, intent(in) :: n
    real(kind=wp), dimension(:,:), allocatable :: identity_matrix
    integer :: i

    allocate(identity_matrix(n, n))
    identity_matrix = 0.0_wp
    do i = 1, n
      identity_matrix(i,i) = 1.0_wp
    end do
  end function

  function ones(n) result(ones_arr)
    integer, intent(in) :: n
    real(kind=wp), dimension(:), allocatable :: ones_arr

    allocate(ones_arr(n))
    ones_arr = 1.0_wp
  end function

  function zero_array(n) result(zero)
    integer, intent(in) :: n
    real(kind=wp), dimension(:), allocatable :: zero
    allocate(zero(n))
    zero = 0.0_wp
  end function zero_array

  function zero_matrix(n, m) result(zero)
    integer, intent(in) :: n, m
    real(kind=wp), dimension(:,:), allocatable :: zero
    allocate(zero(n,m))
    zero = 0.0_wp
  end function zero_matrix

  subroutine print_arr_real(arr, order)
    real(kind=wp), dimension(:), intent(in) :: arr
    integer, optional, intent(in) :: order
    integer :: order_
    integer :: i, m

    order_ = ROW_MAJOR
    if (present(order)) order_ = order

    m = size(arr, 1)
    write(* , "(*(g0))") (arr(i), new_line("A"), i=1,m)
  end subroutine print_arr_real

  subroutine print_arr_int(arr, order)
    integer, dimension(:), intent(in) :: arr
    integer, optional, intent(in) :: order
    integer :: order_
    integer :: i, m

    order_ = ROW_MAJOR
    if (present(order)) order_ = order

    m = size(arr, 1)
    write(* , "(*(i0))") (arr(i), new_line("A"), i=1,m)
  end subroutine print_arr_int

  subroutine print_matrix_real(matrix, order)
    real(kind=wp), dimension(:,:), intent(in) :: matrix
    integer, optional, intent(in) :: order
    integer :: order_
    integer :: i, j, m, n

    order_ = ROW_MAJOR
    if (present(order)) order_ = order

    n = size(matrix, 1)
    m = size(matrix, 2)
    if (order_ .eq. ROW_MAJOR) then
      write(* , "(*(g0))") ((matrix(j,i), " ", j=1,m), new_line("A"), i=1,n)
    else
      write(* , "(*(g0))") ((matrix(i,j), " ", j=1,m), new_line("A"), i=1,n)
    end if 
  end subroutine print_matrix_real

  subroutine print_matrix_int(matrix, order)
    integer, dimension(:,:), intent(in) :: matrix
    integer, optional, intent(in) :: order
    integer :: order_
    integer :: i, j, m, n

    order_ = ROW_MAJOR
    if (present(order)) order_ = order

    n = size(matrix, 1)
    m = size(matrix, 2)
    if (order_ .eq. ROW_MAJOR) then
      write(* , "(*(i0))") ((matrix(j,i), " ", j=1,m), new_line("A"), i=1,n)
    else
      write(* , "(*(i0))") ((matrix(i,j), " ", j=1,m), new_line("A"), i=1,n)
    end if
  end subroutine print_matrix_int

  subroutine print_matrix3_real(matrix, order)
    real(kind=wp), dimension(:,:,:), intent(in) :: matrix
    integer, optional, intent(in) :: order
    integer :: order_
    integer :: i, j, k, m, n, o

    order_ = ROW_MAJOR
    if (present(order)) order_ = order

    n = size(matrix, 1)
    m = size(matrix, 2)
    o = size(matrix, 3)
    if (order_ .eq. ROW_MAJOR) then
      do k = 1, n
        write(* , "(*(g0))") ((matrix(k,j,i), " ", j=1,o), new_line("A"), i=1,m)
      end do
    else  
      do k = 1, n
        write(* , "(*(g0))") ((matrix(k,i,j), " ", j=1,o), new_line("A"), i=1,m)
      end do
    end if
  end subroutine print_matrix3_real

  subroutine print_matrix3_int(matrix, order)
    integer, dimension(:,:,:), intent(in) :: matrix
    integer, optional, intent(in) :: order
    integer :: order_
    integer :: i, j, k, m, n, o

    order_ = ROW_MAJOR
    if (present(order)) order_ = order

    n = size(matrix, 1)
    m = size(matrix, 2)
    o = size(matrix, 3)
    if (order_ .eq. ROW_MAJOR) then
      do k = 1, n
        write(* , "(*(i0))") ((matrix(k,j,i), " ", j=1,o), new_line("A"), i=1,m)
      end do
    else
      do k = 1, n
        write(* , "(*(i0))") ((matrix(k,i,j), " ", j=1,o), new_line("A"), i=1,m)
      end do
    end if
  end subroutine print_matrix3_int

  function euclidean_distance(v1, v2) result(distance)
    real(kind=wp), dimension(:), intent(in) :: v1, v2
    real(kind=wp) :: distance
    real(kind=wp), dimension(:), allocatable :: diff

    diff = v1 - v2
    distance = sqrt(sum(diff*diff))
  end function euclidean_distance
end module utils_array
