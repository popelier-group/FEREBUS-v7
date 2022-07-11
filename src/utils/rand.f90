! MIT License
!
! Copyright (c) 2022 Popelier Group
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.

module utils_rand
  use kinds, only: wp
  implicit none
  private
  public :: init_rand, random

  interface random
    module procedure random_val
    module procedure random_val_max
    module procedure random_val_min_max
    module procedure random_arr
    module procedure random_arr_max
    module procedure random_arr_min_max
    module procedure random_arr_max_arr
    module procedure random_arr_min_max_arr
    module procedure random_mat
    module procedure random_mat_max
    module procedure random_mat_min_max
    module procedure random_mat_max_mat
    module procedure random_mat_min_max_mat
    module procedure random_mat_min_max_arr
  end interface random                                                                                                             

contains

  subroutine init_rand()
    integer, allocatable :: seed(:)                                                                                         
    integer :: is,i,p  

    is = 13                                                                                                                         
    call random_seed(size=p)                                                                                                        
    allocate(seed(p))                                                                                                               
    seed = 17*[(i-is,i=1,p)]                                                                                                        
    call random_seed(put=seed)                                                                                                      
    deallocate(seed)
  end subroutine init_rand
  
  subroutine random_val(val)
    real(kind=wp), intent(out) :: val
    call random_number(val)
  end subroutine random_val

  subroutine random_val_max(val, max)
    real(kind=wp), intent(out) :: val
    real(kind=wp), intent(in) :: max
    call random_number(val)
    val = val*max
  end subroutine random_val_max

  subroutine random_val_min_max(val, min, max)
    real(kind=wp), intent(out) :: val
    real(kind=wp), intent(in) :: min
    real(kind=wp), intent(in) :: max
    call random_number(val)
    val = min + val*(max-min)
  end subroutine random_val_min_max

  subroutine random_arr(arr, n)
    real(kind=wp), dimension(:), allocatable, intent(out)  :: arr
    integer, intent(in) :: n
    allocate(arr(n))
    call random_number(arr)
  end subroutine random_arr

  subroutine random_arr_max(arr, n, max)
    real(kind=wp), dimension(:), allocatable, intent(out)  :: arr
    integer, intent(in) :: n
    real(kind=wp), intent(in) :: max
    allocate(arr(n))
    call random_number(arr)
    arr = arr*max
  end subroutine random_arr_max

  subroutine random_arr_min_max(arr, n, min, max)
    real(kind=wp), dimension(:), allocatable, intent(out)  :: arr
    integer, intent(in) :: n
    real(kind=wp), intent(in) :: min
    real(kind=wp), intent(in) :: max
    allocate(arr(n))
    call random_number(arr)
    arr = min + arr*(max-min)
  end subroutine random_arr_min_max

  subroutine random_arr_max_arr(arr, n, max)
    real(kind=wp), dimension(:), allocatable, intent(out)  :: arr
    integer, intent(in) :: n
    real(kind=wp), dimension(n), intent(in) :: max
    allocate(arr(n))
    call random_number(arr)
    arr = arr*max
  end subroutine random_arr_max_arr

  subroutine random_arr_min_max_arr(arr, n, min, max)
    real(kind=wp), dimension(:), allocatable, intent(out)  :: arr
    integer, intent(in) :: n
    real(kind=wp), dimension(n), intent(in) :: min
    real(kind=wp), dimension(n), intent(in) :: max
    allocate(arr(n))
    call random_number(arr)
    arr = min + arr*(max-min)
  end subroutine random_arr_min_max_arr

  subroutine random_mat(mat, n, m)
    integer, intent(in) :: n, m
    real(kind=wp), dimension(:,:), allocatable, intent(out)  :: mat
    allocate(mat(n,m))
    call random_number(mat)
  end subroutine random_mat

  subroutine random_mat_max(mat, n, m, max)
    real(kind=wp), dimension(:,:), allocatable, intent(out)  :: mat
    integer, intent(in) :: n, m
    real(kind=wp), intent(in) :: max
    allocate(mat(n,m))
    call random_number(mat)
    mat = mat*max
  end subroutine random_mat_max

  subroutine random_mat_min_max(mat, n, m, min, max)
    real(kind=wp), dimension(:,:), allocatable, intent(out)  :: mat
    integer, intent(in) :: n, m
    real(kind=wp), intent(in) :: min
    real(kind=wp), intent(in) :: max
    allocate(mat(n,m))
    call random_number(mat)
    mat = min + mat*(max-min)
  end subroutine random_mat_min_max

  subroutine random_mat_max_mat(mat, n, m, max)
    real(kind=wp), dimension(:,:), allocatable, intent(out)  :: mat
    integer, intent(in) :: n, m
    real(kind=wp), dimension(n,m), intent(in) :: max
    allocate(mat(n,m))
    call random_number(mat)
    mat = mat*max
  end subroutine random_mat_max_mat

  subroutine random_mat_min_max_arr(mat, n, m, min, max)
    real(kind=wp), dimension(:,:), allocatable, intent(out)  :: mat
    integer, intent(in) :: n, m
    real(kind=wp), dimension(n), intent(in) :: min
    real(kind=wp), dimension(n), intent(in) :: max
    integer :: i
    allocate(mat(n,m))
    call random_number(mat)
    do i = 1, n
      mat(i,:) = min + mat(i,:)*(max-min)
    end do
  end subroutine random_mat_min_max_arr

  subroutine random_mat_min_max_mat(mat, n, m, min, max)
    real(kind=wp), dimension(:,:), allocatable, intent(out)  :: mat
    integer, intent(in) :: n, m
    real(kind=wp), dimension(n,m), intent(in) :: min
    real(kind=wp), dimension(n,m), intent(in) :: max
    allocate(mat(n,m))
    call random_number(mat)
    mat = min + mat*(max-min)
  end subroutine random_mat_min_max_mat
end module utils_rand