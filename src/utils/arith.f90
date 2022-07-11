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

module utils_arith
  use kinds, only: wp
  implicit none
  private
  public :: mean, std, standardise, log_determinant, modulo

  interface log_determinant
    module procedure log_determinant_cholesky
    ! module procedure log_determinant_lu
  end interface log_determinant

  interface std
    module procedure std_with_mean
    module procedure std_without_mean
  end interface std

  interface modulo
    module procedure modulo_real_int
    module procedure modulo_real_real
  end interface modulo
contains
  function mean(arr) result(m)
    real(kind=wp), dimension(:), intent(in) :: arr
    real(kind=wp) :: m
    m = sum(arr)/real(size(arr), kind=wp)
  end function mean

  function std_without_mean(arr) result(s)
    real(kind=wp), dimension(:), intent(in) :: arr
    real(kind=wp) :: s
    s = std(arr, mean(arr))
  end function std_without_mean

  function std_with_mean(arr, m) result(s)
    real(kind=wp), dimension(:), intent(in) :: arr
    real(kind=wp), intent(in) :: m
    real(kind=wp) :: s
    s = sqrt(sum((arr-m)*(arr-m))/size(arr))
  end function std_with_mean

  function standardise(arr, m, s) result(r)
    real(kind=wp), dimension(:), intent(in) :: arr
    real(kind=wp), intent(in) :: m
    real(kind=wp), intent(in) :: s
    real(kind=wp), dimension(size(arr)) :: r

    r = (arr-m)/s
  end function standardise

  function log_determinant_cholesky(lower) result(log_determinant)
    real(kind=wp), dimension(:,:), intent(in) :: lower
    real(kind=wp) :: log_determinant

    integer :: i
    
    log_determinant = 0.0_wp
    do i = 1, size(lower, 1)
      log_determinant = log_determinant + log(lower(i,i))
    end do
    log_determinant = 2.0_wp*log_determinant

    !-----------------------------------
    ! log_determinant = 1.0_wp
    !!$omp parallel do reduction(*:log_determinant)
    ! do i = 1, size(lower, 1)
    !   log_determinant = log_determinant * lower(i,i)
    ! end do
    !!$omp end parallel do
    ! log_determinant = 2.0_wp*log(log_determinant)
    !-----------------------------------
  end function log_determinant_cholesky

  function modulo_real_int(a, n) result(b)
    real(kind=wp), intent(in) :: a
    integer, intent(in) :: n
    real(kind=wp) :: b
    b = a - floor(a/real(n, kind=wp), kind=wp) * real(n, kind=wp)
  end function modulo_real_int

  function modulo_real_real(a, n) result(b)
    real(kind=wp), intent(in) :: a
    real(kind=wp), intent(in) :: n
    real(kind=wp) :: b
    b = a - floor(a/n, kind=wp) * n
  end function modulo_real_real
end module utils_arith
