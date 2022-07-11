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

module constants
  use ieee_arithmetic, only: ieee_value, ieee_positive_inf
  use kinds, only: wp
  use, intrinsic :: iso_fortran_env, only : ERROR_UNIT, OUTPUT_UNIT
  implicit none

  integer, parameter :: MAX_OUTPUT_CHARACTER_LENGTH = 30 !> Remove need for this constant, use linked list of strings instead
  real(kind=wp), parameter :: PI = 4.0_wp*atan(1.0_wp)
  real(kind=wp), parameter :: TWOPI = 2.0_wp*PI
  real(kind=wp), parameter :: LOG_TWOPI = log(TWOPI)
  real(kind=wp), parameter :: MINUS_PI = -PI

  real(kind=wp), parameter :: SQRT_5 = sqrt(5.0_wp)
  real(kind=wp), parameter :: DIV_5_3 = 5.0_wp / 3.0_wp

  real(kind=wp), parameter :: MACHINE_PRECISION = epsilon(PI)

  public :: OUTPUT_UNIT, ERROR_UNIT

contains

  function INFINITY() result(inf) !> Would prefer ieee_positive_infinity but I couldn't get it working
    real(kind=wp) :: inf
    inf = ieee_value(0d0, ieee_positive_inf)
  end function INFINITY

end module constants
