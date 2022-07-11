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

module zero_mean_module
  use mean_type, only: MeanType
  use kinds, only: wp
  use utils, only: mean
  implicit none
  private
  public :: ZeroMean

  type, extends(MeanType) :: ZeroMean
  contains
    private
    procedure, public, pass(self) :: init => init_zero_mean
    procedure, public, pass(self) :: value => value_zero_mean
    procedure, public, pass(self) :: write => write_zero_mean
  end type ZeroMean
  
contains

  subroutine init_zero_mean(self, x, y)
    class(ZeroMean), intent(inout) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
    real(kind=wp), dimension(:), intent(in) :: y
  end subroutine init_zero_mean

  subroutine value_zero_mean(self, x, mean)
    class(ZeroMean), intent(in) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
    real(kind=wp), dimension(:), intent(out) :: mean
    mean(:) = 0.0_wp
  end subroutine value_zero_mean

  subroutine write_zero_mean(self, unit)
    class(ZeroMean), intent(in) :: self
    integer, intent(in) :: unit
    write(unit, '(A)') "[mean]"
    write(unit, '(A, 1x, A)') "type", "zero"
  end subroutine write_zero_mean
end module zero_mean_module