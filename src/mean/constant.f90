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

module constant_mean_module
  use mean_type, only: MeanType
  use kinds, only: wp
  use utils, only: mean
  implicit none
  private
  public :: ConstantMean

  type, extends(MeanType) :: ConstantMean
    real(kind=wp) :: mean
  contains
    private
    procedure, public, pass(self) :: init => init_constant_mean
    procedure, public, pass(self) :: value => value_constant_mean
    procedure, public, pass(self) :: write => write_constant_mean
  end type ConstantMean
  
contains

  subroutine init_constant_mean(self, x, y)
    class(ConstantMean), intent(inout) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
      real(kind=wp), dimension(:), intent(in) :: y

    self%mean = mean(y)
  end subroutine init_constant_mean

  subroutine value_constant_mean(self, x, mean)
    class(ConstantMean), intent(in) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
    real(kind=wp), dimension(:), intent(out) :: mean
    mean(:) = self%mean
  end subroutine value_constant_mean

  subroutine write_constant_mean(self, unit)
    class(ConstantMean), intent(in) :: self
    integer, intent(in) :: unit
    write(unit, '(A)') "[mean]"
    write(unit, '(A, 1x, A)') "type", "constant"
    write(unit, '(A, 1x, G0)') "value", self%mean
  end subroutine write_constant_mean

  function nparams_constant_mean(self) result(nparams)
    class(ConstantMean), intent(in) :: self
    integer :: nparams

    nparams = 1
  end function nparams_constant_mean

  function get_params_constant_mean(self) result(params)
    class(ConstantMean), intent(in) :: self
    real(kind=wp), dimension(:), allocatable :: params

    params = (/self%mean/)
  end function get_params_constant_mean

  subroutine set_params_constant_mean(self, params)
    class(ConstantMean), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: params

    self%mean = params(1)
  end subroutine set_params_constant_mean
end module constant_mean_module