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

module constant_mean_config_module
  use mean_config_module, only: MeanConfig
  use config_error, only: error_data
  use tomlf, only: toml_table
  implicit none
  private
  public :: ConstantMeanConfig

  type, extends(MeanConfig) :: ConstantMeanConfig
  contains
    private
    procedure, public, pass(self) :: init => init_constant_mean
    procedure, public, pass(self) :: info
  end type ConstantMeanConfig

contains
  
  subroutine init_constant_mean(self, table, error)
    class(ConstantMeanConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_data), allocatable, intent(out) :: error
  end subroutine init_constant_mean

  subroutine info(self, unit)
    class(ConstantMeanConfig), intent(in) :: self
    integer, intent(in) :: unit

    write(unit, '(A)') "Mean Config"
    write(unit, '(" - ", A, T30, A)') "Type", "Constant"
  end subroutine info
end module constant_mean_config_module