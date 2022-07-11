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

module mean_config_module
  use config_type_module, only: ConfigType
  use config_error, only: error_data
  use tomlf, only: toml_table
  implicit none
  private
  public :: MeanConfig

  type, abstract, extends(ConfigType) :: MeanConfig
  contains
    procedure(init_interface), public, deferred, pass(self) :: init
  end type MeanConfig

  abstract interface
    subroutine init_interface(self, table, error)
      import MeanConfig
      import toml_table
      import error_data
      class(MeanConfig), intent(inout) :: self
      type(toml_table), intent(inout) :: table
      type(error_data), allocatable, intent(out) :: error
    end subroutine init_interface
  end interface
end module mean_config_module