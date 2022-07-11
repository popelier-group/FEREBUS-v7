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

module kernel_config_type_module
  use config_type_module, only : ConfigType
  use config_error, only: error_data
  use tomlf, only: toml_table
  implicit none
  private
  public :: KernelConfig
 
  type, abstract, extends(ConfigType) :: KernelConfig
    character(len=:), allocatable :: name
    integer, allocatable, dimension(:) :: active_dims
    integer :: ndim = -1
  contains
    private
    procedure(init_interface), public, deferred, pass(self) :: init
    procedure, public, pass(self) :: set_ndim
  end type KernelConfig

  abstract interface
    subroutine init_interface(self, table, error)
      import KernelConfig
      import toml_table
      import error_data
      class(KernelConfig), intent(inout) :: self
      type(toml_table), intent(inout) :: table
      type(error_data), allocatable, intent(out) :: error
    end subroutine init_interface
  end interface

contains

  subroutine set_ndim(self, ndim)
    class(KernelConfig), intent(inout) :: self
    integer, intent(in) :: ndim
    integer :: i

    if (self%ndim .eq. -1) then
      if (.not.allocated(self%active_dims)) then
        self%ndim = ndim
      else
        self%ndim = size(self%active_dims)
      end if
    end if

    if (.not.allocated(self%active_dims)) then
      allocate(self%active_dims(self%ndim))
      do i = 1, self%ndim
        self%active_dims(i) = i
      end do
    end if
  end subroutine set_ndim
end module kernel_config_type_module
