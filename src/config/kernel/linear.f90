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

module linear_config_module
    use tomlf, only: toml_table
    use utils, only: get_value_converted
    use kernel_config_type_module, only: KernelConfig
    use config_error, only: error_data
  
    implicit none
    private
    public :: LinearConfig
  
    type, extends(KernelConfig) :: LinearConfig
      real(kind=wp), allocatable, dimension(:) :: c
    contains
      private
      procedure, public, pass(self) :: init => init_lin_config
      procedure, public, pass(self) :: info => info_lin
    end type LinearConfig
  
  contains
  
    subroutine init_lin_config(self, table, error)
      class(LinearConfig), intent(inout) :: self
      type(toml_table), intent(inout) :: table
      type(error_data), allocatable, intent(out) :: error
  
      call table%get_key(self%name)
      call get_value_converted(table, "active_dimensions", self%active_dims)
      call get_value_converted(table, "c", self%c)
    end subroutine init_lin_config
  
    subroutine info_lin(self, unit)
      class(PeriodicConfig), intent(in) :: self
      integer, intent(in) :: unit
      integer :: i
  
      write(unit, '(A)') "Periodic Config"
      write(unit, '(" - ", A, T30, A)') "Name", self%name
      if (allocated(self%active_dims)) then
        write(unit, '(" - ", A, T30, (*(I0, 1x)))') "Active Dimensions", (self%active_dims(i), i=1,size(self%active_dims))
      end if
      if (allocated(self%lengthscale)) then
        write(unit, '(" - ", A, T30, (*(G0, 1x)))') "C", (self%lengthscale(i), i=1,size(self%c))
      end if
    end subroutine info_lin
end module linear_config_module