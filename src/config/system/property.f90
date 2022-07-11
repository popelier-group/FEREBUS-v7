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

module property_config_module
  use config_type_module, only: ConfigType
  implicit none
  private
  public :: PropertyConfig

  type, extends(ConfigType) :: PropertyConfig
    character(len=:), allocatable :: name
  contains
    private
    procedure, public, pass(self) :: init => init_property_config
    procedure, public, pass(self) :: info
  end type PropertyConfig
  
  contains
  
  subroutine init_property_config(self, property_name)
    class(PropertyConfig), intent(inout) :: self
    character(len=*), intent(in) :: property_name

    self%name = property_name
  end subroutine init_property_config
    
  subroutine info(self, unit)
    class(PropertyConfig), intent(in) :: self
    integer, intent(in) :: unit

    if (allocated(self%name)) then
      write(unit, '(" - ", a, t30, a)') "Name", self%name
    end if
  end subroutine info
end module property_config_module