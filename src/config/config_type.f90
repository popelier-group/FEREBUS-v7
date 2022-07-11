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

module config_type_module
  use writable_module, only: Writable
  implicit none
  private
  public :: ConfigType, WritableConfigType

  type, abstract :: ConfigType
  contains
    private
    procedure(info_interface), public, pass(self), deferred :: info  
  end type ConfigType

  type, abstract, extends(Writable) :: WritableConfigType
  contains
    private
    procedure(writable_info_interface), public, pass(self), deferred :: info  
  end type WritableConfigType

  abstract interface
    subroutine info_interface(self, unit)
      import ConfigType
      implicit none
      class(ConfigType), intent(in) :: self
      integer, intent(in) :: unit
    end subroutine info_interface

    subroutine writable_info_interface(self, unit)
      import WritableConfigType
      implicit none
      class(WritableConfigType), intent(in) :: self
      integer, intent(in) :: unit
    end subroutine writable_info_interface
  end interface

end module config_type_module