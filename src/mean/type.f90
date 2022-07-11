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

module mean_type
  use kinds, only: wp
  use writable_module, only: Writable
  implicit none
  private
  public :: MeanType

  type, abstract, extends(Writable) :: MeanType
  contains
    private
    procedure(init_interface), public, deferred, pass(self) :: init
    procedure(value_interface), public, deferred, pass(self) :: value
    procedure(write_interface), public, deferred, pass(self) :: write
    procedure, public, pass(self) :: nparams => nparams_interface
    procedure, public, pass(self) :: get_params => get_params_interface
    procedure, public, pass(self) :: set_params => set_params_interface
  end type MeanType

  abstract interface
    subroutine init_interface(self, x, y)
      import MeanType
      import wp
      class(MeanType), intent(inout) :: self
      real(kind=wp), dimension(:,:), intent(in) :: x
      real(kind=wp), dimension(:), intent(in) :: y
    end subroutine init_interface

    subroutine value_interface(self, x, mean)
      import MeanType
      import wp
      class(MeanType), intent(in) :: self
      real(kind=wp), dimension(:,:), intent(in) :: x
      real(kind=wp), dimension(:), intent(out) :: mean
    end subroutine value_interface

    subroutine write_interface(self, unit)
      import MeanType
      class(MeanType), intent(in) :: self
      integer, intent(in) :: unit
    end subroutine write_interface
  end interface

  contains

  function nparams_interface(self) result(nparams)
    class(MeanType), intent(in) :: self
    integer :: nparams

    nparams = 0
  end function nparams_interface

  function get_params_interface(self) result(params)
    class(MeanType), intent(in) :: self
    real(kind=wp), dimension(:), allocatable :: params

    allocate(params(0))
  end function get_params_interface

  subroutine set_params_interface(self, params)
    class(MeanType), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: params
  end subroutine set_params_interface
end module mean_type