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

module error_module
  implicit none
  private

  type error
    character(len=:), allocatable :: message
    logical :: is_fatal
  contains
    procedure :: dislpay => display_error
  end type error

  public :: error, fatal_error, display_error

contains

  subroutine display_error(self, unit)
    class(error), intent(in) :: self
    integer, intent(in) :: unit
    write(unit, *) "Error: " // self%message
    if (self%is_fatal) then
      stop
    end if
  end subroutine display_error

  subroutine fatal_error(error_, message)
    type(error), allocatable, intent(out) :: error_
    character(len=*), intent(in) :: message

    allocate(error_)
    error_%message = message
    error_%is_fatal = .true.
  end subroutine

end module error_module
