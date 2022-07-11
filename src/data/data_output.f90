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

module data_output
  use kinds, only: wp
  use writable_module, only: Writable
  use units_module, only: UnitType
  implicit none
  private
  public :: Output

  type, extends(Writable) :: Output
    character(len=:), allocatable :: label
    real(kind=wp), dimension(:), allocatable :: data
    type(UnitType), allocatable :: unit
  contains
    private
    procedure, public, pass(self) :: init => init_output
    procedure, public, pass(self) :: write => write_output
  end type Output

contains
  subroutine init_output(self, label, data, unit)
    class(Output), intent(inout) :: self
    character(len=*), intent(in) :: label
    real(kind=wp), dimension(:), intent(in) :: data
    type(UnitType), intent(in) :: unit

    allocate(character(len=len(trim(label))) :: self%label)
    self%label = trim(label)
    allocate(self%data, source=data)
    allocate(self%unit, source=unit)
  end subroutine init_output

  subroutine write_output(self, unit)
    class(Output), intent(in) :: self
    integer, intent(in) :: unit
    integer :: i

    write(unit, "(A)") "[training_data.y]"
    do i = 1, size(self%data)
      write(unit, "(g0)") self%data(i)
    end do
  end subroutine write_output
end module
