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

module data_input
  use kinds, only: wp
  use writable_module, only: Writable
  use units_module, only: UnitType
  use utils, only: fmt_str
  implicit none
  private
  public :: Input

  type, extends(Writable) :: Input
    integer :: ntrain, nfeats
    real(kind=wp), dimension(:, :), allocatable :: data
    type(UnitType), dimension(:), allocatable :: units
  contains
    private
    procedure, public, pass(self) :: init => init_input
    procedure, public, pass(self) :: write => write_input
  end type Input
  
contains
  subroutine init_input(self, data, units)
    class(Input), intent(inout) :: self
    real(kind=wp), dimension(:, :), intent(in) :: data
    type(UnitType), dimension(:), intent(in) :: units

    self%ntrain = size(data, 1)
    self%nfeats = size(data, 2)
    allocate(self%data, source=data)
    allocate(self%units, source=units)
  end subroutine init_input

  subroutine write_input(self, unit)
    class(Input), intent(in) :: self
    integer, intent(in) :: unit
    integer :: i, j
    character(len=50) :: fmt

    write(fmt,'(a,i0,a)') "(",self%nfeats,"(g0,1x))"

    write(unit, "(A)") "[training_data.x]"
    do i=1, self%ntrain
      write(unit, trim(fmt)) (self%data(i, j), j=1,self%nfeats)
    end do
  end subroutine

  subroutine destructor(self)
    type(Input), intent(inout) :: self

    if (allocated(self%data)) deallocate(self%data)
  end subroutine destructor
end module data_input
