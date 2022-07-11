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

module learning_automa_value_module
  use kinds, only: wp
  implicit none
  private
  public :: AutomataValue

  type AutomataValue
    real(kind=wp) :: value
    real(kind=wp) :: probablity
  contains
    procedure, public, pass(self) :: init => init_automata_value
  end type AutomataValue

contains
  
  subroutine init_automata_value(self, value, probablity)
    class(AutomataValue), intent(inout) :: self
    real(kind=wp), intent(in) :: value
    real(kind=wp), intent(in) :: probablity

    self%value = value
    self%probablity = probablity
  end subroutine init_automata_value
end module learning_automa_value_module