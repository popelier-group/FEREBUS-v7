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

module learning_automa_module
  use kinds, only: wp
  use learning_automa_value_module, only: AutomataValue
  use utils, only: random
  implicit none
  private
  public :: LearningAutomata

  type LearningAutomata
    type(AutomataValue), dimension(:), allocatable :: automata
    integer :: selected_action
    real(kind=wp) :: reward_step_length
    real(kind=wp) :: penalty_step_length
  contains
    private
    procedure, public, pass(self) :: init => init_learning_automata
    procedure, public, pass(self) :: choose
    procedure, public, pass(self) :: reward
    procedure, public, pass(self) :: penalise
    procedure, public, pass(self) :: sum_of_probabilities
    procedure, public, pass(self) :: n_actions
  end type LearningAutomata

contains

  subroutine init_learning_automata(self, min_value, max_value, n_values, reward_step_length, penalty_step_length)
    class(LearningAutomata), intent(inout) :: self
    real(kind=wp), intent(in) :: min_value
    real(kind=wp), intent(in) :: max_value
    integer, intent(in) :: n_values
    real(kind=wp), intent(in) :: reward_step_length
    real(kind=wp), intent(in) :: penalty_step_length

    integer :: i

    allocate(self%automata(n_values))
    do i = 1, n_values
      call self%automata(i)%init( &
        min_value + real(i-1, kind=wp)*(max_value-min_value)/real(n_values-1, kind=wp), &
        1.0_wp/real(n_values, kind=wp) &
      )
    end do

    self%reward_step_length = reward_step_length
    self%penalty_step_length = penalty_step_length
  end subroutine init_learning_automata

  function choose(self) result(choice)
    class(LearningAutomata), intent(inout) :: self
    real(kind=wp) :: choice

    integer :: i
    real(kind=wp) :: rand, previous_probablity, current_probablity

    call random(rand, self%sum_of_probabilities())
    previous_probablity = 0.0_wp

    do i = 1, self%n_actions()
      current_probablity = previous_probablity + self%automata(i)%probablity
      if ((rand.ge.previous_probablity).and.(rand.le.current_probablity)) then
        self%selected_action = i
        choice = self%automata(i)%value
        continue
      end if
      previous_probablity = previous_probablity + self%automata(i)%probablity
    end do
  end function choose

  subroutine reward(self)
    class(LearningAutomata), intent(inout) :: self
    integer :: i

    do i = 1, self%n_actions()
      if (i .eq. self%selected_action) then
        self%automata(i)%probablity = self%automata(i)%probablity + self%reward_step_length*(1.0_wp-self%automata(i)%probablity)
      else
        self%automata(i)%probablity = self%automata(i)%probablity*(1.0_wp-self%reward_step_length)
      end if
    end do
  end subroutine reward

  subroutine penalise(self)
    class(LearningAutomata), intent(inout) :: self
    integer :: i

    do i = 1, self%n_actions()
      if (i .eq. self%selected_action) then
        self%automata(i)%probablity = self%automata(i)%probablity*(1.0_wp-self%penalty_step_length)
      else
        self%automata(i)%probablity = (self%penalty_step_length/real(self%n_actions()-1, kind=wp))+&
	  (1.0_wp-self%penalty_step_length)*self%automata(i)%probablity
      end if
    end do
  end subroutine penalise

  function sum_of_probabilities(self) result(n)
    class(LearningAutomata), intent(in) :: self
    real(kind=wp) :: n
    integer :: i
    n = 0.0_wp
    do i = 1, self%n_actions()
      n = n + self%automata(i)%probablity
    end do
  end function sum_of_probabilities

  function n_actions(self) result(n)
    class(LearningAutomata), intent(in) :: self
    integer :: n
    n = size(self%automata)
  end function n_actions
end module learning_automa_module
