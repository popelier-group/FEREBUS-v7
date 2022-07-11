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