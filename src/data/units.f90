module units_module
  use utils_str, only: to_lower
  implicit none
  private

  type :: UnitType
    character(len=:), allocatable :: name
  contains
    procedure, public, pass(self) :: init
    procedure, public, pass(self) :: unknown_input
    procedure, public, pass(self) :: unknown_output
    ! procedure, public, pass(self) :: unknown => unknown_input, unkown_output
  end type UnitType

  public UnitType

  character(len=4), parameter :: DEFAULT_DISTANCE_UNIT = "bohr"
  character(len=7), parameter :: DEFAULT_ANGLE_UNIT = "radians"

  character(len=2), parameter :: DEFAULT_IQA_UNIT = "Ha"
  character(len=1), parameter :: DEFAULT_Q00_UNIT = "e"

  contains

  subroutine init(self, unit)
    class(UnitType), intent(inout) :: self
    character(len=*), intent(in) :: unit

    allocate(character(len=len(trim(unit))) :: self%name)
    self%name = trim(unit)
  end subroutine

  subroutine unknown_input(self, i)
    class(UnitType), intent(inout) :: self
    integer :: i

    if (modulo(i, 3) .eq. 0 .or. (i .gt. 3 .and. modulo(i+1, 3) .eq. 0)) then
      call self%init(DEFAULT_ANGLE_UNIT)
    else
      call self%init(DEFAULT_DISTANCE_UNIT)
    end if
  end subroutine unknown_input

  subroutine unknown_output(self, label)
    class(UnitType), intent(inout) :: self
    character(len=*), intent(in) :: label

    if (trim(to_lower(label)) .eq. "iqa") then
      call self%init(DEFAULT_IQA_UNIT)
    else if (trim(to_lower(label)) .eq. "q00") then
      call self%init(DEFAULT_Q00_UNIT)
    else
      call self%init("unknown")
    end if
  end subroutine unknown_output
end module units_module