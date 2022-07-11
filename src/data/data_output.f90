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
