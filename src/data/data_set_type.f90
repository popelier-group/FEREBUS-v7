module data_set_type_module
  use kinds, only: wp
  use constants, only: MAX_OUTPUT_CHARACTER_LENGTH
  use writable_module, only: Writable
  use data_output, only: Output
  use data_input, only: Input
  use units_module, only: UnitType
  implicit none
  private
  public :: DataSet

  type, extends(Writable) :: DataSet
    type(Input), allocatable :: x
    type(Output), allocatable :: y
  contains
    private
    procedure, public, pass(self) :: init => init_dataset
    procedure, pass(self) :: copy_dataset
    procedure, public, pass(self) :: write => write_dataset
  end type DataSet
contains

  subroutine init_dataset(self, x, y, y_label, xunits, yunit)
    class(DataSet), intent(inout) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
    real(kind=wp), dimension(:), intent(in) :: y
    character(len=*), intent(in) :: y_label
    type(UnitType), dimension(:), intent(in) :: xunits
    type(UnitType), intent(in) :: yunit
    integer :: ntrain, nfeats

    allocate(self%x)
    call self%x%init(x, xunits)
    allocate(self%y)
    call self%y%init(y_label, y, yunit)
  end subroutine init_dataset

  subroutine write_dataset(self, unit)
    class(DataSet), intent(in) :: self
    integer, intent(in) :: unit
    character(len=50) :: fmt
    integer :: i, nx_units

    nx_units = size(self%x%units)

    write(fmt,'(a,i0,a)') "(a,1x,(", nx_units, "(a,1x)))"

    write(unit, "(a)") "[training_data]"
    write(unit, trim(fmt)) "units.x", (self%x%units(i)%name, i=1,nx_units)
    write(unit, "(a,1x,a)") "units.y", self%y%unit%name
    write(unit, "(a)") "scaling.x none"
    write(unit, "(a)") "scaling.y none"
    write(unit, "(a)")
    call self%x%write(unit)
    write(unit, "(a)")
    call self%y%write(unit)
  end subroutine write_dataset

  subroutine copy_dataset(self, from)
    class(DataSet), intent(inout) :: self
    type(DataSet), intent(in) :: from

    allocate(self%x, source=from%x)
    self%x = from%x

    allocate(self%y, source=from%y)
    self%y = from%y
  end subroutine copy_dataset
end module data_set_type_module
