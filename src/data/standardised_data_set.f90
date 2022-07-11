module standardised_data_set_module
  use kinds, only: wp
  use utils, only: mean, std, standardise
  use data_output, only: Output
  use data_input, only: Input
  use data_set_type_module, only: DataSet
  use units_module, only: UnitType
  implicit none
  private
  public :: StandardisedDataSet

  type, extends(DataSet) :: StandardisedDataSet
    real(kind=wp), allocatable, dimension(:) :: x_mu
    real(kind=wp), allocatable, dimension(:) :: x_std
    real(kind=wp) :: y_mu
    real(kind=wp) :: y_std
    type(Input), allocatable :: original_x
    type(Output), allocatable :: original_y
  contains
    private
    procedure, public, pass(self) :: init => init_standardised_data_set
    procedure, public, pass(self) :: write => write_dataset
  end type StandardisedDataSet

contains

  subroutine init_standardised_data_set(self, x, y, y_label, xunits, yunit)
    class(StandardisedDataSet), intent(inout) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
    real(kind=wp), dimension(:), intent(in) :: y
    character(len=*), intent(in) :: y_label
    type(UnitType), dimension(:), intent(in) :: xunits
    type(UnitType), intent(in) :: yunit

    real(kind=wp), dimension(size(x,1), size(x,2)) :: std_x
    real(kind=wp), dimension(size(y)) :: std_y
    integer :: i

    allocate(self%original_x)
    call self%original_x%init(x, xunits)
    allocate(self%original_y)
    call self%original_y%init(y_label, y, yunit)

    allocate(self%x_mu(self%original_x%nfeats))
    allocate(self%x_std(self%original_x%nfeats))

    do i = 1, self%original_x%nfeats
      self%x_mu(i) = mean(self%original_x%data(:, i))
      self%x_std(i) = std(self%original_x%data(:, i), self%x_mu(i))

      std_x(:, i) = standardise(self%original_x%data(:, i), self%x_mu(i), self%x_std(i))
    end do

    self%y_mu = mean(self%original_y%data)
    self%y_std = std(self%original_y%data, self%y_mu)

    std_y = standardise(self%original_y%data, self%y_mu, self%y_std)

    allocate(self%x)
    call self%x%init(std_x, xunits)
    allocate(self%y)
    call self%y%init(y_label, std_y, yunit)
  end subroutine init_standardised_data_set

  subroutine write_dataset(self, unit)
    class(StandardisedDataSet), intent(in) :: self
    integer, intent(in) :: unit
    character(len=50) :: fmt
    integer :: i, nx_units

    nx_units = size(self%x%units)

    write(fmt,'(a,i0,a)') "(a,1x,(", nx_units, "(a,1x)))"

    write(unit, "(A)") "[training_data]"
    write(unit, trim(fmt)) "units.x", (self%x%units(i)%name, i=1,nx_units)
    write(unit, "(a,1x,a)") "units.y", self%y%unit%name
    write(unit, "(A)") "scaling.x standardise"
    write(unit, "(A)") "scaling.y standardise"
    write(unit, "(A)")
    call self%original_x%write(unit)
    write(unit, "(A)")
    call self%original_y%write(unit)
  end subroutine write_dataset
end module standardised_data_set_module
