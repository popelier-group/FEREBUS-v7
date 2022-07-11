module system_config_module
  use config_error, only : error_data, fatal_error
  use atom_config_module, only: AtomConfig, SlicedAtomConfig
  use property_config_module, only: PropertyConfig
  use tomlf
  implicit none
  private
  public :: SystemConfig, SlicedSystemConfig
  
  type :: SystemConfig
    character(len=:), allocatable :: name
    integer :: natoms
    integer :: nfeats
    type(AtomConfig), allocatable, dimension(:) :: atoms
  contains
    private
    procedure, public, pass(self) :: init => init_system_config
    procedure, public, pass(self) :: info
  end type SystemConfig

  type, extends(SystemConfig) :: SlicedSystemConfig
    type(SlicedAtomConfig), allocatable :: atom
  contains
    private
    procedure, public, pass(self) :: slice => init_sliced_system_config
  end type SlicedSystemConfig
  
contains
  
  subroutine init_system_config(self, table, error)
    class(SystemConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_data), allocatable, intent(out) :: error

    type(toml_array), pointer :: children
    type(toml_table), pointer :: child
    type(PropertyConfig), allocatable, dimension(:) :: properties
    character(len=:), allocatable :: property
    character(len=:), allocatable :: atom
    integer :: iatm, natm, iprop, nprop
    
    call get_value(table, "name", self%name)
    if (.not.allocated(self%name)) then
      call table%get_key(self%name)
    end if
    if (.not.allocated(self%name)) then
      call fatal_error(error, "System name is not provided")
      stop
    end if

    call get_value(table, "natoms", self%natoms, -1)
    call get_value(table, "nfeats", self%nfeats, -1)

    call get_value(table, "properties", children, requested=.false.)
    if (associated(children)) then
      nprop = len(children)
      allocate(properties(nprop))
      do iprop = 1, nprop
        call get_value(children, iprop, property)
        if (.not.allocated(property)) then
          call fatal_error(error, "Element of property array is not a string value")
          exit
        end if
        call properties(iprop)%init(property)
      end do
    end if

    call get_value(table, "atoms", children, requested=.false.)
    if (associated(children)) then
      natm = len(children)
      if (natm < 1) then
        call fatal_error(error, "Atoms field requires at least one entry or must be omitted")
        return
      end if
      allocate(self%atoms(natm))
      do iatm = 1, natm
        call get_value(children, iatm, child)
        call self%atoms(iatm)%init(child)
      end do
    end if

    if (allocated(properties)) then
      do iatm = 1, size(self%atoms)
        call self%atoms(iatm)%set_properties(properties)
      end do
    end if

    if (self%natoms .eq. -1) then
      call fatal_error(error, "'natoms' must be defined")
      ! exit
    end if
  
    if (self%nfeats .eq. -1) then
      self%nfeats = 3*self%natoms - 6
    end if
  end subroutine init_system_config
  
  subroutine info(self, unit)
    class(SystemConfig), intent(in) :: self
    integer, intent(in) :: unit
    integer :: iatm

    write(unit, '(a, t30, a)') "System name", self%name
    write(unit, '(a, t30, i5)') "nAtoms", self%natoms
    write(unit, '(a, t30, i5)') "nFeats", self%nfeats

    write(unit, '(a, t30)') "Atoms"
    do iatm = 1, size(self%atoms)
      call self%atoms(iatm)%info(unit)
    end do
  end subroutine info
    
  subroutine init_sliced_system_config(self, system, iatom, iprop)
    class(SlicedSystemConfig), intent(inout) :: self
    type(SystemConfig), intent(in) :: system
    integer, intent(in) :: iatom
    integer, intent(in) :: iprop

    allocate(self%name, source=system%name)
    self%name = system%name

    self%natoms = system%natoms
    self%nfeats = system%nfeats

    allocate(self%atoms, source=system%atoms)
    self%atoms = system%atoms

    allocate(self%atom)
    call self%atom%slice(self%atoms(iatom), iprop)
  end subroutine init_sliced_system_config
end module system_config_module
