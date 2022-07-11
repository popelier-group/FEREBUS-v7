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

module atom_config_module
  use tomlf, only: toml_table, get_value, toml_array
  use config_type_module, only: ConfigType
  use property_config_module, only: PropertyConfig
  use alf_config_module, only: AtomicLocalFrameConfig
  implicit none
  private
  public :: AtomConfig, SlicedAtomConfig
  
  type, extends(ConfigType) :: AtomConfig
    character(len=:), allocatable :: name
    type(AtomicLocalFrameConfig), allocatable :: alf
    type(PropertyConfig), allocatable, dimension(:) :: properties
  contains
    private
    procedure, public, pass(self) :: init => init_atom_config
    procedure, public, pass(self) :: set_properties
    procedure, public, pass(self) :: info

  end type AtomConfig

  type, extends(AtomConfig) :: SlicedAtomConfig
    type(PropertyConfig), allocatable :: property
  contains
    private
    procedure, public, pass(self) :: slice => init_sliced_atom_config
  end type SlicedAtomConfig
  
contains
  
  subroutine init_atom_config(self, table)
    class(AtomConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
  
    type(toml_array), pointer :: alf_array
    integer, dimension(3) :: alf
    integer :: i

    call get_value(table, "name", self%name)
    call get_value(table, "alf", alf_array)
    if (associated(alf_array)) then
      do i = 1, 3
        call get_value(alf_array, i, alf(i))
      end do
    end if
    
    allocate(self%alf)
    call self%alf%init(alf)
  end subroutine init_atom_config
  
  subroutine set_properties(self, properties)
    class(AtomConfig), intent(inout) :: self
    type(PropertyConfig), dimension(:), intent(in) :: properties
  
    allocate(self%properties, source=properties)
    self%properties = properties
  end subroutine set_properties
  
  subroutine info(self, unit)
    class(AtomConfig), intent(in) :: self
    integer, intent(in) :: unit

    if (allocated(self%name)) then
      write(unit, '(" - ", A, T30, A)') "Name", self%name
      if (allocated(self%alf)) then
        call self%alf%info(unit)
      end if
    end if
  end subroutine info

  subroutine init_sliced_atom_config(self, atom, iprop)
    class(SlicedAtomConfig), intent(inout) :: self
    type(AtomConfig), intent(in) :: atom
    integer, intent(in) :: iprop

    if(.not.allocated(self%name)) allocate(self%name, source=atom%name)
    self%name = atom%name
    allocate(self%alf, source=atom%alf)
    self%alf = atom%alf
    if (allocated(atom%properties)) then
      allocate(self%properties, source=atom%properties)
      self%properties = atom%properties
      allocate(self%property, source=self%properties(iprop))
      self%property = self%properties(iprop)
    end if
  end subroutine init_sliced_atom_config
end module atom_config_module