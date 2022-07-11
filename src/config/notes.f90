module notes_module
    use, intrinsic :: iso_fortran_env, only : error_unit, output_unit
    use tomlf, only: get_value, toml_table, toml_key, new_table
    use config_error, only : error_data
    use utils, only: to_lower
    use config_type_module, only: ConfigType
  
    implicit none
    private
    public :: Note
  
    type, extends(ConfigType) :: Note
      character(len=:), allocatable :: key
      character(len=:), allocatable :: value
    contains
      private
      procedure, public, pass(self) :: init => init_note
      procedure, public, pass(self) :: info => note_info
    end type Note
  
  contains

    subroutine init_note(self, key, value)
      class(Note), intent(inout) :: self
      character(len=*), intent(in) :: key
      character(len=*), intent(in) :: value

      allocate(self%key, source=key)
      allocate(self%value, source=value)
    end subroutine init_note

    subroutine note_info(self, unit)
      class(Note), intent(in) :: self
      integer, intent(in) :: unit
  
      write(unit, '(a, a, t30, a)') ' - ', self%key, self%value
    end subroutine note_info
end module notes_module
  