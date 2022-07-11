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
  