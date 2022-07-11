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

module config_toml
  use config_error, only : error_data, fatal_error, file_not_found_error
  use tomlf, only : toml_table, toml_error, toml_parse, toml_stat, &
    & set_value, get_value
  implicit none
  private

  public :: read_config_file
  public :: toml_table, toml_stat, set_value, get_value

contains

  subroutine read_config_file(table, config, error)
    type(toml_table), allocatable, intent(out) :: table
    character(len=*), intent(in) :: config
    type(error_data), allocatable, intent(out) :: error

    type(toml_error), allocatable :: parse_error
    integer :: unit
    logical :: exist

    inquire(file=config, exist=exist)

    if (.not.exist) then
        call file_not_found_error(error, config)
        return
    end if

    open(file=config, newunit=unit, status='old')
    call toml_parse(table, unit, parse_error)
    close(unit)

    if (allocated(parse_error)) then
        allocate(error)
        call move_alloc(parse_error%message, error%message)
        return
    end if
  end subroutine read_config_file
end module config_toml
