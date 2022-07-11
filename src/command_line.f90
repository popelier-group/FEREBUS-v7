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

module command_line
  implicit none
  private

  type command_line_arguments
    character(len=:), allocatable :: config_file
    logical :: read_config_only = .false.
  contains
    procedure, pass(self) :: default
  end type command_line_arguments

  public :: command_line_arguments, parse_command_line_arguments

contains

  subroutine parse_command_line_arguments(cmd_args)
    type(command_line_arguments), intent(out) :: cmd_args

    character(len=*), parameter :: version = '8.0'
    character(len=:), allocatable :: argument
    integer :: iarg, length
    logical :: ignore = .false.

    do iarg = 1, command_argument_count()
      if (allocated(argument)) deallocate(argument)
      call get_command_argument(iarg, length=length)
      allocate(character(len=length) :: argument)

      if (length .gt. 0) then
        call get_command_argument(iarg, argument)
        select case (argument)
        case ('-v', '--version')
          print '(2a)', 'ferebus version ', version
          stop
        case ('-c', '--config')
          !> Get config file name
          call get_command_argument(iarg+1, length=length)
          allocate(character(len=length) :: cmd_args%config_file)
          call get_command_argument(iarg+1, cmd_args%config_file)
          ignore = .true.
        case ('-r', '--read')
          !> Read config file only
          cmd_args%read_config_only = .true.
        case ('-h', '--help')
          call print_help()
          stop
        case default
          if (ignore .eqv. .true.) then
            ignore = .false.
            continue
          else
            print '(a,a,/)', 'Unrecognized command-line option: ', argument
            call print_help()
            stop
          end if
        end select
      end if
      if (allocated(argument)) deallocate(argument)
    end do
    call cmd_args%default()
  end subroutine parse_command_line_arguments

  subroutine print_help()
    print '(a)', 'usage: ferebus [OPTIONS]'
    print '(a)', ''
    print '(a)', 'cmdline options:'
    print '(a)', ''
    print '(a)', '  -v, --version              print version information and exit'
    print '(a)', '  -h, --help                 print usage information and exit'
    print '(a)', '  -c, --config [config_file] set config file'
    print '(a)', '  -r, --read                 read input files only'
  end subroutine print_help

  subroutine default(self)
    class(command_line_arguments), intent(inout) :: self

    if (.not.allocated(self%config_file)) then
      allocate(character(len=12) :: self%config_file)
      self%config_file = "ferebus.toml"
    end if
  end subroutine default
end module command_line