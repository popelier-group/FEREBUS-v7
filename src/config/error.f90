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

module config_error
   implicit none
   private

   public :: error_data
   public :: fatal_error, file_not_found_error

   !> Data type defining an error
   type :: error_data
      !> Error message
      character(len=:), allocatable :: message
   end type error_data
contains
   !> Generic fatal runtime error
   subroutine fatal_error(error, message)
      !> Instance of the error data
      type(error_data), allocatable, intent(out) :: error
      !> Name of the missing file
      character(len=*), intent(in) :: message
      allocate(error)
      error%message = message
   end subroutine fatal_error

   !> Error created when a file is missing or not found
   subroutine file_not_found_error(error, file_name)
      !> Instance of the error data
      type(error_data), allocatable, intent(out) :: error

      !> Name of the missing file
      character(len=*), intent(in) :: file_name
      character(len=:), allocatable :: message

      allocate(character(len=47 + len(file_name)) :: message)
      message = "'"//file_name//"' could not be found, check if the file exists"

      call move_alloc(message, error%message)
   end subroutine file_not_found_error
end module config_error
