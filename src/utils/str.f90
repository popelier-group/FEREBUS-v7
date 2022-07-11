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

module utils_str
    implicit none
    private

    public :: to_lower, to_upper, starts_with, is_feature_header, strip, replace, sanitise, contains_substr
    public :: is_space, is_digit, is_alpha, is_alphanum, is_num, concat, fmt_str

    contains

    function fmt_str(n) result(fmt)
      integer :: n
      character(len=:), allocatable :: fmt

      write(fmt,'(a,i0,a)') "(a, 1x, (",n,"(g0,1x)))"
    end function fmt_str

    function to_lower(str_in) result(str_out)
      character(len=*), intent(in) :: str_in
      character(len=len(str_in))   :: str_out
      integer                      :: i, j

      do i = 1, len(str_in)
        j = iachar(str_in(i:i))
        if (j .ge. iachar("A") .and. j .le. iachar("Z")) then
          str_out(i:i) = achar(iachar(str_in(i:i)) + 32)
        else
          str_out(i:i) = str_in(i:i)
        end if
      end do
    end function to_lower

    function to_upper(str_in) result(str_out)
      character(len=*), intent(in) :: str_in
      character(len=len(str_in))   :: str_out
      integer                      :: i, j

      do i = 1, len(str_in)
        j = iachar(str_in(i:i))
        if (j .ge. iachar("a") .and. j .le. iachar("z")) then
          str_out(i:i) = achar(iachar(str_in(i:i)) - 32)
        else
          str_out(i:i) = str_in(i:i)
        end if
      end do
    end function to_upper

    function replace(str_in, curr_char, repl_char) result(str_out)
      character(len=*), intent(in) :: str_in
      character(len=1), intent(in) :: curr_char
      character(len=1), intent(in) :: repl_char
      character(len=len(str_in))   :: str_out
      integer                      :: i

      do i = 1, len(str_in)
        if (str_in(i:i) .eq. curr_char) then
          str_out(i:i) = repl_char
        else
          str_out(i:i) = str_in(i:i)
        end if
      end do
    end function replace

    function sanitise(str_in, str_replace) result(str_out)
      character(len=*), intent(in) :: str_in
      character(len=*), optional, intent(in) :: str_replace
      character(len=len(str_in))   :: str_out
      character(len=:), allocatable :: str_replace_

      if (present(str_replace)) then
        str_replace_ = str_replace
      else
        str_replace_ = "_"
      end if

      str_out = to_lower(replace(str_in, " ", str_replace_))
    end function sanitise

    function is_numeric(string)
      character(len=*), intent(in) :: string
      logical :: is_numeric
      real :: x
      integer :: e

      read(string, *, iostat=e) x
      is_numeric = e == 0
    end function is_numeric

    function starts_with(string, substring)
      character(len=*), intent(in) :: string, substring
      logical :: starts_with
      starts_with = index(string, substring) == 1
    end function starts_with

    elemental subroutine strip(string,set)
      character(len=*), intent(inout) :: string
      character(len=*), intent(in)    :: set
      integer                         :: old, new, stride
      old = 1; new = 1
      do
        stride = scan( string( old : ), set )
        if ( stride > 0 ) then
          string( new : new+stride-2 ) = string( old : old+stride-2 )
          old = old+stride
          new = new+stride-1
        else
          string( new : ) = string( old : )
          return
        end if
      end do
    end subroutine strip

    function is_feature_header(header)
      character(len=*), intent(in) :: header
      character(len=:), allocatable :: header_f
      logical :: is_feature_header

      is_feature_header = .false.

      if (starts_with(header, "f")) then
        header_f = header
        call strip(header_f, "f")
        if (is_numeric(header_f)) then
          is_feature_header = .true.
        end if
      end if
    end function is_feature_header

    function contains_substr(str, substr)
      character(len=*), intent(in) :: str
      character(len=*), intent(in) :: substr
      logical :: contains_substr
      integer :: result

      result = index(str, substr)
      contains_substr = (result .gt. 0)
    end function contains_substr

    function is_space(char)
      character, intent(in) :: char
      logical :: is_space

      is_space = trim(char) .eq. ''
    end function is_space
    
    function is_digit(char)
      character, intent(in) :: char
      logical :: is_digit

      is_digit = (iachar(char) .ge. iachar("0") .and. iachar(char) .le. iachar("9"))
    end function is_digit
    
    function is_alpha(char)
      character, intent(in) :: char
      logical :: is_alpha

      is_alpha = ((iachar(char) .ge. iachar("A") .and. iachar(char) .le. iachar("Z")) &
             .or. (iachar(char) .ge. iachar("a") .and. iachar(char) .le. iachar("z")))
    end function is_alpha
    
    function is_alphanum(char)
      character, intent(in) :: char
      logical :: is_alphanum

      is_alphanum = is_alpha(char) .or. is_digit(char)
    end function is_alphanum

    function is_real(char)
      character, intent(in) :: char
      logical :: is_real

      is_real = any(char .eq. (/'.', 'e', '-', '+'/))
    end function is_real

    function is_num(char)
      character, intent(in) :: char
      logical :: is_num

      is_num = is_digit(char) .or. is_real(char)
    end function is_num

    function concat(strin, cin) result(strout)
      character(len=*), intent(in) :: strin
      character, intent(in) :: cin
      character(len=len(strin)+1) :: strout

      strout = strin // cin
    end function

end module utils_str