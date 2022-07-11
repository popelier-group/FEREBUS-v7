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

module utils_time
  use kinds, only: wp
  implicit none
  private
  public wtime

contains

  function wtime() result(wall_time)
    !*****************************************************************************80
    !  wtime returns a reading of the wall clock time.
    !
    !  Discussion:
    !    To get the elapsed wall clock time, call WTIME before and after a given
    !    operation, and subtract the first reading from the second.
    !
    !    This function is meant to suggest the similar routines:
    !      "omp_get_wtime ( )" in OpenMP,
    !      "MPI_Wtime ( )" in MPI,
    !      and "tic" and "toc" in MATLAB.
    !  Licensing:
    !    This code is distributed under the GNU LGPL license. 
    !  Modified:
    !    25 February 2021
    !  Author:
    !    John Burkardt
    !
    !  Parameters:
    !    Output, real(kind=wp) wall_time, the wall clock reading, in seconds.
    implicit none
  
    integer :: clock_max
    integer :: clock_rate
    integer :: clock_reading
    real(kind=wp) :: wall_time
  
    call system_clock(clock_reading, clock_rate, clock_max)
  
    wall_time = real(clock_reading, kind=wp) / real(clock_rate, kind=wp)
  end function wtime
end module utils_time