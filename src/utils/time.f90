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