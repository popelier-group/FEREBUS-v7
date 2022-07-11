module mpi_utils
  ! use blacs, only: blacs_pinfo
  implicit none
  private
  public :: setup_blacs
contains
  subroutine setup_blacs()
    integer :: iproc, nprocs
    ! Determine current processor and number of processors on the machine
    call blacs_pinfo(iproc, nprocs)
    print*, iproc, nprocs
  end subroutine setup_blacs
end module mpi_utils
