module error_module
  implicit none
  private

  type error
    character(len=:), allocatable :: message
    logical :: is_fatal
  contains
    procedure :: dislpay => display_error
  end type error

  public :: error, fatal_error, display_error

contains

  subroutine display_error(self, unit)
    class(error), intent(in) :: self
    integer, intent(in) :: unit
    write(unit, *) "Error: " // self%message
    if (self%is_fatal) then
      stop
    end if
  end subroutine display_error

  subroutine fatal_error(error_, message)
    type(error), allocatable, intent(out) :: error_
    character(len=*), intent(in) :: message

    allocate(error_)
    error_%message = message
    error_%is_fatal = .true.
  end subroutine

end module error_module
