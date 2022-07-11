module writable_module
  implicit none
  private
  public :: Writable

  type, abstract :: Writable
  contains
    private
    procedure(write_interface), public, pass(self), deferred :: write
  end type Writable
  
  abstract interface
    subroutine write_interface(self, unit)
      import Writable
      implicit none
      class(Writable), intent(in) :: self
      integer, intent(in) :: unit
    end subroutine write_interface
  end interface
end module writable_module