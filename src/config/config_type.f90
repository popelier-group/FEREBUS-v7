module config_type_module
  use writable_module, only: Writable
  implicit none
  private
  public :: ConfigType, WritableConfigType

  type, abstract :: ConfigType
  contains
    private
    procedure(info_interface), public, pass(self), deferred :: info  
  end type ConfigType

  type, abstract, extends(Writable) :: WritableConfigType
  contains
    private
    procedure(writable_info_interface), public, pass(self), deferred :: info  
  end type WritableConfigType

  abstract interface
    subroutine info_interface(self, unit)
      import ConfigType
      implicit none
      class(ConfigType), intent(in) :: self
      integer, intent(in) :: unit
    end subroutine info_interface

    subroutine writable_info_interface(self, unit)
      import WritableConfigType
      implicit none
      class(WritableConfigType), intent(in) :: self
      integer, intent(in) :: unit
    end subroutine writable_info_interface
  end interface

end module config_type_module