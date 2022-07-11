module property_config_module
  use config_type_module, only: ConfigType
  implicit none
  private
  public :: PropertyConfig

  type, extends(ConfigType) :: PropertyConfig
    character(len=:), allocatable :: name
  contains
    private
    procedure, public, pass(self) :: init => init_property_config
    procedure, public, pass(self) :: info
  end type PropertyConfig
  
  contains
  
  subroutine init_property_config(self, property_name)
    class(PropertyConfig), intent(inout) :: self
    character(len=*), intent(in) :: property_name

    self%name = property_name
  end subroutine init_property_config
    
  subroutine info(self, unit)
    class(PropertyConfig), intent(in) :: self
    integer, intent(in) :: unit

    if (allocated(self%name)) then
      write(unit, '(" - ", a, t30, a)') "Name", self%name
    end if
  end subroutine info
end module property_config_module