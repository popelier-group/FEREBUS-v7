module mean_config_module
  use config_type_module, only: ConfigType
  use config_error, only: error_data
  use tomlf, only: toml_table
  implicit none
  private
  public :: MeanConfig

  type, abstract, extends(ConfigType) :: MeanConfig
  contains
    procedure(init_interface), public, deferred, pass(self) :: init
  end type MeanConfig

  abstract interface
    subroutine init_interface(self, table, error)
      import MeanConfig
      import toml_table
      import error_data
      class(MeanConfig), intent(inout) :: self
      type(toml_table), intent(inout) :: table
      type(error_data), allocatable, intent(out) :: error
    end subroutine init_interface
  end interface
end module mean_config_module