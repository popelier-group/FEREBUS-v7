module constant_mean_config_module
  use mean_config_module, only: MeanConfig
  use config_error, only: error_data
  use tomlf, only: toml_table
  implicit none
  private
  public :: ConstantMeanConfig

  type, extends(MeanConfig) :: ConstantMeanConfig
  contains
    private
    procedure, public, pass(self) :: init => init_constant_mean
    procedure, public, pass(self) :: info
  end type ConstantMeanConfig

contains
  
  subroutine init_constant_mean(self, table, error)
    class(ConstantMeanConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_data), allocatable, intent(out) :: error
  end subroutine init_constant_mean

  subroutine info(self, unit)
    class(ConstantMeanConfig), intent(in) :: self
    integer, intent(in) :: unit

    write(unit, '(A)') "Mean Config"
    write(unit, '(" - ", A, T30, A)') "Type", "Constant"
  end subroutine info
end module constant_mean_config_module