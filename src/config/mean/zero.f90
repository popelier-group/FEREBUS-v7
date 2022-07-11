module zero_mean_config_module
  use mean_config_module, only: MeanConfig
  use config_error, only: error_data
  use tomlf, only: toml_table
  implicit none
  private
  public :: ZeroMeanConfig

  type, extends(MeanConfig) :: ZeroMeanConfig
  contains
    private
    procedure, public, pass(self) :: init => init_zero_mean
    procedure, public, pass(self) :: info
  end type ZeroMeanConfig

contains
  
subroutine init_zero_mean(self, table, error)
  class(ZeroMeanConfig), intent(inout) :: self
  type(toml_table), intent(inout) :: table
  type(error_data), allocatable, intent(out) :: error
end subroutine init_zero_mean

  subroutine info(self, unit)
    class(ZeroMeanConfig), intent(in) :: self
    integer, intent(in) :: unit

    write(unit, '(A)') "Mean Config"
    write(unit, '(" - ", A, T30, A)') "Type", "Zero"
  end subroutine info
end module zero_mean_config_module