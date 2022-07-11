module linear_mean_config_module
    use mean_config_module, only: MeanConfig
    use config_error, only: error_data
    use tomlf, only: toml_table
    implicit none
    private
    public :: LinearMeanConfig
  
    type, extends(MeanConfig) :: LinearMeanConfig
    contains
      private
      procedure, public, pass(self) :: init => init_linear_mean
      procedure, public, pass(self) :: info
    end type LinearMeanConfig
  
  contains
    
  subroutine init_linear_mean(self, table, error)
    class(LinearMeanConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_data), allocatable, intent(out) :: error
  end subroutine init_linear_mean
  
    subroutine info(self, unit)
      class(LinearMeanConfig), intent(in) :: self
      integer, intent(in) :: unit
  
      write(unit, '(A)') "Mean Config"
      write(unit, '(" - ", A, T30, A)') "Type", "Linear"
    end subroutine info
  end module linear_mean_config_module