module quadratic_mean_config_module
    use mean_config_module, only: MeanConfig
    use config_error, only: error_data
    use tomlf, only: toml_table
    implicit none
    private
    public :: QuadraticMeanConfig
  
    type, extends(MeanConfig) :: QuadraticMeanConfig
    contains
      private
      procedure, public, pass(self) :: init => init_quadratic_mean
      procedure, public, pass(self) :: info
    end type QuadraticMeanConfig
  
  contains
    
  subroutine init_quadratic_mean(self, table, error)
    class(QuadraticMeanConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_data), allocatable, intent(out) :: error
  end subroutine init_quadratic_mean
  
    subroutine info(self, unit)
      class(QuadraticMeanConfig), intent(in) :: self
      integer, intent(in) :: unit
  
      write(unit, '(A)') "Mean Config"
      write(unit, '(" - ", A, T30, A)') "Type", "Linear"
    end subroutine info
  end module quadratic_mean_config_module