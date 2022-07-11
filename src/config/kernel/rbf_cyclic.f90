module rbf_cyclic_config_module
    use stationary_kernel_config_module, only: StationaryKernelConfig
    use tomlf, only: toml_table
    use utils, only: get_value_converted
    use config_error, only: error_data

    implicit none
    private
    public :: RBFCyclicConfig

    type, extends(StationaryKernelConfig) :: RBFCyclicConfig
    contains
      private
      procedure, public, pass(self) :: init => init_rbf_config
      procedure, public, pass(self) :: info
    end type RBFCyclicConfig

  contains

    subroutine init_rbf_config(self, table, error)
      class(RBFCyclicConfig), intent(inout) :: self
      type(toml_table), intent(inout) :: table
      type(error_data), allocatable, intent(out) :: error

      call table%get_key(self%name)
      call get_value_converted(table, "active_dimensions", self%active_dims)
      call get_value_converted(table, "theta", self%lengthscale)

    end subroutine init_rbf_config

    subroutine info(self, unit)
      class(RBFCyclicConfig), intent(in) :: self
      integer, intent(in) :: unit
      integer :: i

      write(unit, '(A)') "RBF Cyclic Config"
      write(unit, '(" - ", A, T30, A)') "Name", self%name
      if (allocated(self%active_dims)) then
        write(unit, '(" - ", A, T30, (*(I0, 1x)))') &
	  "Active Dimensions", (self%active_dims(i), i=1,size(self%active_dims))
      end if
      if (allocated(self%lengthscale)) then
        write(unit, '(" - ", A, T30, (*(G0, 1x)))') "Theta", (self%lengthscale(i), i=1,size(self%lengthscale))
      end if
    end subroutine info
  end module rbf_cyclic_config_module
