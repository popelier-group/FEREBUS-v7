module model_config_module
  use, intrinsic :: iso_fortran_env, only : error_unit, output_unit
  use kinds, only: wp
  use tomlf, only: get_value, toml_table, toml_key, new_table
  use config_error, only : error_data
  use utils, only: to_lower
  !> TODO: Cleanup these import statements
  use particle_swarm_config_module, only: ParticleSwarmConfig
  ! use unified_adaptive_particle_swarm_config_module, only: UnifiedAdaptiveParticleSwarmConfig
  use optimiser_config_module, only: OptimiserConfig
  use config_type_module, only: ConfigType
  use mean_config_module, only: MeanConfig
  use constant_mean_config_module, only: ConstantMeanConfig
  use zero_mean_config_module, only: ZeroMeanConfig
  use linear_mean_config_module, only: LinearMeanConfig
  use quadratic_mean_config_module, only: QuadraticMeanConfig

  implicit none
  private
  public :: ModelConfig

  type, extends(ConfigType) :: ModelConfig
    class(MeanConfig), allocatable :: mean
    class(OptimiserConfig), allocatable :: optimiser
    character(len=:), allocatable :: kernel
    character(len=:), allocatable :: likelihood
    real(kind=wp) :: noise = 1e-10_wp
    logical :: optimise_noise = .false.
    logical :: standardise = .false.
  contains
    private
    procedure, public, pass(self) :: init => init_model_config
    procedure, public, pass(self) :: info
  end type ModelConfig

contains

  subroutine init_model_config(self, table, parent_table, error)
    class(ModelConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(toml_table), intent(inout) :: parent_table
    type(error_data), allocatable, intent(out) :: error

    type(toml_table), pointer :: optimiser_table
    character(len=:), allocatable :: tmp

    call get_value(table, "mean", tmp, "constant")
    tmp = to_lower(tmp)
    select case (tmp)
      case ("constant")
        allocate(ConstantMeanConfig :: self%mean)
        call self%mean%init(table, error)
      case ("zero")
        allocate(ZeroMeanConfig :: self%mean)
        call self%mean%init(table, error)
      case ("linear")
        allocate(LinearMeanConfig :: self%mean)
        call self%mean%init(table, error)
      case ("quadratic")
        allocate(QuadraticMeanConfig :: self%mean)
        call self%mean%init(table, error)
      case default
        !> TODO Convert this to fatal error
        print*, "TODO: Convert this to error"
        print*, "Unknown Mean Type: ", tmp
        stop
    end select

    call get_value(table, "optimiser", tmp, "pso")
    tmp = to_lower(tmp)
    call get_optimiser_table(parent_table, tmp, optimiser_table)
    select case (tmp)
      case ("pso")
        allocate(ParticleSwarmConfig :: self%optimiser)
        call self%optimiser%init(optimiser_table, error)
      case default
        !> TODO Convert this to fatal error
        print*, "TODO: Convert this to error"
        print*, "Unknown Optimiser: ", tmp
        stop
    end select
    call self%optimiser%initialise(parent_table, error)

    ! This may need a check, could be put here or when the kernel is being composed
    call get_value(table, "kernel", self%kernel)

    ! May be useful in the future to turn noise into a separate Config Type
    ! For now kept it simple with just a value and a boolean
    call get_value(table, "noise", self%noise)
    call get_value(table, "optimise_noise", self%optimise_noise)

    call get_value(table, "standardise", self%standardise)

    call get_value(table, "likelihood", self%likelihood, "marginal")
    self%likelihood = to_lower(self%likelihood)
  end subroutine init_model_config

  subroutine info(self, unit)
    class(ModelConfig), intent(in) :: self
    integer, intent(in) :: unit

    call self%mean%info(unit)
    call self%optimiser%info(unit)
  end subroutine info

  subroutine get_optimiser_table(table, optimiser, child)
    type(toml_table), intent(inout) :: table
    character(len=*), intent(in) :: optimiser
    type(toml_table), pointer, intent(out) :: child
    type(toml_table), pointer :: optimiser_table

    call get_value(table, "optimiser", optimiser_table, requested=.false.)
    if (associated(optimiser_table)) then
      call get_value(optimiser_table, optimiser, child)
    end if

    if (.not.associated(child)) then
      allocate(child)
      call new_table(child)
    end if
  end subroutine get_optimiser_table
end module model_config_module
