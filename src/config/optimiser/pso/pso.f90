module particle_swarm_config_module
  use config_error, only: error_data
  use optimiser_config_module, only: OptimiserConfig
  use pso_swarm_updater_config_module, only: ParticleSwarmSwarmUpdaterConfig, &
                                             GlobalParticleSwarmConfig, &
                                             ForcedParticleSwarmConfig
  use pso_stopping_criteria_config_module, only: ParticleSwarmStoppingCriteriaConfig, &
                                                 NoStoppingCriteriaConfig, &
                                                 RelativeChangeStoppingCriteriaConfig, &
                                                 PotentialBasedCriteriaConfig, &
                                                 SignTestCriteriaConfig
  use pso_parameter_updater_config_module, only: ParticleSwarmParameterUpdaterConfig, &
                                                 ConstantParameterUpdaterConfig, &
                                                 UnifiedAdaptiveParameterConfig, &
                                                 IndividualAdaptiveParameterConfig, &
                                                 RandomUnifiedParameterUpdaterConfig, &
                                                 RandomIndividualParameterUpdaterConfig
  use kinds, only: wp
  use tomlf, only: toml_table, get_value, toml_key, new_table
  use utils, only: get_value_converted, get_table, is_empty_table, sanitise

  implicit none
  private
  public :: ParticleSwarmConfig

  type, extends(OptimiserConfig) :: ParticleSwarmConfig
    integer :: swarm_size = 50
    integer :: iterations = 1000
    real(kind=wp) :: inertia_weight = 0.7890
    real(kind=wp) :: cognitive_learning_rate = 1.4940
    real(kind=wp) :: social_learning_rate = 1.4940
    class(ParticleSwarmSwarmUpdaterConfig), allocatable :: swarm_updater
    class(ParticleSwarmParameterUpdaterConfig), allocatable :: parameter_updater
    class(ParticleSwarmStoppingCriteriaConfig), allocatable :: stopping_criteria
  contains
    private
    procedure, public, pass(self) :: init => init_particle_swarm_config
    procedure, public, pass(self) :: info
    procedure, public, pass(self) :: select_swarm_updater
    procedure, public, pass(self) :: select_stopping_criteria
    procedure, public, pass(self) :: select_parameter_updater
  end type ParticleSwarmConfig

contains

  subroutine init_particle_swarm_config(self, table, error)
    class(ParticleSwarmConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_data), allocatable, intent(out) :: error

    integer :: stat, i
    type(toml_key), allocatable :: list(:)

    call self%set_min_max(table)
    call get_value_converted(table, "swarm_size", self%swarm_size)
    call get_value_converted(table, "iterations", self%iterations)
    call get_value_converted(table, "inertia_weight", self%inertia_weight)
    call get_value_converted(table, "cognitive_learning_rate", self%cognitive_learning_rate)
    call get_value_converted(table, "social_learning_rate", self%social_learning_rate)

    call self%select_swarm_updater(table, error)
    call self%select_stopping_criteria(table, error)
    call self%select_parameter_updater(table, error)
  end subroutine init_particle_swarm_config

  subroutine select_stopping_criteria(self, table, error)
    class(ParticleSwarmConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_data), allocatable, intent(out) :: error
    character(len=:), allocatable :: stopping_criteria
    type(toml_table), pointer :: stopping_criteria_table

    call get_value(table, "stopping_criteria", stopping_criteria, "none")
    stopping_criteria = sanitise(stopping_criteria)

    call get_table(table, stopping_criteria, stopping_criteria_table)
    select case (stopping_criteria)
      case ("none")
        allocate(NoStoppingCriteriaConfig :: self%stopping_criteria)
        call self%stopping_criteria%init(stopping_criteria_table, error)
      case ("relative_change")
        allocate(RelativeChangeStoppingCriteriaConfig :: self%stopping_criteria)
        call self%stopping_criteria%init(stopping_criteria_table, error)
      case ("potential_based")
        allocate(PotentialBasedCriteriaConfig :: self%stopping_criteria)
        call self%stopping_criteria%init(stopping_criteria_table, error)
      case ("sign_test")
        allocate(SignTestCriteriaConfig :: self%stopping_criteria)
        call self%stopping_criteria%init(stopping_criteria_table, error)
    end select
  end subroutine select_stopping_criteria

  subroutine select_parameter_updater(self, table, error)
    class(ParticleSwarmConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_data), allocatable, intent(out) :: error
    character(len=:), allocatable :: parameter_updater
    type(toml_table), pointer :: parameter_updater_table

    call get_value(table, "parameter_updater", parameter_updater, "constant")
    parameter_updater = sanitise(parameter_updater)

    ! call get_value(table, parameter_updater, parameter_updater_table, requested=.false.)
    call get_table(table, parameter_updater, parameter_updater_table)
    select case (parameter_updater)
      case ("constant", "none")
        allocate(ConstantParameterUpdaterConfig :: self%parameter_updater)
        call self%parameter_updater%init(parameter_updater_table, error)
      case ("unified_adaptive", "uapso", "ua")
        if (is_empty_table(parameter_updater_table)) then
          call get_table(table, "adaptive", parameter_updater_table)
        end if
        if (is_empty_table(parameter_updater_table)) then
          call get_table(table, "individual_adaptive", parameter_updater_table)
        end if
        allocate(UnifiedAdaptiveParameterConfig :: self%parameter_updater)
        call self%parameter_updater%init(parameter_updater_table, error)
      case ("individual_adaptive", "iapso", "ia")
        if (is_empty_table(parameter_updater_table)) then
          call get_table(table, "adaptive", parameter_updater_table)
        end if
        if (is_empty_table(parameter_updater_table)) then
          call get_table(table, "unified_adaptive", parameter_updater_table)
        end if
        allocate(IndividualAdaptiveParameterConfig :: self%parameter_updater)
        call self%parameter_updater%init(parameter_updater_table, error)
      case ("random_unified", "random", "ru")
        allocate(RandomUnifiedParameterUpdaterConfig :: self%parameter_updater)
        call self%parameter_updater%init(parameter_updater_table, error)
      case ("random_individual", "ri")
        allocate(RandomIndividualParameterUpdaterConfig :: self%parameter_updater)
        call self%parameter_updater%init(parameter_updater_table, error)
      case default
        !> TODO convert this to fatal error
        print*, "Error unknown parameter updater: ", parameter_updater
        stop
    end select
  end subroutine select_parameter_updater

  subroutine select_swarm_updater(self, table, error)
    class(ParticleSwarmConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_data), allocatable, intent(out) :: error
    character(len=:), allocatable :: swarm_updater
    type(toml_table), pointer :: swarm_updater_table

    call get_value(table, "swarm_updater", swarm_updater, "global")
    swarm_updater = sanitise(swarm_updater)

    call get_table(table, swarm_updater, swarm_updater_table)
    select case (swarm_updater)
      case ("global", "default")
        allocate(GlobalParticleSwarmConfig :: self%swarm_updater)
        call self%swarm_updater%init(swarm_updater_table, error)
      case ("forced")
        allocate(ForcedParticleSwarmConfig :: self%swarm_updater)
        call self%swarm_updater%init(swarm_updater_table, error)
    end select
  end subroutine select_swarm_updater

  subroutine info(self, unit)
    class(ParticleSwarmConfig), intent(in) :: self
    integer, intent(in) :: unit

    write(unit, '(A)') "PSO Config"
    call self%optimiser_info(unit)
    write(unit, '(" - ", A, T30, I0)') "Swarm Size", self%swarm_size
    write(unit, '(" - ", A, T30, I0)') "Iterations", self%iterations
    write(unit, '(" - ", A, T30, G0)') "Inertia Weight", self%inertia_weight
    write(unit, '(" - ", A, T30, G0)') "Coginitive Learning Rate", self%cognitive_learning_rate
    write(unit, '(" - ", A, T30, G0)') "Social Learning Rate", self%social_learning_rate
    call self%swarm_updater%info(unit)
    call self%parameter_updater%info(unit)
    call self%stopping_criteria%info(unit)
  end subroutine info
end module particle_swarm_config_module