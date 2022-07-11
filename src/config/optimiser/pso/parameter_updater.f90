module pso_parameter_updater_config_module
  use kinds, only: wp
  use config_type_module, only: ConfigType
  use tomlf, only: toml_table, get_value
  use utils, only: get_value_converted
  use config_error, only: error_data

  implicit none
  private
  public :: ParticleSwarmParameterUpdaterConfig
  public :: ConstantParameterUpdaterConfig
  public :: AdaptiveParameterUpdaterConfig
  public :: UnifiedAdaptiveParameterConfig
  public :: IndividualAdaptiveParameterConfig
  public :: RandomUnifiedParameterUpdaterConfig
  public :: RandomIndividualParameterUpdaterConfig

  type, abstract, extends(ConfigType) :: ParticleSwarmParameterUpdaterConfig
  contains
    private
    procedure(init_interface), public, deferred, pass(self) :: init
  end type ParticleSwarmParameterUpdaterConfig

  abstract interface
    subroutine init_interface(self, table, error)
      import ParticleSwarmParameterUpdaterConfig
      import toml_table
      import error_data
      class(ParticleSwarmParameterUpdaterConfig), intent(inout) :: self
      type(toml_table), intent(inout) :: table
      type(error_data), allocatable, intent(out) :: error
    end subroutine init_interface
  end interface

  type, extends(ParticleSwarmParameterUpdaterConfig) :: ConstantParameterUpdaterConfig
  contains
    private
    procedure, public, pass(self) :: init => init_constant_parameter_updater_config
    procedure, public, pass(self) :: info => constant_parameter_updater_info
  end type ConstantParameterUpdaterConfig

  type, abstract, extends(ParticleSwarmParameterUpdaterConfig) :: AdaptiveParameterUpdaterConfig
    real(kind=wp) :: min_inertia_weight = 0.0_wp
    real(kind=wp) :: max_inertia_weight = 1.0_wp
    integer :: n_inertia_weight = 10
    real(kind=wp) :: min_cognitive_learning_rate = 0.0_wp
    real(kind=wp) :: max_cognitive_learning_rate = 4.0_wp
    integer :: n_cognitive_learning_rate = 10
    real(kind=wp) :: min_social_learning_rate = 0.0_wp
    real(kind=wp) :: max_social_learning_rate = 4.0_wp
    integer :: n_social_learning_rate = 10
    real(kind=wp) :: reward_step_length = 0.01_wp
    real(kind=wp) :: penalty_step_length = 0.01_wp
    real(kind=wp) :: success_threshold = 0.5_wp
  contains
    private
    procedure, public, pass(self) :: init => init_adaptive_parameter_config
    procedure, public, pass(self) :: adaptive_parameter_info
  end type AdaptiveParameterUpdaterConfig

  type, extends(AdaptiveParameterUpdaterConfig) :: UnifiedAdaptiveParameterConfig
  contains
    private
    procedure, public, pass(self) :: info => unified_adaptive_parameter_updater_info
  end type UnifiedAdaptiveParameterConfig

  type, extends(AdaptiveParameterUpdaterConfig) :: IndividualAdaptiveParameterConfig
  contains
    private
    procedure, public, pass(self) :: info => individual_adaptive_parameter_updater_info
  end type IndividualAdaptiveParameterConfig

  type, extends(ParticleSwarmParameterUpdaterConfig) :: RandomUnifiedParameterUpdaterConfig
  contains
    private
    procedure, public, pass(self) :: init => init_random_unified_parameter_updater_config
    procedure, public, pass(self) :: info => random_unified_parameter_updater_info
  end type RandomUnifiedParameterUpdaterConfig

  type, extends(ParticleSwarmParameterUpdaterConfig) :: RandomIndividualParameterUpdaterConfig
  contains
    private
    procedure, public, pass(self) :: init => init_random_individual_parameter_updater_config
    procedure, public, pass(self) :: info => random_individual_parameter_updater_info
  end type RandomIndividualParameterUpdaterConfig

contains

  subroutine constant_parameter_updater_info(self, unit)
    class(ConstantParameterUpdaterConfig), intent(in) :: self
    integer, intent(in) :: unit

    write(unit, '(" - ", A, T30, A)') "Parameter Updater", "Constant"
  end subroutine constant_parameter_updater_info

  subroutine init_constant_parameter_updater_config(self, table, error)
    class(ConstantParameterUpdaterConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_data), allocatable, intent(out) :: error
  end subroutine init_constant_parameter_updater_config

  subroutine adaptive_parameter_info(self, unit)
    class(AdaptiveParameterUpdaterConfig), intent(in) :: self
    integer, intent(in) :: unit

    write(unit, '("    - ", A, T40, G0)') "Min Inertia Weight", self%min_inertia_weight
    write(unit, '("    - ", A, T40, G0)') "Max Inertia Weight", self%max_inertia_weight
    write(unit, '("    - ", A, T40, G0)') "N Inertia Weight", self%n_inertia_weight
    write(unit, '("    - ", A, T40, G0)') "Min Cognitive Learning Rate", self%min_cognitive_learning_rate
    write(unit, '("    - ", A, T40, G0)') "Max Cognitive Learning Rate", self%max_cognitive_learning_rate
    write(unit, '("    - ", A, T40, G0)') "N Cognitive Learning Rate", self%n_cognitive_learning_rate
    write(unit, '("    - ", A, T40, G0)') "Min Social Learning Rate", self%min_social_learning_rate
    write(unit, '("    - ", A, T40, G0)') "Max Social Learning Rate", self%max_social_learning_rate
    write(unit, '("    - ", A, T40, G0)') "N Social Learning Rate", self%n_social_learning_rate
    write(unit, '("    - ", A, T40, G0)') "Reward Step Length", self%reward_step_length
    write(unit, '("    - ", A, T40, G0)') "Penalty Step Length", self%penalty_step_length
    write(unit, '("    - ", A, T40, G0)') "Success Threshold", self%success_threshold
  end subroutine adaptive_parameter_info

  subroutine unified_adaptive_parameter_updater_info(self, unit)
    class(UnifiedAdaptiveParameterConfig), intent(in) :: self
    integer, intent(in) :: unit

    write(unit, '(" - ", A, T30, A)') "Parameter Updater", "Unified Adaptive"
    call self%adaptive_parameter_info(unit)
  end subroutine unified_adaptive_parameter_updater_info

  subroutine init_adaptive_parameter_config(self, table, error)
    class(AdaptiveParameterUpdaterConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_data), allocatable, intent(out) :: error

    call get_value_converted(table, "min_inertia_weight", self%min_inertia_weight)
    call get_value_converted(table, "max_inertia_weight", self%max_inertia_weight)
    call get_value_converted(table, "n_inertia_weight", self%n_inertia_weight)

    call get_value_converted(table, "min_cognitive_learning_rate", self%min_cognitive_learning_rate)
    call get_value_converted(table, "max_cognitive_learning_rate", self%max_cognitive_learning_rate)
    call get_value_converted(table, "n_cognitive_learning_rate", self%n_cognitive_learning_rate)

    call get_value_converted(table, "min_social_learning_rate", self%min_social_learning_rate)
    call get_value_converted(table, "max_social_learning_rate", self%max_social_learning_rate)
    call get_value_converted(table, "n_social_learning_rate", self%n_social_learning_rate)

    call get_value_converted(table, "reward_step_length", self%reward_step_length)
    call get_value_converted(table, "penalty_step_length", self%penalty_step_length)
    call get_value_converted(table, "success_threshold", self%success_threshold)
  end subroutine init_adaptive_parameter_config

  subroutine individual_adaptive_parameter_updater_info(self, unit)
    class(IndividualAdaptiveParameterConfig), intent(in) :: self
    integer, intent(in) :: unit

    write(unit, '(" - ", A, T30, A)') "Parameter Updater", "Individual Adaptive"
    call self%adaptive_parameter_info(unit)
  end subroutine individual_adaptive_parameter_updater_info

  subroutine random_unified_parameter_updater_info(self, unit)
    class(RandomUnifiedParameterUpdaterConfig), intent(in) :: self
    integer, intent(in) :: unit

    write(unit, '(" - ", A, T30, A)') "Parameter Updater", "Random-Unified"
  end subroutine random_unified_parameter_updater_info

  subroutine init_random_unified_parameter_updater_config(self, table, error)
    class(RandomUnifiedParameterUpdaterConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_data), allocatable, intent(out) :: error
  end subroutine init_random_unified_parameter_updater_config

  subroutine random_individual_parameter_updater_info(self, unit)
    class(RandomIndividualParameterUpdaterConfig), intent(in) :: self
    integer, intent(in) :: unit

    write(unit, '(" - ", A, T30, A)') "Parameter Updater", "Random-Individual"
  end subroutine random_individual_parameter_updater_info

  subroutine init_random_individual_parameter_updater_config(self, table, error)
    class(RandomIndividualParameterUpdaterConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_data), allocatable, intent(out) :: error
  end subroutine init_random_individual_parameter_updater_config
end module pso_parameter_updater_config_module