! MIT License
!
! Copyright (c) 2022 Popelier Group
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.

module optimiser_pso_module
  use kinds, only: wp
  use optimiser_type, only: Optimiser
  use gpr_module, only: GaussianProcessRegressor
  use optimiser_pso_particle, only: Particle, init_particle
  use optimiser_iter_result, only: IterResult
  use utils, only: random, euclidean_distance
  use constants, only: INFINITY
  use config_module, only: ParticleSwarmConfig, &
                           NoStoppingCriteriaConfig, &
                           RelativeChangeStoppingCriteriaConfig, &
                           ConstantParameterUpdaterConfig, &
                           UnifiedAdaptiveParameterConfig, &
                           AdaptiveParameterUpdaterConfig, &
                           IndividualAdaptiveParameterConfig, &
                           RandomUnifiedParameterUpdaterConfig, &
                           RandomIndividualParameterUpdaterConfig, &
                           GlobalParticleSwarmConfig, &
                           ForcedParticleSwarmConfig, &
                           ParticleSwarmSwarmUpdaterConfig, &
                           ParticleSwarmParameterUpdaterConfig, &
                           ParticleSwarmStoppingCriteriaConfig, &
                           OptimiserConfig
  use learning_automa_module, only: LearningAutomata

  use omp_lib, only: omp_get_num_threads, omp_get_max_threads

  implicit none
  private
  public :: ParticleSwarm

  type OpElement
    type(GaussianProcessRegressor), allocatable :: op
  contains
    procedure, public, pass(self) :: init => init_op_el
  end type OpElement


  ! Swarm Updater
  type, abstract :: SwarmUpdater
  contains
    private
    procedure(init_swarm_updater_interface), public, deferred, pass(self) :: init
    procedure(update_swarm_interface), public, deferred, pass(self) :: update
  end type SwarmUpdater

    ! Parameter Updater
  type, abstract :: ParameterUpdater
    logical :: update_per_particle_update = .false.
    logical :: update_per_swarm_update = .false.
  contains
    private
    procedure(init_parameter_updater_interface), public, deferred, pass(self) :: init
    procedure(update_parameters_interface), public, deferred, pass(self) :: update_parameters
  end type ParameterUpdater

  ! Stopping Criteria
  type, abstract :: StoppingCriteria
  contains
    private
    procedure(init_stopping_criteria_interface), public, deferred, pass(self) :: init
    procedure(check_interface), public, deferred, pass(self) :: check
  end type StoppingCriteria

  type, extends(Optimiser) :: ParticleSwarm
    type(Particle), dimension(:), allocatable :: particles

    real(kind=wp) :: inertia_weight
    real(kind=wp) :: cognitive_learning_rate
    real(kind=wp) :: social_learning_rate

    real(kind=wp), dimension(:), allocatable :: best_position
    real(kind=wp) :: best_cost

    type(IterResult), dimension(:), allocatable :: particle_results

    class(SwarmUpdater), allocatable :: swarm_updater
    class(ParameterUpdater), allocatable :: parameter_updater
    class(StoppingCriteria), allocatable :: stopping_criteria
  contains
    private
    procedure, public, pass(self) :: init => init_pso
    procedure, public, pass(self) :: next_iter => next_iter_pso
    ! procedure, public, pass(self) :: update_particles
    procedure, public, pass(self) :: nparticles
    procedure, public, pass(self) :: nfeatures
  end type ParticleSwarm

  abstract interface
    subroutine init_swarm_updater_interface(self, cfg)
      import SwarmUpdater
      import ParticleSwarmSwarmUpdaterConfig
      class(SwarmUpdater), intent(inout) :: self
      class(ParticleSwarmSwarmUpdaterConfig), intent(in) :: cfg
    end subroutine init_swarm_updater_interface

    subroutine update_swarm_interface(self, swarm)
      import SwarmUpdater
      import ParticleSwarm
      class(SwarmUpdater), intent(inout) :: self
      class(ParticleSwarm), intent(inout) :: swarm
    end subroutine update_swarm_interface

    subroutine init_stopping_criteria_interface(self, cfg)
      import StoppingCriteria
      import ParticleSwarmStoppingCriteriaConfig
      class(StoppingCriteria), intent(inout) :: self
      class(ParticleSwarmStoppingCriteriaConfig), intent(in) :: cfg
    end subroutine init_stopping_criteria_interface

    function check_interface(self, swarm) result(terminate)
      import StoppingCriteria
      import ParticleSwarm
      class(StoppingCriteria), intent(inout) :: self
      class(ParticleSwarm), intent(inout) :: swarm
      logical :: terminate
    end function check_interface

    subroutine init_parameter_updater_interface(self, swarm, cfg)
      import ParameterUpdater
      import ParticleSwarm
      import ParticleSwarmParameterUpdaterConfig
      class(ParameterUpdater), intent(inout) :: self
      type(ParticleSwarm), intent(inout) :: swarm
      class(ParticleSwarmParameterUpdaterConfig), intent(in) :: cfg
    end subroutine init_parameter_updater_interface

    subroutine update_parameters_interface(self, swarm, iparticle)
      import ParameterUpdater
      import ParticleSwarm
      class(ParameterUpdater), intent(inout) :: self
      class(ParticleSwarm), intent(inout) :: swarm
      integer, optional, intent(in) :: iparticle
    end subroutine update_parameters_interface
  end interface

  type, extends(SwarmUpdater) :: GlobalSwarmUpdater
  contains
    private
    procedure, public, pass(self) :: init => init_global_swarm_updater
    procedure, public, pass(self) :: update => global_swarm_update
  end type GlobalSwarmUpdater

  type, extends(SwarmUpdater) :: ForcedSwarmUpdater
    real(kind=wp) :: delta
  contains
    private
    procedure, public, pass(self) :: init => init_forced_swarm_updater
    procedure, public, pass(self) :: update => forced_swarm_update
  end type ForcedSwarmUpdater

  type, extends(StoppingCriteria) :: NoStoppingCriteria
  contains
    private
    procedure, public, pass(self) :: init => init_no_stopping_criteria
    procedure, public, pass(self) :: check => check_no_stopping_criteria
  end type NoStoppingCriteria

  type, extends(StoppingCriteria) :: RelativeChangeStoppingCriteria
    integer :: stall_iterations
    real(kind=wp) :: tolerance
    integer :: stall_count
    real(kind=wp) :: stall_cost
  contains
    private
    procedure, public, pass(self) :: init => init_relative_change_stopping_criteria
    procedure, public, pass(self) :: check => check_relative_change_stopping_criteria
  end type RelativeChangeStoppingCriteria

  type, extends(ParameterUpdater) :: ConstantParameterUpdater
  contains
    private
    procedure, public, pass(self) :: init => init_constant_parameter_updater
    procedure, public, pass(self) :: update_parameters => update_parameters_constant
  end type ConstantParameterUpdater

  type, abstract, extends(ParameterUpdater) :: AdaptiveParameterUpdater
    real(kind=wp) :: min_inertia_weight
    real(kind=wp) :: max_inertia_weight
    integer :: n_inertia_weight
    real(kind=wp) :: min_cognitive_learning_rate
    real(kind=wp) :: max_cognitive_learning_rate
    integer :: n_cognitive_learning_rate
    real(kind=wp) :: min_social_learning_rate
    real(kind=wp) :: max_social_learning_rate
    integer :: n_social_learning_rate
    real(kind=wp) :: success_threshold
    real(kind=wp), dimension(:), allocatable :: previous_costs

    type(LearningAutomata), allocatable :: inertia_weight_automata
    type(LearningAutomata), allocatable :: cognitive_learning_rate_automata
    type(LearningAutomata), allocatable :: social_learning_rate_automata
  contains
    private
    procedure, public, pass(self) :: initialise => init_adaptive_parameter_updater
    procedure, public, pass(self) :: update_parameters => update_parameters_adaptive
    procedure, public, pass(self) :: reward => reward_adaptive_parameter_updater
    procedure, public, pass(self) :: penalise => penalise_adaptive_parameter_updater
  end type AdaptiveParameterUpdater

  type, extends(AdaptiveParameterUpdater) :: UnifiedAdaptiveParameterUpdater
  contains
    procedure, public, pass(self) :: init => init_unified_adaptive_particle_swarm
  end type UnifiedAdaptiveParameterUpdater

  type, extends(AdaptiveParameterUpdater) :: IndividualAdaptiveParameterUpdater
  contains
    procedure, public, pass(self) :: init => init_individual_adaptive_particle_swarm
  end type IndividualAdaptiveParameterUpdater

  type, abstract, extends(ParameterUpdater) :: RandomParameterUpdater
  contains
    procedure, public, pass(self) :: update_parameters => update_parameters_random
  end type RandomParameterUpdater

  type, extends(RandomParameterUpdater) :: RandomUnifiedParameterUpdater
  contains
    procedure, public, pass(self) :: init => init_random_unified_particle_swarm
  end type RandomUnifiedParameterUpdater

  type, extends(RandomParameterUpdater) :: RandomIndividualParameterUpdater
  contains
    procedure, public, pass(self) :: init => init_random_individual_particle_swarm
  end type RandomIndividualParameterUpdater

contains

  subroutine init_pso(self, op, cfg)
    class(ParticleSwarm), intent(inout) :: self
    type(GaussianProcessRegressor), intent(in) :: op
    class(OptimiserConfig), intent(in) :: cfg

    integer :: nfeatures
    type(Particle), allocatable :: p
    type(IterResult), allocatable :: particle_result
    integer :: i, nfeats, nparams
    type(GaussianProcessRegressor), dimension(:), allocatable :: ops
    real(kind=wp), dimension(:), allocatable :: search_min, search_max
    type(Particle), dimension(:), allocatable :: particles
    type(IterResult), dimension(:), allocatable :: particle_results
    real(kind=wp), dimension(:,:), allocatable :: randpos, randvel

    allocate(self%op, source=op)
    ! self%op = op
    self%require_gradient = .false.
    self%require_hessian = .false.

    select type (cfg)
      type is (ParticleSwarmConfig)
        call self%init_iter_state(cfg%iterations)

        allocate(particles(cfg%swarm_size))
        allocate(particle_results(cfg%swarm_size))

        allocate(ops(cfg%swarm_size))
        do i = 1, cfg%swarm_size
          call ops(i)%init(self%op%config, self%op%training_set)
          ! op(i) = swarm%op
        end do

        nparams = self%op%nparams()

        allocate(search_min(nparams))
        allocate(search_max(nparams))

        call self%op%search_min(search_min)
        call self%op%search_max(search_max)

        call random(randpos, nparams, cfg%swarm_size, search_min, search_max)
        call random(randvel, nparams, cfg%swarm_size, search_min-search_max, search_max-search_min)

        !$omp parallel do private(i, particle_result)
        do i = 1, cfg%swarm_size
          call init_particle(  &
            particles(i),      &
            i,                 &
            ops(i),            &
            randpos(:,i),      &
            randvel(:,i),      &
            search_min,        &
            search_max,        &
            particle_result    &
          )
          particle_results(i) = particle_result
        end do
        !$omp end parallel do

        allocate(self%particles(cfg%swarm_size))
        do i = 1, cfg%swarm_size
          self%particles(i)%id = particles(i)%id
          self%particles(i)%position = particles(i)%position
          self%particles(i)%velocity = particles(i)%velocity
          self%particles(i)%cost = particles(i)%cost
          self%particles(i)%best_position = particles(i)%best_position
          self%particles(i)%best_cost = particles(i)%best_cost
          self%particles(i)%min = particles(i)%min
          self%particles(i)%max = particles(i)%max
        end do

        allocate(self%particle_results(cfg%swarm_size))
        do i = 1, cfg%swarm_size
          self%particle_results(i)%param = particle_results(i)%param
          self%particle_results(i)%cost = particle_results(i)%cost
          call self%update(particle_results(i))
        end do


        self%inertia_weight = cfg%inertia_weight
        self%cognitive_learning_rate = cfg%cognitive_learning_rate
        self%social_learning_rate = cfg%social_learning_rate

        allocate(self%best_position, source=self%state%best_param)
        self%best_position = self%state%best_param
        self%best_cost = self%state%best_cost

        select type (swarm_updater_config => cfg%swarm_updater)
          type is (GlobalParticleSwarmConfig)
            allocate(GlobalSwarmUpdater :: self%swarm_updater)
          type is (ForcedParticleSwarmConfig)
            allocate(ForcedSwarmUpdater :: self%swarm_updater)
        end select
        call self%swarm_updater%init(cfg%swarm_updater)

        select type (parameter_config => cfg%parameter_updater)
          type is (ConstantParameterUpdaterConfig)
            allocate(ConstantParameterUpdater :: self%parameter_updater)
          type is (UnifiedAdaptiveParameterConfig)
            allocate(UnifiedAdaptiveParameterUpdater :: self%parameter_updater)
          type is (IndividualAdaptiveParameterConfig)
            allocate(IndividualAdaptiveParameterUpdater :: self%parameter_updater)
          type is (RandomUnifiedParameterUpdaterConfig)
            allocate(RandomUnifiedParameterUpdater :: self%parameter_updater)
          type is (RandomIndividualParameterUpdaterConfig)
            allocate(RandomIndividualParameterUpdater :: self%parameter_updater)
        end select
        call self%parameter_updater%init(self, cfg%parameter_updater)

        select type (stopping_criteria_config => cfg%stopping_criteria)
          type is (NoStoppingCriteriaConfig)
            allocate(NoStoppingCriteria :: self%stopping_criteria)
          type is (RelativeChangeStoppingCriteriaConfig)
            allocate(RelativeChangeStoppingCriteria :: self%stopping_criteria)
        end select
        call self%stopping_criteria%init(cfg%stopping_criteria)
    end select
  end subroutine init_pso

  subroutine init_op_el(self, op)
    class(OpElement), intent(inout) :: self
    type(GaussianProcessRegressor), intent(in) :: op
    allocate(self%op, source=op)
  end subroutine init_op_el

  subroutine init_global_swarm_updater(self, cfg)
    class(GlobalSwarmUpdater), intent(inout) :: self
    ! type(GlobalParticleSwarmConfig), intent(in) :: cfg
    class(ParticleSwarmSwarmUpdaterConfig), intent(in) :: cfg
  end subroutine init_global_swarm_updater

  subroutine global_swarm_update(self, swarm)
    class(GlobalSwarmUpdater), intent(inout) :: self
    class(ParticleSwarm), intent(inout) :: swarm

    type(IterResult), allocatable :: particle_result
    real(kind=wp), dimension(:), allocatable :: momentum
    real(kind=wp), dimension(:), allocatable :: to_optimum
    real(kind=wp), dimension(:), allocatable :: to_global_optimum
    ! real(kind=wp), dimension(:), allocatable :: random_array
    real(kind=wp), dimension(:), allocatable :: new_position
    real(kind=wp), dimension(:,:), allocatable :: rand_mat1, rand_mat2
    type(GaussianProcessRegressor), dimension(:), allocatable :: op
    integer :: i, j, nparticles, nfeatures

    nfeatures = swarm%nfeatures()
    nparticles = swarm%nparticles()

    allocate(momentum(nfeatures))
    allocate(to_optimum(nfeatures))
    allocate(to_global_optimum(nfeatures))
    ! allocate(random_array(nfeatures))
    allocate(new_position(nfeatures))

    allocate(rand_mat1(nfeatures, nparticles))
    allocate(rand_mat2(nfeatures, nparticles))

    call random(rand_mat1, nfeatures, nparticles)
    call random(rand_mat2, nfeatures, nparticles)

    allocate(op(nparticles))
    do i = 1, nparticles
      call op(i)%init(swarm%op%config, swarm%op%training_set)
    end do

    ! allocate(op(nparticles))
    ! op = swarm%op

    !$omp parallel private(i, momentum, to_optimum, to_global_optimum, particle_result, new_position)
    !$omp do
    do i = 1, nparticles
      momentum = swarm%inertia_weight * swarm%particles(i)%velocity

      to_optimum = swarm%particles(i)%best_position - swarm%particles(i)%position
      to_optimum = swarm%cognitive_learning_rate * rand_mat1(:,i) * to_optimum

      to_global_optimum = swarm%best_position - swarm%particles(i)%position
      to_global_optimum = swarm%social_learning_rate * rand_mat2(:,i) * to_global_optimum

      swarm%particles(i)%velocity = momentum + to_optimum + to_global_optimum
      new_position = swarm%particles(i)%position + swarm%particles(i)%velocity

      !> TODO: Implement search boundaries
      swarm%particles(i)%position = new_position

      ! particle_result = swarm%particles(i)%get_cost(swarm%op)
      particle_result = swarm%particles(i)%get_cost(op(i))
      swarm%particles(i)%cost = particle_result%cost

      if (swarm%particles(i)%cost .le. swarm%particles(i)%best_cost) then
        swarm%particles(i)%best_position = swarm%particles(i)%position
        swarm%particles(i)%best_cost = swarm%particles(i)%cost
      end if

      swarm%particle_results(i) = particle_result

      ! if (swarm%parameter_updater%update_per_particle_update .eqv. .true.) then
      !   call swarm%parameter_updater%update_parameters(swarm, i)
      ! end if
    end do
    !$omp end do
    !$omp end parallel

    do i = 1, nparticles
      if (swarm%particles(i)%cost .le. swarm%best_cost) then
        swarm%best_position = swarm%particles(i)%position
        swarm%best_cost = swarm%particles(i)%cost
      end if
    end do

    deallocate(momentum)
    deallocate(to_optimum)
    deallocate(to_global_optimum)
    deallocate(new_position)
    deallocate(rand_mat1)
    deallocate(rand_mat2)

    ! do i = 1, nparticles
    !   deallocate(op(i))
    ! end do
    deallocate(op)
  end subroutine global_swarm_update

  subroutine init_forced_swarm_updater(self, cfg)
    class(ForcedSwarmUpdater), intent(inout) :: self
    ! type(ForcedParticleSwarmConfig), intent(in) :: cfg
    class(ParticleSwarmSwarmUpdaterConfig), intent(in) :: cfg

    select type (cfg)
      type is (ForcedParticleSwarmConfig)
        self%delta = cfg%delta
    end select
  end subroutine init_forced_swarm_updater

  subroutine forced_swarm_update(self, swarm)
    class(ForcedSwarmUpdater), intent(inout) :: self
    class(ParticleSwarm), intent(inout) :: swarm

    type(IterResult), allocatable :: particle_result
    real(kind=wp), dimension(:), allocatable :: momentum
    real(kind=wp), dimension(:), allocatable :: to_optimum
    real(kind=wp), dimension(:), allocatable :: to_global_optimum
    real(kind=wp), dimension(:), allocatable :: random_array
    real(kind=wp), dimension(:), allocatable :: new_position
    integer :: i, j, nparticles, nfeatures
    real(kind=wp), dimension(:), allocatable :: partial_potential
    real(kind=wp), dimension(:), allocatable :: delta_velocity

    nfeatures = swarm%nfeatures()
    nparticles = swarm%nparticles()

    allocate(momentum(nfeatures))
    allocate(to_optimum(nfeatures))
    allocate(to_global_optimum(nfeatures))
    allocate(random_array(nfeatures))
    allocate(new_position(nfeatures))
    allocate(partial_potential(nfeatures))
    allocate(delta_velocity(nfeatures))

    do i = 1, nparticles
      momentum = swarm%inertia_weight * swarm%particles(i)%velocity

      call random(random_array, nfeatures)
      to_optimum = swarm%particles(i)%best_position - swarm%particles(i)%position
      to_optimum = swarm%cognitive_learning_rate * random_array * to_optimum

      call random(random_array, nfeatures)
      to_global_optimum = swarm%best_position - swarm%particles(i)%position
      to_global_optimum = swarm%social_learning_rate * random_array * to_global_optimum

      swarm%particles(i)%velocity = momentum + to_optimum + to_global_optimum
      partial_potential = abs(swarm%particles(i)%velocity) + abs(swarm%best_position - swarm%particles(i)%position)

      if (any(partial_potential .lt. self%delta)) then
        call random(delta_velocity, nfeatures, -self%delta, self%delta)
        swarm%particles(i)%velocity = merge(delta_velocity, swarm%particles(i)%velocity, partial_potential .lt. self%delta)
      end if

      new_position = swarm%particles(i)%position + swarm%particles(i)%velocity

      !> TODO: Implement search boundaries
      swarm%particles(i)%position = new_position
      particle_result = swarm%particles(i)%get_cost(swarm%op)
      swarm%particles(i)%cost = particle_result%cost

      if (swarm%particles(i)%cost .le. swarm%particles(i)%best_cost) then
        swarm%particles(i)%best_position = swarm%particles(i)%position
        swarm%particles(i)%best_cost = swarm%particles(i)%cost

        if (swarm%particles(i)%cost .le. swarm%best_cost) then
          swarm%best_position = swarm%particles(i)%position
          swarm%best_cost = swarm%particles(i)%cost
        end if
      end if

      swarm%particle_results(i) = particle_result

      if (swarm%parameter_updater%update_per_particle_update .eqv. .true.) then
        call swarm%parameter_updater%update_parameters(swarm, i)
      end if
    end do

    deallocate(momentum)
    deallocate(to_optimum)
    deallocate(to_global_optimum)
    deallocate(random_array)
    deallocate(new_position)
  end subroutine forced_swarm_update

  function next_iter_pso(self) result(iter_result)
    class(ParticleSwarm), intent(inout) :: self
    type(IterResult), allocatable :: iter_result
    type(IterResult), dimension(:), allocatable :: particle_results
    logical :: terminate

    call self%swarm_updater%update(self)

    if (self%parameter_updater%update_per_swarm_update .eqv. .true.) then
      call self%parameter_updater%update_parameters(self)
    end if
    terminate = self%stopping_criteria%check(self)

    self%state%terminated = terminate
    allocate(iter_result)
    call iter_result%init(self%best_position, self%best_cost)
  end function next_iter_pso

  function nparticles(self)
    class(ParticleSwarm), intent(in) :: self
    integer :: nparticles
    nparticles = size(self%particles)
  end function nparticles

  function nfeatures(self)
    class(ParticleSwarm), intent(in) :: self
    integer :: nfeatures
    nfeatures = self%particles(1)%nfeatures()
  end function nfeatures

  ! Stopping Criteria
  subroutine init_no_stopping_criteria(self, cfg)
    class(NoStoppingCriteria), intent(inout) :: self
    ! type(NoStoppingCriteriaConfig), intent(in) :: cfg
    class(ParticleSwarmStoppingCriteriaConfig), intent(in) :: cfg
  end subroutine init_no_stopping_criteria

  function check_no_stopping_criteria(self, swarm) result(terminate)
    class(NoStoppingCriteria), intent(inout) :: self
    class(ParticleSwarm), intent(inout) :: swarm
    logical :: terminate

    terminate = .false.
  end function check_no_stopping_criteria

  subroutine init_relative_change_stopping_criteria(self, cfg)
    class(RelativeChangeStoppingCriteria), intent(inout) :: self
    ! type(RelativeChangeStoppingCriteriaConfig), intent(in) :: cfg
    class(ParticleSwarmStoppingCriteriaConfig), intent(in) :: cfg

    select type (cfg)
      type is (RelativeChangeStoppingCriteriaConfig)
        self%stall_iterations = cfg%stall_iterations
        self%tolerance = cfg%tolerance
        self%stall_cost = INFINITY()
        self%stall_count = 0
    end select
  end subroutine init_relative_change_stopping_criteria

  function check_relative_change_stopping_criteria(self, swarm) result(terminate)
    class(RelativeChangeStoppingCriteria), intent(inout) :: self
    class(ParticleSwarm), intent(inout) :: swarm
    logical :: terminate

    if (abs((swarm%best_cost - self%stall_cost)/self%stall_cost) .le. self%tolerance) then
      self%stall_count = self%stall_count + 1
    else
      self%stall_count = 0
      self%stall_cost = swarm%best_cost
    end if

    if (self%stall_count .ge. self%stall_iterations) then
      terminate = .true.
    else
      terminate = .false.
    end if
  end function check_relative_change_stopping_criteria

  subroutine init_constant_parameter_updater(self, swarm, cfg)
    class(ConstantParameterUpdater), intent(inout) :: self
    type(ParticleSwarm), intent(inout) :: swarm
    ! class(ConstantParameterUpdaterConfig), intent(in) :: cfg
    class(ParticleSwarmParameterUpdaterConfig), intent(in) :: cfg

    select type (cfg)
      type is (ConstantParameterUpdaterConfig)
        self%update_per_particle_update = .false.
        self%update_per_swarm_update = .false.
    end select
  end subroutine init_constant_parameter_updater

  subroutine update_parameters_constant(self, swarm, iparticle)
    class(ConstantParameterUpdater), intent(inout) :: self
    class(ParticleSwarm), intent(inout) :: swarm
    integer, optional, intent(in) :: iparticle
  end subroutine update_parameters_constant

  subroutine init_adaptive_parameter_updater(self, swarm, cfg)
    class(AdaptiveParameterUpdater), intent(inout) :: self
    type(ParticleSwarm), intent(inout) :: swarm
    ! class(AdaptiveParameterUpdaterConfig), intent(in) :: cfg
    class(ParticleSwarmParameterUpdaterConfig), intent(in) :: cfg
    integer :: i

    select type (cfg)
      class is (AdaptiveParameterUpdaterConfig)
        self%min_inertia_weight = cfg%min_inertia_weight
        self%max_inertia_weight = cfg%max_inertia_weight
        self%n_inertia_weight = cfg%n_inertia_weight
        self%min_cognitive_learning_rate = cfg%min_cognitive_learning_rate
        self%max_cognitive_learning_rate = cfg%max_cognitive_learning_rate
        self%n_cognitive_learning_rate = cfg%n_cognitive_learning_rate
        self%min_social_learning_rate = cfg%min_social_learning_rate
        self%max_social_learning_rate = cfg%max_social_learning_rate
        self%n_social_learning_rate = cfg%n_social_learning_rate
        self%success_threshold = cfg%success_threshold

        allocate(self%previous_costs(swarm%nparticles()))
        do i = 1, swarm%nparticles()
          self%previous_costs(i) = swarm%particle_results(i)%cost
        end do

        allocate(self%inertia_weight_automata)
        call self%inertia_weight_automata%init( &
          self%min_inertia_weight, &
          self%max_inertia_weight, &
          self%n_inertia_weight, &
          cfg%reward_step_length, &
          cfg%penalty_step_length &
        )

        allocate(self%cognitive_learning_rate_automata)
        call self%cognitive_learning_rate_automata%init( &
          self%min_cognitive_learning_rate, &
          self%max_cognitive_learning_rate, &
          self%n_cognitive_learning_rate, &
          cfg%reward_step_length, &
          cfg%penalty_step_length &
        )

        allocate(self%social_learning_rate_automata)
        call self%social_learning_rate_automata%init( &
          self%min_social_learning_rate, &
          self%max_social_learning_rate, &
          self%n_social_learning_rate, &
          cfg%reward_step_length, &
          cfg%penalty_step_length &
        )
    end select

    swarm%inertia_weight = self%inertia_weight_automata%choose()
    swarm%cognitive_learning_rate = self%cognitive_learning_rate_automata%choose()
    swarm%social_learning_rate = self%social_learning_rate_automata%choose()
  end subroutine init_adaptive_parameter_updater

  subroutine init_unified_adaptive_particle_swarm(self, swarm, cfg)
    class(UnifiedAdaptiveParameterUpdater), intent(inout) :: self
    type(ParticleSwarm), intent(inout) :: swarm
    ! class(AdaptiveParameterUpdaterConfig), intent(in) :: cfg
    class(ParticleSwarmParameterUpdaterConfig), intent(in) :: cfg

    self%update_per_particle_update = .false.
    self%update_per_swarm_update = .true.
    call self%initialise(swarm, cfg)
  end subroutine init_unified_adaptive_particle_swarm

  subroutine init_individual_adaptive_particle_swarm(self, swarm, cfg)
    class(IndividualAdaptiveParameterUpdater), intent(inout) :: self
    type(ParticleSwarm), intent(inout) :: swarm
    ! class(AdaptiveParameterUpdaterConfig), intent(in) :: cfg
    class(ParticleSwarmParameterUpdaterConfig), intent(in) :: cfg

    self%update_per_particle_update = .true.
    self%update_per_swarm_update = .false.
    call self%initialise(swarm, cfg)
  end subroutine init_individual_adaptive_particle_swarm

  subroutine update_parameters_adaptive(self, swarm, iparticle)
    class(AdaptiveParameterUpdater), intent(inout) :: self
    class(ParticleSwarm), intent(inout) :: swarm
    integer, optional, intent(in) :: iparticle
    integer :: i, n_improved, nparticles, ipart

    if (present(iparticle)) then
      nparticles = 1
      ipart = iparticle
    else
      nparticles = swarm%nparticles()
      ipart = 1
    end if

    n_improved = 0
    do i = ipart, (ipart-1)+nparticles
      !> NOTE: Not sure if this should be lt or le
      if (swarm%particle_results(i)%cost .lt. self%previous_costs(i)) then
        n_improved = n_improved + 1
      end if
      self%previous_costs(i) = swarm%particle_results(i)%cost
    end do

    if ((n_improved/nparticles).ge.self%success_threshold) then
      call self%reward()
    else
      call self%penalise()
    end if

    swarm%inertia_weight = self%inertia_weight_automata%choose()
    swarm%cognitive_learning_rate = self%cognitive_learning_rate_automata%choose()
    swarm%social_learning_rate = self%social_learning_rate_automata%choose()

    ! print*, "UAPSO",swarm%inertia_weight , swarm%cognitive_learning_rate, swarm%social_learning_rate
  end subroutine update_parameters_adaptive

  subroutine reward_adaptive_parameter_updater(self)
    class(AdaptiveParameterUpdater), intent(inout) :: self

    call self%inertia_weight_automata%reward()
    call self%cognitive_learning_rate_automata%reward()
    call self%social_learning_rate_automata%reward()
  end subroutine reward_adaptive_parameter_updater

  subroutine penalise_adaptive_parameter_updater(self)
    class(AdaptiveParameterUpdater), intent(inout) :: self

    call self%inertia_weight_automata%penalise()
    call self%cognitive_learning_rate_automata%penalise()
    call self%social_learning_rate_automata%penalise()
  end subroutine penalise_adaptive_parameter_updater

  subroutine init_random_unified_particle_swarm(self, swarm, cfg)
    class(RandomUnifiedParameterUpdater), intent(inout) :: self
    type(ParticleSwarm), intent(inout) :: swarm
    class(ParticleSwarmParameterUpdaterConfig), intent(in) :: cfg

    self%update_per_particle_update = .false.
    self%update_per_swarm_update = .true.

    call self%update_parameters(swarm)
  end subroutine init_random_unified_particle_swarm

  subroutine init_random_individual_particle_swarm(self, swarm, cfg)
    class(RandomIndividualParameterUpdater), intent(inout) :: self
    type(ParticleSwarm), intent(inout) :: swarm
    class(ParticleSwarmParameterUpdaterConfig), intent(in) :: cfg

    self%update_per_particle_update = .true.
    self%update_per_swarm_update = .false.

    call self%update_parameters(swarm)
  end subroutine init_random_individual_particle_swarm

  subroutine update_parameters_random(self, swarm, iparticle)
    class(RandomParameterUpdater), intent(inout) :: self
    class(ParticleSwarm), intent(inout) :: swarm
    integer, optional, intent(in) :: iparticle
    real(kind=wp) :: inertia_ineq

    ! Must satisfy inquality
    ! c1+c2 < 24(1-w^2)/(7-5w)
    ! https://doi.org/10.1007/s11721-017-0150-9

    call random(swarm%inertia_weight, 0.0_wp, 1.0_wp)
    inertia_ineq = 24*(1-swarm%inertia_weight*swarm%inertia_weight)/(7-5*swarm%inertia_weight)
    call random(swarm%cognitive_learning_rate, 0.0_wp, inertia_ineq)
    call random(swarm%social_learning_rate, 0.0_wp, inertia_ineq-swarm%cognitive_learning_rate)
  end subroutine update_parameters_random
end module optimiser_pso_module
