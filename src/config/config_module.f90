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

module config_module
  use, intrinsic :: iso_fortran_env, only : error_unit, output_unit
  use config_type_module, only: ConfigType
  use config_error, only : error_data, fatal_error
  use system_config_module, only: SystemConfig, SlicedSystemConfig
  use config_toml, only: read_config_file
  use model_config_module, only: ModelConfig
  use mean_config_module, only: MeanConfig
  use constant_mean_config_module, only: ConstantMeanConfig
  use zero_mean_config_module, only: ZeroMeanConfig
  use linear_mean_config_module, only: LinearMeanConfig
  use quadratic_mean_config_module, only: QuadraticMeanConfig
  use optimiser_config_module, only: OptimiserConfig
  use particle_swarm_config_module, only: ParticleSwarmConfig
  use pso_stopping_criteria_config_module, only: ParticleSwarmStoppingCriteriaConfig, &
                                                 RelativeChangeStoppingCriteriaConfig, &
                                                 NoStoppingCriteriaConfig
  use pso_parameter_updater_config_module, only: ParticleSwarmParameterUpdaterConfig, &
                                                 ConstantParameterUpdaterConfig, &
                                                 UnifiedAdaptiveParameterConfig, &
                                                 IndividualAdaptiveParameterConfig, &
                                                 AdaptiveParameterUpdaterConfig, &
                                                 RandomUnifiedParameterUpdaterConfig, &
                                                 RandomIndividualParameterUpdaterConfig
  use pso_swarm_updater_config_module, only: ParticleSwarmSwarmUpdaterConfig, &
                                             GlobalParticleSwarmConfig, &
                                             ForcedParticleSwarmConfig
  use kernel_config_module, only: KernelConfig, RBFConfig, RBFCyclicConfig, KernelConfigList, &
                                  PeriodicConfig, LinearConfig
  use notes_module, only: Note
  use tomlf
  use tomlf_type, only : len
  use utils, only: sanitise

  implicit none
  private
  public :: Config, error_data, SlicedConfig
  public :: MeanConfig, ConstantMeanConfig, ZeroMeanConfig, LinearMeanConfig, QuadraticMeanConfig
  public :: OptimiserConfig
  public :: RBFConfig, KernelConfig, RBFCyclicConfig, PeriodicConfig, LinearConfig
  public :: ParticleSwarmConfig, &
            RelativeChangeStoppingCriteriaConfig, &
            NoStoppingCriteriaConfig, &
            ConstantParameterUpdaterConfig, &
            UnifiedAdaptiveParameterConfig, &
            IndividualAdaptiveParameterConfig, &
            AdaptiveParameterUpdaterConfig, &
            RandomUnifiedParameterUpdaterConfig, &
            RandomIndividualParameterUpdaterConfig, &
            GlobalParticleSwarmConfig, &
            ForcedParticleSwarmConfig, &
            ParticleSwarmSwarmUpdaterConfig, &
            ParticleSwarmParameterUpdaterConfig, &
            ParticleSwarmStoppingCriteriaConfig

  type, extends(ConfigType) :: Config
    type(SystemConfig), allocatable, dimension(:) :: systems
    type(ModelConfig), allocatable :: model
    type(KernelConfigList), allocatable :: kernels
    type(Note), dimension(:), allocatable :: notes
  contains
    private
    procedure, public, pass(self) :: info
    procedure, public, pass(self) :: init => init_config
  end type Config

  type, extends(Config) :: SlicedConfig
    type(SlicedSystemConfig), allocatable :: system
  contains
    procedure, public, pass(self) :: slice => init_sliced_config
  end type SlicedConfig

contains

  subroutine init_config(self, file, error)
    class(Config), intent(inout) :: self
    character(len=*), intent(in) :: file
    type(error_data), allocatable, intent(out) :: error

    type(toml_table), allocatable :: table
    type(toml_table), pointer :: child, node
    type(toml_array), pointer :: children
    type(toml_table), pointer :: kernel, kernel_list
    type(toml_array), pointer :: kernels
    type(toml_key), allocatable :: list(:), keys(:)
    class(KernelConfig), allocatable :: kernel_config
    character(len=:), allocatable :: kernel_type, value
    integer :: stat, isys, ikern, nkern, i

    call read_config_file(table, file, error)
    if (allocated(error)) return

    if (.not.allocated(table)) then
      call fatal_error(error, "Unclassified error while reading: '"//file//"'")
      return
    end if

    call get_value(table, "system", child, requested=.false.)
    if (associated(child)) then
      allocate(self%systems(1))
      call self%systems(1)%init(child, error)
    else
      call get_value(table, "system", children, requested=.false.)
      if (.not.associated(children)) then
          call fatal_error(error, "Requires at least one system table")
          return
      end if
      if (.not.is_array_of_tables(children)) then
          call fatal_error(error, "Systems must be provided as table or array of tables")
          return
      end if
      
      allocate(self%systems(len(children)))
      do isys = 1, len(children)
        call get_value(children, isys, node, stat=stat)
        if (stat /= toml_stat%success) then
          call fatal_error(error, "Could not retrieve systems from array entry")
          exit
        end if
        call self%systems(isys)%init(node, error)
        if (allocated(error)) exit
      end do
    end if
    if (allocated(error)) return

    call get_value(table, "model", child, requested=.false.)
    if (associated(child)) then
      allocate(self%model)
      call self%model%init(child, table, error)
    end if

    call get_value(table, "kernels", kernel_list, requested=.false.)

    if (.not.associated(kernel_list)) then
      call fatal_error(error, "Requires at least one kernel table")
      return
    end if

    if (allocated(list)) deallocate(list)
    call kernel_list%get_keys(list)
    allocate(self%kernels)
    call self%kernels%init(size(list))
    do ikern = 1, size(list)
      call get_value(kernel_list, list(ikern)%key, kernel, stat=stat)
      if (stat /= toml_stat%success) then
        call fatal_error(error, "Could not retrieve kernels from array entry")
        exit
      end if
      call get_value(kernel, "type", kernel_type, "rbf")
      kernel_type = sanitise(kernel_type, str_replace="-")

      select case (kernel_type)
        case ("rbf")
          allocate(RBFConfig :: kernel_config)
        case ("rbf-cyclic")
          allocate(RBFCyclicConfig :: kernel_config)
        case ("periodic")
          allocate(PeriodicConfig :: kernel_config)
        case ("linear")
          allocate(LinearConfig :: kernel_config)
      end select
      call kernel_config%init(kernel, error)

      call self%kernels%add(kernel_config)
      if (allocated(kernel_config)) deallocate(kernel_config)
      if (allocated(error)) exit
    end do

    call get_value(table, "notes", child, requested=.false.)
    if (associated(child)) then
      call child%get_keys(keys)

      allocate(self%notes(size(keys)))

      do i = 1, size(keys)
        call get_value(child, keys(i), value)
        call self%notes(i)%init(keys(i)%key, value)
      end do
    end if

    if (allocated(error)) return
  end subroutine init_config

  subroutine init_sliced_config(self, from, isys, iatom, iprop)
    class(SlicedConfig), intent(inout) :: self
    type(Config), intent(in) :: from
    integer, intent(in) :: isys
    integer, intent(in) :: iatom
    integer, intent(in) :: iprop

    allocate(self%model, source=from%model)
    allocate(self%kernels, source=from%kernels)
    allocate(self%systems, source=from%systems)
    allocate(self%system)
    call self%system%slice(self%systems(isys), iatom, iprop)
    if (allocated(from%notes)) allocate(self%notes, source=from%notes)
  end subroutine init_sliced_config

  subroutine info(self, unit)
    class(Config), intent(in) :: self
    integer, intent(in) :: unit

    integer :: ikern, isys, i

    write(unit, '(a, i2, a)') "Systems (n=", size(self%systems), ")"
    do isys = 1, size(self%systems)
        call self%systems(isys)%info(unit)
    end do
    call self%model%info(unit)
    call self%kernels%info(unit)

    if (allocated(self%notes)) then
      write(unit, '(a)') "Notes"
      do i = 1, size(self%notes)
        call self%notes(i)%info(unit)
      end do
    end if
  end subroutine info
end module config_module
