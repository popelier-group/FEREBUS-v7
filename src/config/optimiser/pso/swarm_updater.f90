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

module pso_swarm_updater_config_module
  use kinds, only: wp
  use tomlf, only: toml_table, toml_stat
  use config_error, only: error_data
  use config_type_module, only: ConfigType
  use utils, only: get_value_converted
  implicit none
  private
  public :: ParticleSwarmSwarmUpdaterConfig, GlobalParticleSwarmConfig, ForcedParticleSwarmConfig

  type, abstract, extends(ConfigType) :: ParticleSwarmSwarmUpdaterConfig
  contains
    private
    procedure(init_interface), public, deferred, pass(self) :: init
  end type ParticleSwarmSwarmUpdaterConfig

  abstract interface
    subroutine init_interface(self, table, error)
      import ParticleSwarmSwarmUpdaterConfig
      import toml_table
      import error_data
      class(ParticleSwarmSwarmUpdaterConfig), intent(inout) :: self
      type(toml_table), intent(inout) :: table
      type(error_data), allocatable, intent(out) :: error
    end subroutine init_interface
  end interface

  type, extends(ParticleSwarmSwarmUpdaterConfig) :: GlobalParticleSwarmConfig
  contains
    private
    procedure, public, pass(self) :: init => init_global_particle_swarm_config
    procedure, public, pass(self) :: info => global_particle_swarm_info
  end type GlobalParticleSwarmConfig

  type, extends(ParticleSwarmSwarmUpdaterConfig) :: ForcedParticleSwarmConfig
    real(kind=wp) :: delta = 1e-7_wp
  contains
    private
    procedure, public, pass(self) :: init => init_forced_particle_swarm_config
    procedure, public, pass(self) :: info => forced_particle_swarm_info
  end type ForcedParticleSwarmConfig

contains

  subroutine global_particle_swarm_info(self, unit)
    class(GlobalParticleSwarmConfig), intent(in) :: self
    integer, intent(in) :: unit

    write(unit, '(" - ", A, T30, A)') "Variant", "Global"
  end subroutine global_particle_swarm_info

  subroutine init_global_particle_swarm_config(self, table, error)
    class(GlobalParticleSwarmConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_data), allocatable, intent(out) :: error
  end subroutine init_global_particle_swarm_config

  subroutine forced_particle_swarm_info(self, unit)
    class(ForcedParticleSwarmConfig), intent(in) :: self
    integer, intent(in) :: unit

    write(unit, '(" - ", A, T30, A)') "Variant", "Forced"
    write(unit, '("    - ", A, T40, G0)') "Delta", self%delta
  end subroutine forced_particle_swarm_info

  subroutine init_forced_particle_swarm_config(self, table, error)
    class(ForcedParticleSwarmConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_data), allocatable, intent(out) :: error

    call get_value_converted(table, "delta", self%delta)
  end subroutine init_forced_particle_swarm_config
end module pso_swarm_updater_config_module