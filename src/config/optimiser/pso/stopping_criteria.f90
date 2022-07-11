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

module pso_stopping_criteria_config_module
  use kinds, only: wp
  use tomlf, only: toml_table, toml_stat
  use config_error, only: error_data
  use config_type_module, only: ConfigType
  use utils, only: get_value_converted
  implicit none
  private
  public :: ParticleSwarmStoppingCriteriaConfig, &
            NoStoppingCriteriaConfig, &
            RelativeChangeStoppingCriteriaConfig, &
            PotentialBasedCriteriaConfig, &
            SignTestCriteriaConfig

  type, abstract, extends(ConfigType) :: ParticleSwarmStoppingCriteriaConfig
  contains
    procedure(init_interface), public, deferred, pass(self) :: init
  end type ParticleSwarmStoppingCriteriaConfig

  abstract interface
    subroutine init_interface(self, table, error)
      import ParticleSwarmStoppingCriteriaConfig
      import toml_table
      import error_data
      class(ParticleSwarmStoppingCriteriaConfig), intent(inout) :: self
      type(toml_table), intent(inout) :: table
      type(error_data), allocatable, intent(out) :: error
    end subroutine init_interface
  end interface

  type, extends(ParticleSwarmStoppingCriteriaConfig) :: NoStoppingCriteriaConfig
  contains
    private
    procedure, public, pass(self) :: init => init_no_stopping_criteria_config
    procedure, public, pass(self) :: info => no_stopping_criteria_info
  end type NoStoppingCriteriaConfig

  type, extends(ParticleSwarmStoppingCriteriaConfig) :: RelativeChangeStoppingCriteriaConfig
    integer :: stall_iterations = 20
    real(kind=wp) :: tolerance = 1e-6
  contains
    private
    procedure, public, pass(self) :: init => init_relative_change_stopping_criteria_config
    procedure, public, pass(self) :: info => relative_change_stopping_criteria_info
  end type RelativeChangeStoppingCriteriaConfig

  type, extends(ParticleSwarmStoppingCriteriaConfig) :: PotentialBasedCriteriaConfig
    real(kind=wp) :: delta = 1e-7
  contains
    private
    procedure, public, pass(self) :: init => init_potential_based_stopping_criteria_config
    procedure, public, pass(self) :: info => potential_based_stopping_criteria_info
  end type PotentialBasedCriteriaConfig

  type, extends(ParticleSwarmStoppingCriteriaConfig) :: SignTestCriteriaConfig
    real(kind=wp) :: false_alarm = 1e-3
  contains
    private
    procedure, public, pass(self) :: init =>init_sign_test_stopping_criteria_config
    procedure, public, pass(self) :: info => sign_test_stopping_criteria_info
  end type SignTestCriteriaConfig

contains

  subroutine stopping_criteria_info(self, unit)
    class(ParticleSwarmStoppingCriteriaConfig), intent(in) :: self
    integer, intent(in) :: unit

    write(unit, '(A)') "Error: Unreachable code"
    stop
  end subroutine stopping_criteria_info

  subroutine no_stopping_criteria_info(self, unit)
    class(NoStoppingCriteriaConfig), intent(in) :: self
    integer, intent(in) :: unit

    write(unit, '(A, T30, A)') "Stopping Criteria", "None"
  end subroutine no_stopping_criteria_info

  subroutine init_no_stopping_criteria_config(self, table, error)
    class(NoStoppingCriteriaConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_data), allocatable, intent(out) :: error
  end subroutine init_no_stopping_criteria_config

  subroutine relative_change_stopping_criteria_info(self, unit)
    class(RelativeChangeStoppingCriteriaConfig), intent(in) :: self
    integer, intent(in) :: unit

    write(unit, '(" - ", A, T30, A)') "Stopping Criteria", "Relative Change"
    write(unit, '("    - ", A, T30, I0)') "Stall Iterations", self%stall_iterations
    write(unit, '("    - ", A, T30, G0)') "Tolerance", self%tolerance
  end subroutine relative_change_stopping_criteria_info

  subroutine init_relative_change_stopping_criteria_config(self, table, error)
    class(RelativeChangeStoppingCriteriaConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_data), allocatable, intent(out) :: error

    call get_value_converted(table, "stall_iterations", self%stall_iterations)
    call get_value_converted(table, "tolerance", self%tolerance)
  end subroutine init_relative_change_stopping_criteria_config

  subroutine potential_based_stopping_criteria_info(self, unit)
    class(PotentialBasedCriteriaConfig), intent(in) :: self
    integer, intent(in) :: unit

    write(unit, '(" - ", A, T30, A)') "Stopping Criteria", "Potential Based"
    write(unit, '("    - ", A, T30, G0)') "Delta", self%delta
  end subroutine potential_based_stopping_criteria_info

  subroutine init_potential_based_stopping_criteria_config(self, table, error)
    class(PotentialBasedCriteriaConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_data), allocatable, intent(out) :: error

    call get_value_converted(table, "delta", self%delta)
  end subroutine init_potential_based_stopping_criteria_config

  subroutine sign_test_stopping_criteria_info(self, unit)
    class(SignTestCriteriaConfig), intent(in) :: self
    integer, intent(in) :: unit

    write(unit, '(" - ", A, T30, A)') "Stopping Criteria", "Sign Test"
    write(unit, '("    - ", A, T30, G0)') "False Alarm", self%false_alarm
  end subroutine sign_test_stopping_criteria_info

  subroutine init_sign_test_stopping_criteria_config(self, table, error)
    class(SignTestCriteriaConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_data), allocatable, intent(out) :: error

    call get_value_converted(table, "false_alarm", self%false_alarm)
  end subroutine init_sign_test_stopping_criteria_config
end module pso_stopping_criteria_config_module