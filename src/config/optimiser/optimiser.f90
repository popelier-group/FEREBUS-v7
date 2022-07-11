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

module optimiser_config_module
  use kinds, only: wp
  use tomlf, only: toml_table, toml_stat, get_value
  use config_error, only: error_data
  use config_type_module, only: ConfigType
  use utils, only: get_value_converted
  use config_error, only: error_data
  implicit none
  private
  public :: OptimiserConfig

  type, abstract, extends(ConfigType) :: OptimiserConfig
    real(kind=wp) :: search_min = 0.0_wp
    logical :: search_min_set = .false.
    real(kind=wp) :: search_max = 1.0_wp
    logical :: search_max_set = .false.
  contains
    private
    procedure(init_interface), public, deferred, pass(self) :: init
    procedure, public, pass(self) :: initialise => init_optimiser
    procedure, public, pass(self) :: set_min_max
    procedure, public, pass(self) :: optimiser_info
  end type OptimiserConfig

  abstract interface
    subroutine init_interface(self, table, error)
      import OptimiserConfig
      import toml_table
      import error_data
      class(OptimiserConfig), intent(inout) :: self
      type(toml_table), intent(inout) :: table
      type(error_data), allocatable, intent(out) :: error
    end subroutine init_interface
  end interface

contains

  subroutine init_optimiser(self, table, error)
    class(OptimiserConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: table
    type(error_data), allocatable, intent(out) :: error
    type(toml_table), pointer :: optimiser_table

    call get_value(table, "optimiser", optimiser_table, requested=.false.)
    if (associated(optimiser_table)) then
      call self%set_min_max(optimiser_table)
    end if
  end subroutine init_optimiser

  subroutine set_min_max(self, optimiser_table)
    class(OptimiserConfig), intent(inout) :: self
    type(toml_table), intent(inout) :: optimiser_table
    integer :: stat

    if (.not.self%search_min_set.eqv..true.) then
      call get_value_converted(optimiser_table, "search_min", self%search_min, stat=stat)
      self%search_min_set = (stat .eq. toml_stat%success)
    end if

    if (.not.self%search_max_set.eqv..true.) then
      call get_value_converted(optimiser_table, "search_max", self%search_max, stat=stat)
      self%search_max_set = (stat .eq. toml_stat%success)
    end if
  end subroutine

  subroutine optimiser_info(self, unit)
    class(OptimiserConfig), intent(in) :: self
    integer, intent(in) :: unit

    if (self%search_min_set.eqv..true.) then    
      write(unit, '(" - ", A, T30, G0)') "Search Min", self%search_min
    end if
    if (self%search_max_set.eqv..true.) then    
      write(unit, '(" - ", A, T30, G0)') "Search Max", self%search_max
    end if
  end subroutine optimiser_info
end module optimiser_config_module
