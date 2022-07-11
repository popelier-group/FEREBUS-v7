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

module constant_kernel_module
  use kinds, only: wp
  use kernels_module, only: Kernel
  use config_module, only: KernelConfig
  implicit none
  private
  public :: Constant

  type, extends(Kernel) :: Constant
    real(kind=wp) :: value
  contains
    private
    procedure, public, pass(self) :: init => init_constant
    procedure, public, pass(self) :: k => k_constant
    procedure, public, pass(self) :: g => g_constant
    procedure, public, pass(self) :: get_params => get_params_constant
    procedure, public, pass(self) :: set_params => set_params_constant
    procedure, public, pass(self) :: nparams => nparams_constant
    procedure, public, pass(self) :: write => write_constant
    procedure, public, pass(self) :: cleanup => cleanup_constant
  end type Constant

contains

  subroutine init_constant(self, cfg)
    class(Constant), intent(inout) :: self
    class(KernelConfig), intent(in) :: cfg

    self%ndim = 1
    self%value = 1.0_wp
    self%kernel_name = "c"
  end subroutine init_constant

  subroutine cleanup_constant(self)
    class(Constant), intent(inout) :: self

    !$acc exit data delete(self) async(1)
  end subroutine cleanup_constant

  function k_constant(self, xi, xj) result(k)
    class(Constant), intent(in) :: self
    real(kind=wp), dimension(:), intent(in) :: xi, xj
    real(kind=wp) :: k

    k = self%value
  end function k_constant

  function g_constant(self, xi, xj) result(g)
    class(Constant), intent(in) :: self
    real(kind=wp), dimension(:), intent(in) :: xi, xj
    real(kind=wp), dimension(:), allocatable :: g
    real(kind=wp), dimension(:), allocatable :: diff

    g = (/1.0_wp/)
  end function g_constant

  function get_params_constant(self) result(params)
    class(Constant), intent(in) :: self
    real(kind=wp), dimension(:), allocatable :: params

    params = (/self%value/)
  end function get_params_constant

  subroutine set_params_constant(self, params)
    class(Constant), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: params
    self%value = params(1)
  end subroutine set_params_constant

  function nparams_constant(self) result(nparams)
    class(Constant), intent(in) :: self
    integer :: nparams
    nparams = 1
  end function nparams_constant

  subroutine write_constant(self, unit)
    class(Constant), intent(in) :: self
    integer, intent(in) :: unit
    integer :: i, nparams

    nparams = self%nparams()
    write(unit, "(A, A, A)") "[kernel.", self%kernel_name, "]"
    write(unit, "(A, 1x, A)") "type", "constant"
    write(unit, "(A, 1x, I0)") "number_of_dimensions", nparams
    write(unit, "(A, 1x, A)") "active_dimensions", "<TODO>"
    write(unit, "(A, 1x, G0)") "value", self%value
  end subroutine write_constant
end module constant_kernel_module
  