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

module linear_kernel_module
    use kinds, only: wp
    use kernels_module, only: Kernel
    use kernel_config_module, only: LinearConfig, KernelConfig
    implicit none
    private
    public :: LinearKernel
  
    type, extends(Kernel) :: LinearKernel
        real(kind=wp), allocatable, dimension(:) :: c
        real(kind=wp) :: order = 0.0_wp
    contains
      private
      procedure, public, pass(self) :: init => init_lin
      procedure, public, pass(self) :: k => k_lin
      procedure, public, pass(self) :: g => g_lin
      procedure, public, pass(self) :: get_params => get_params_lin
      procedure, public, pass(self) :: set_params => set_params_lin
      procedure, public, pass(self) :: nparams => nparams_lin
      procedure, public, pass(self) :: write => write_lin
      procedure, public, pass(self) :: cleanup => cleanup_lin
    end type LinearKernel
  
  contains
  
    subroutine init_lin(self, cfg)
      class(LinearKernel), intent(inout) :: self
      class(KernelConfig), intent(in) :: cfg
  
      select type (cfg)
        type is (LinearConfig)
          self%ndim = cfg%ndim
          if (allocated(cfg%c)) then
            allocate(self%c, source=cfg%c)
          else
            allocate(self%c(self%ndim))
            self%c = 1.0_wp
          end if
          
          allocate(self%kernel_name, source=cfg%name)
  
          if (allocated(cfg%active_dims)) then
            allocate(self%active_dims, source=cfg%active_dims)
          end if
      end select  
    end subroutine init_lin
  
    function k_lin(self, xi, xj) result(k)
      class(LinearKernel), intent(in) :: self
      real(kind=wp), dimension(:), intent(in) :: xi, xj
      real(kind=wp) :: k

      k = sum(((xi(self%active_dims)*xj(self%active_dims))**self%order)/self%c)
    end function k_lin

    function g_lin(self, xi, xj) result(g)
      class(LinearKernel), intent(in) :: self
      real(kind=wp), dimension(:), intent(in) :: xi, xj
      real(kind=wp), dimension(:), allocatable :: g
  
      allocate(g(self%ndim))
      g = 0.0_wp
    end function g_lin
  
    function get_params_lin(self) result(params)
      class(LinearKernel), intent(in) :: self
      real(kind=wp), dimension(:), allocatable :: params
  
      allocate(params, source=-log(self%c))
    end function get_params_lin
  
    subroutine set_params_lin(self, params)
      class(LinearKernel), intent(inout) :: self
      real(kind=wp), dimension(:), intent(in) :: params
  
      if (.not.allocated(self%c)) allocate(self%c(size(params)))
      self%c = exp(-params)
    end subroutine set_params_lin
  
    function nparams_lin(self) result(nparams)
      class(LinearKernel), intent(in) :: self
      integer :: nparams
      nparams = size(self%c)
    end function nparams_lin
  
    subroutine write_lin(self, unit)
      class(LinearKernel), intent(in) :: self
      integer, intent(in) :: unit
      integer :: i, nparams
      real(kind=wp), dimension(:), allocatable :: params
      character(len=50) :: fmt, ifmt
  
      nparams = self%nparams()
  
      write(fmt,'(a,i0,a)') "(a,1x,(", nparams, "(g0,1x)))"
      write(ifmt,'(a,i0,a)') "(a,1x,(", nparams, "(i0,1x)))"
  
      write(unit, "(A, A, A)") "[kernel.", self%kernel_name, "]"
      write(unit, "(A, 1x, A)") "type", "linear"
      write(unit, "(A, 1x, I0)") "number_of_dimensions", nparams
      write(unit, trim(ifmt)) "active_dimensions", self%active_dims
      write(unit, "(A, 1x, G0)") "order", self%order
      write(unit, trim(fmt)) "c", self%c
    end subroutine write_lin

    subroutine cleanup_lin(self)
      class(LinearKernel), intent(inout) :: self
    end subroutine cleanup_lin

end module linear_kernel_module
  
