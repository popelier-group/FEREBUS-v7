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

module periodic_kernel_module
    use kinds, only: wp
    use stationary_kernel_module, only: StationaryKernel
    use kernel_config_module, only: PeriodicConfig, KernelConfig
    use distance_matrix_module, only: periodic_distance_matrix_required, periodic_distance_matrix
    use constants, only: PI, TWOPI
    implicit none
    private
    public :: PeriodicKernel
  
    type, extends(StationaryKernel) :: PeriodicKernel
    contains
      private
      procedure, public, pass(self) :: init => init_per
      procedure, public, pass(self) :: k => k_per
      procedure, public, pass(self) :: g => g_per
      procedure, public, pass(self) :: R => R_per
      procedure, public, pass(self) :: k_diff => k_diff_per
      procedure, public, pass(self) :: get_params => get_params_per
      procedure, public, pass(self) :: set_params => set_params_per
      procedure, public, pass(self) :: nparams => nparams_per
      procedure, public, pass(self) :: write => write_per
    end type PeriodicKernel
  
  contains
  
    subroutine init_per(self, cfg)
      class(PeriodicKernel), intent(inout) :: self
      class(KernelConfig), intent(in) :: cfg
  
      select type (cfg)
        type is (PeriodicConfig)
          self%ndim = cfg%ndim
          if (allocated(cfg%lengthscale)) then
            allocate(self%lengthscale, source=cfg%lengthscale)
          else
            allocate(self%lengthscale(self%ndim))
            self%lengthscale = 1.0_wp
          end if
          
          allocate(self%kernel_name, source=cfg%name)
  
          if (allocated(cfg%active_dims)) then
            allocate(self%active_dims, source=cfg%active_dims)
          end if
      end select
  
      periodic_distance_matrix_required = .true.
    end subroutine init_per
  
    function k_per(self, xi, xj) result(k)
      class(PeriodicKernel), intent(in) :: self
      real(kind=wp), dimension(:), intent(in) :: xi, xj
      real(kind=wp) :: k
  
      real(kind=wp), dimension(:), allocatable :: diff
  
      if (.not.allocated(diff)) allocate(diff(self%ndim))
      diff = xi(self%active_dims) - xj(self%active_dims)
      k = self%k_diff(diff)
    end function k_per
  
    function k_diff_per(self, diff) result(k)
      class(PeriodicKernel), intent(in) :: self
      real(kind=wp), dimension(:), intent(in) :: diff
      real(kind=wp) :: k
  
      k = exp(-2.0_wp*sum(self%lengthscale*sin(diff)*sin(diff)))
    end function k_diff_per
  
    function g_per(self, xi, xj) result(g)
      class(PeriodicKernel), intent(in) :: self
      real(kind=wp), dimension(:), intent(in) :: xi, xj
      real(kind=wp), dimension(:), allocatable :: g
      real(kind=wp), dimension(:), allocatable :: diff
  
      print*, "Not Implemented Gradient for Periodic Kernel"
      stop
    end function g_per
  
    function get_params_per(self) result(params)
      class(PeriodicKernel), intent(in) :: self
      real(kind=wp), dimension(:), allocatable :: params
  
      allocate(params(self%nparams()))
      params = 1.0_wp/log(sqrt(self%lengthscale))
    end function get_params_per
  
    subroutine set_params_per(self, params)
      class(PeriodicKernel), intent(inout) :: self
      real(kind=wp), dimension(:), intent(in) :: params
  
      if (.not.allocated(self%lengthscale)) allocate(self%lengthscale, source=params)
      self%lengthscale = 1.0_wp/exp(params)
      self%lengthscale = self%lengthscale*self%lengthscale
    end subroutine set_params_per
  
    function nparams_per(self) result(nparams)
      class(PeriodicKernel), intent(in) :: self
      integer :: nparams
      nparams = size(self%lengthscale)
    end function nparams_per
  
    subroutine write_per(self, unit)
      class(PeriodicKernel), intent(in) :: self
      integer, intent(in) :: unit
      integer :: i, nparams
      real(kind=wp), dimension(:), allocatable :: params
      character(len=50) :: fmt, ifmt
  
      nparams = self%nparams()
      allocate(params, source=self%lengthscale)
      !> DLPOLY uses theta values in RBF not lengthscale
      params = 0.5*params
  
      write(fmt,'(a,i0,a)') "(a,1x,(", nparams, "(g0,1x)))"
      write(ifmt,'(a,i0,a)') "(a,1x,(", nparams, "(i0,1x)))"
  
      write(unit, "(A, A, A)") "[kernel.", self%kernel_name, "]"
      write(unit, "(A, 1x, A)") "type", "periodic"
      write(unit, "(A, 1x, I0)") "number_of_dimensions", nparams
      write(unit, trim(ifmt)) "active_dimensions", self%active_dims
      write(unit, trim(fmt)) "theta", params
  
      deallocate(params)
    end subroutine write_per

#if defined(__PGI__)
  subroutine R_per(self, x, R)
    class(PeriodicKernel), target, intent(inout) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
    real(kind=wp), dimension(:,:), intent(inout) :: R
    real(kind=wp), dimension(:,:), allocatable :: tmp
    real(kind=wp), dimension(:), pointer :: lengthscale
    integer :: ntrain, nfeats, i, j, thread

    ntrain = size(x, 1)
    nfeats = size(x, 2)

    lengthscale => self%lengthscale

    if (.not.allocated(R)) allocate(R(ntrain, ntrain))

    thread = omp_get_thread_num() + 2

    !$acc data copy(R) copyin(lengthscale) async(thread)
    !$acc wait(1)
    !$acc parallel loop collapse(2) async(thread)
    do j = 1, ntrain
      do i = 1, ntrain
        R(i, j) = exp(-0.5_wp*sum(lengthscale * distance_matrix(:, i, j)))
      end do
    end do
    !$acc end parallel
    !$acc end data
    !$acc wait(thread)
  end subroutine R_per
#else
  subroutine R_per(self, x, R)
    class(PeriodicKernel), target, intent(inout) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
    real(kind=wp), dimension(:,:), intent(inout) :: R
    integer :: ntrain, nfeats, i, j
    real(kind=wp), dimension(:,:,:), allocatable :: dm
    real(kind=wp), dimension(:), pointer :: lengthscale

    ntrain = size(x,1)
    nfeats = size(x,2)

    lengthscale => self%lengthscale

    if (size(self%active_dims) .eq. nfeats) then
      call dgemv('t', nfeats, ntrain*ntrain, -2.0_wp, periodic_distance_matrix, nfeats, self%lengthscale, 1, 0.0_wp, R, 1)
      R = exp(R)
    else
      allocate(dm(size(self%active_dims), ntrain, ntrain))
      do i = 1, size(self%active_dims)
        dm(i,:,:) = periodic_distance_matrix(self%active_dims(i),:,:)
      end do
      call dgemv('t', size(self%active_dims), ntrain*ntrain, -2.0_wp, dm, size(self%active_dims), self%lengthscale, 1, 0.0_wp, R, 1)
      R = exp(R)
      deallocate(dm)
    end if
  end subroutine R_per
#endif
end module periodic_kernel_module
  