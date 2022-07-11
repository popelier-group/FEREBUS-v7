module kernels_m52
    use kinds, only: wp
    use constants, only: SQRT_5, DIV_5_3
    use kernels_stationary, only: StationaryKernel
    use kernels_module, only: Kernel
    implicit none
    private
    public :: Matern52, init_m52
  
    type, extends(StationaryKernel) :: Matern52
      real(kind=wp), dimension(:), allocatable :: lengthscale
    contains
      private
      procedure :: copy_m52
      procedure, public, pass(self) :: k => k_m52
      procedure, public, pass(self) :: g => g_m52
      procedure, public, pass(self) :: R => R_m52
      procedure, public, pass(self) :: get_params => get_params_m52
      procedure, public, pass(self) :: set_params => set_params_m52
      procedure, public, pass(self) :: nparams => nparams_m52
      procedure, public, pass(self) :: write => write_m52
      generic, public :: assignment(=) => copy_m52
    end type Matern52
  
  contains
  
    function init_m52(ndim, lengthscale) result(m52_kernel)
      integer, optional, intent(in) :: ndim
      real(kind=wp), dimension(:), optional, intent(in) :: lengthscale
      class(Matern52), allocatable :: m52_kernel
  
      allocate(rbf_kernel)
      if (present(ndim)) then
        m52_kernel%ndim = ndim
      else
        if (present(lengthscale)) then
            m52_kernel%ndim = size(lengthscale)
        else
          print*, "Error: Lengthscale and ndim Missing in Matern52 initialisation"
          stop
        end if
      end if
  
      if (present(lengthscale)) then
        allocate(m52_kernel%lengthscale, source=lengthscale)
        m52_kernel%lengthscale = lengthscale
      else
        allocate(m52_kernel%lengthscale(m52_kernel%ndim))
        m52_kernel%lengthscale = 1.0_wp
      end if
  
      ! rbf_kernel%number_of_kernels = rbf_kernel%number_of_kernels + 1
      call m52_kernel%set_kernel_name()
    end function init_m52
  
    function k_m52(self, xi, xj) result(k)
      class(Matern52), intent(in) :: self
      real(kind=wp), dimension(:), intent(in) :: xi, xj
      real(kind=wp) :: k
  
      real(kind=wp), dimension(:), allocatable :: diff
      real(kind=wp), dimension(:), allocatable :: diffl
  
      if (.not.allocated(diff)) allocate(diff(self%ndim))
      if (.not.allocated(diffl)) allocate(diffl(self%ndim))
      diff = xi - xj
      diffl = diff/self%lengthscale
      !> TODO
      k = product((1.0_wp + SQRT_5*diffl + DIV_5_3*diffl*diffl) * exp(-))
    end function k_m52
  
    function g_m52(self, xi, xj) result(g)
      class(Matern52), intent(in) :: self
      real(kind=wp), dimension(:), intent(in) :: xi, xj
      real(kind=wp), dimension(:), allocatable :: g
  
      allocate(g(self%ndim))
      g = 0.0_wp
    end function g_m52
  
    ! function R_m52(self, x) result(R)
    !   class(Matern52), intent(inout) :: self
    !   real(kind=wp), dimension(:, :), intent(in) :: x
    !   real(kind=wp), dimension(:, :), allocatable :: R
    !   integer :: ntrain, i, j
  
    !   ntrain = size(x, 1)
  
    !   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !   !> BIG PROBLEM                                                              <!
    !   !> There is an issue when using multiple composite kernels of the same type <!
    !   !> e.g. k = k1*k2*k3                                                        <!
    !   !> NEEDS FIXING ASAP                                                        <!
    !   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !   if (.not.allocated(self%distance_matrix)) then
    !     call self%compute_distance_matrix(x)
    !   end if
  
    !   ! print*, self%distance_matrix
  
    !   if (.not.allocated(R)) allocate(R(ntrain, ntrain))
    !   do i = 1, ntrain
    !     R(i, i) = 1.0_wp
    !     do j = i + 1, ntrain !> TODO
    !       R(i, j) = exp(-sum((1.0_wp/(2.0_wp*self%lengthscale*self%lengthscale)) * self%distance_matrix(i, j, :)))
    !       R(j, i) = R(i, j)
    !     end do
    !   end do
    ! end function R_m52
  
    function get_params_m52(self) result(params)
      class(Matern52), intent(in) :: self
      real(kind=wp), dimension(:), allocatable :: params
  
      allocate(params, source=self%lengthscale)
      params = self%lengthscale
    end function get_params_m52
  
    subroutine set_params_m52(self, params)
      class(Matern52), intent(inout) :: self
      real(kind=wp), dimension(:), intent(in) :: params
  
      if (.not.allocated(self%lengthscale)) allocate(self%lengthscale, source=params)
      self%lengthscale = params
    end subroutine set_params_m52
  
    function nparams_m52(self) result(nparams)
      class(Matern52), intent(in) :: self
      integer :: nparams
      nparams = size(self%lengthscale)
    end function nparams_m52
  
    subroutine write_m52(self, unit)
      class(Matern52), intent(in) :: self
      integer, intent(in) :: unit
      integer :: i, nparams
      real(kind=wp), dimension(:), allocatable :: params
  
      nparams = self%nparams()
      allocate(params(nparams))
      params = self%get_params()
      !> DLPOLY uses theta values in RBF not lengthscale
      params = 1/(2.0_wp*params*params)
  
      write(unit, "(A, A, A)") "[kernel.", self%kernel_name, "]"
      write(unit, "(A, 1x, A)") "type", "m52"
      write(unit, "(A, 1x, I0)") "number_of_dimensions", nparams
      write(unit, "(A, 1x, A)") "active_dimensions", "<TODO>"
      write(unit, "(A, 1x, (*(G0, 1x)))") "lengthscale", (params(i), i=1,nparams)
  
      deallocate(params)
    end subroutine write_m52
  
    subroutine destructor(self)
      type(Matern52), intent(inout) :: self
  
      ! self%number_of_kernels = self%number_of_kernels - 1
      if (allocated(self%lengthscale)) deallocate(self%lengthscale)
    end subroutine destructor
  
    subroutine copy_m52(self, from)
      class(Matern52), intent(inout) :: self
      type(Matern52), intent(in) :: from
  
      ! if (.not.allocated(self)) allocate(self, source=from)
      if (.not.allocated(self%lengthscale)) allocate(self%lengthscale, source=from%lengthscale)
      self%lengthscale = from%lengthscale
    end subroutine copy_m52
  end module kernels_m52
