module rbf_kernel_module
  use kinds, only: wp
  use stationary_kernel_module, only: StationaryKernel
  use kernels_module, only: Kernel
  use kernel_config_module, only: RBFConfig, KernelConfig
  use utils, only: fmt_str
  use distance_matrix_module, only: distance_matrix_required
  implicit none
  private
  public :: RBF

  type, extends(StationaryKernel) :: RBF
  contains
    private
    procedure, public, pass(self) :: init => init_rbf
    procedure, public, pass(self) :: k => k_rbf
    procedure, public, pass(self) :: g => g_rbf
    procedure, public, pass(self) :: k_diff => k_diff_rbf !> TODO: Maybe turn this into interface so that only k needs to be called
    procedure, public, pass(self) :: get_params => get_params_rbf
    procedure, public, pass(self) :: set_params => set_params_rbf
    procedure, public, pass(self) :: nparams => nparams_rbf
    procedure, public, pass(self) :: write => write_rbf
  end type RBF

contains

  subroutine init_rbf(self, cfg)
    class(RBF), intent(inout) :: self
    class(KernelConfig), intent(in) :: cfg

    select type (cfg)
      type is (RBFConfig)
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

    distance_matrix_required = .true.
  end subroutine init_rbf

  function k_rbf(self, xi, xj) result(k)
    class(RBF), intent(in) :: self
    real(kind=wp), dimension(:), intent(in) :: xi, xj
    real(kind=wp) :: k

    real(kind=wp), dimension(:), allocatable :: diff

    if (.not.allocated(diff)) allocate(diff(self%ndim))
    diff = xi(self%active_dims) - xj(self%active_dims)
    k = self%k_diff(diff*diff)
  end function k_rbf

  function k_diff_rbf(self, diff) result(k)
    class(RBF), intent(in) :: self
    real(kind=wp), dimension(:), intent(in) :: diff
    real(kind=wp) :: k

    k = exp(-0.5_wp*self%r2(diff))
  end function k_diff_rbf

  function g_rbf(self, xi, xj) result(g)
    class(RBF), intent(in) :: self
    real(kind=wp), dimension(:), intent(in) :: xi, xj
    real(kind=wp), dimension(:), allocatable :: g
    real(kind=wp), dimension(:), allocatable :: diff

    diff = xi - xj
    diff = self%lengthscale*diff*diff

    g = diff * exp(-0.5*sum(diff))
  end function g_rbf

  function get_params_rbf(self) result(params)
    class(RBF), intent(in) :: self
    real(kind=wp), dimension(:), allocatable :: params

    allocate(params(self%nparams()))
    params = log(self%lengthscale)
  end function get_params_rbf

  subroutine set_params_rbf(self, params)
    class(RBF), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: params

    if (.not.allocated(self%lengthscale)) allocate(self%lengthscale, source=params)
    self%lengthscale = exp(params)
  end subroutine set_params_rbf

  function nparams_rbf(self) result(nparams)
    class(RBF), intent(in) :: self
    integer :: nparams
    nparams = size(self%lengthscale)
  end function nparams_rbf

  subroutine write_rbf(self, unit)
    class(RBF), intent(in) :: self
    integer, intent(in) :: unit
    integer :: i, nparams
    real(kind=wp), dimension(:), allocatable :: params
    character(len=50) :: fmt, ifmt

    nparams = self%nparams()
    allocate(params, source=self%lengthscale)
    !> DLPOLY uses theta values in RBF not lengthscale
    params = 0.5_wp*params

    write(fmt,'(a,i0,a)') "(a,1x,(", nparams, "(g0,1x)))"
    write(ifmt,'(a,i0,a)') "(a,1x,(", nparams, "(i0,1x)))"

    write(unit, "(A, A, A)") "[kernel.", self%kernel_name, "]"
    write(unit, "(A, 1x, A)") "type", "rbf"
    write(unit, "(A, 1x, I0)") "number_of_dimensions", nparams
    write(unit, trim(ifmt)) "active_dimensions", self%active_dims
    write(unit, trim(fmt)) "theta", params

    deallocate(params)
  end subroutine
end module rbf_kernel_module
