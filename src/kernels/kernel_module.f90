module kernels_module
  use kinds, only: wp
  use writable_module, only: Writable
  use utils, only: stack
  use constants, only: OUTPUT_UNIT
  use config_module, only: KernelConfig
  implicit none
  private
  public :: mult_kernels,add_kernels,Kernel, KernelSum, KernelProd, operator(.eq.), CompositeKernel!, copy_kernel

  integer :: kernel_counter = 0

  type, abstract, extends(Writable) :: Kernel
    character(len=:), allocatable :: kernel_name
    integer :: ndim
    integer, dimension(:), allocatable :: active_dims
    !> Variables for standardised cyclic feature correction
    real(kind=wp), dimension(:), allocatable :: cyclic_pi
    real(kind=wp), dimension(:), allocatable :: cyclic_minus_pi
    real(kind=wp), dimension(:), allocatable :: cyclic_twopi
  contains
    private
    procedure(init_interface), public, deferred, pass(self) :: init
    procedure :: add_kernels
    procedure :: mult_kernels
    procedure(k_interface), public, deferred, pass(self) :: k !> Covariance function
    procedure(g_interface), public, deferred, pass(self) :: g !> Gradient function
    procedure, public, pass(self) :: R => R_interface
    procedure(get_params_interface), public, deferred, pass(self) :: get_params
    procedure(set_params_interface), public, deferred, pass(self) :: set_params
    procedure(nparams_interface), public, deferred, pass(self) :: nparams
    procedure, public, pass(self) :: name => get_kernel_name
    procedure, public, pass(self) :: nkernels
    procedure, public, pass(self) :: set_kernel_name
    procedure, public, pass(self) :: init_cyclic_pi
    generic, public :: operator(+) => add_kernels
    generic, public :: operator(*) => mult_kernels
    procedure(cleanup_interface), public, deferred, pass(self) :: cleanup
  end type Kernel

  abstract interface
    subroutine init_interface(self, cfg)
      import Kernel
      import KernelConfig
      class(Kernel), intent(inout) :: self
      class(KernelConfig), intent(in) :: cfg
    end subroutine init_interface

    function k_interface(self, xi, xj) result(k)
      import Kernel
      import wp
      class(Kernel), intent(in) :: self
      real(kind=wp), dimension(:), intent(in) :: xi, xj
      real(kind=wp) :: k
    end function k_interface

    function g_interface(self, xi, xj) result(g)
      import Kernel
      import wp
      class(Kernel), intent(in) :: self
      real(kind=wp), dimension(:), intent(in) :: xi, xj
      real(kind=wp), dimension(:), allocatable :: g
    end function g_interface

    function get_params_interface(self) result(params)
      import Kernel
      import wp
      class(Kernel), intent(in) :: self
      real(kind=wp), dimension(:), allocatable :: params
    end function get_params_interface

    subroutine set_params_interface(self, params)
      import Kernel
      import wp
      class(Kernel), intent(inout) :: self
      real(kind=wp), dimension(:), intent(in) :: params
    end subroutine set_params_interface

    function nparams_interface(self) result(nparams)
      import Kernel
      class(Kernel), intent(in) :: self
      integer :: nparams
    end function nparams_interface

    subroutine cleanup_interface(self)
      import Kernel
      class(Kernel), intent(inout) :: self
    end subroutine cleanup_interface
  end interface

  type, abstract, extends(Kernel) :: CompositeKernel
    class(Kernel), allocatable :: k1
    class(Kernel), allocatable :: k2
  contains
    private
    procedure, public, pass(self) :: init => comp_init
    procedure, pass(self) :: init_composite
    procedure, public, pass(self) :: get_params => get_params_composite
    procedure, public, pass(self) :: set_params => set_params_composite
    procedure, public, pass(self) :: nparams => nparams_composite
    procedure, public, pass(self) :: nkernels => n_kernels_composite
    procedure, public, pass(self) :: init_cyclic_pi => init_cyclic_pi_composite
    procedure, public, pass(self) :: write => write_composite
    ! procedure, public, pass(self) :: check_associated
    procedure, public, pass(self) :: cleanup => cleanup_composite
  end type CompositeKernel

  type, extends(CompositeKernel) :: KernelSum
  contains
    private
    ! procedure, pass(self) :: copy_sum
    procedure, public, pass(self) :: k => k_sum
    procedure, public, pass(self) :: g => g_sum
    procedure, public, pass(self) :: R => R_sum
    procedure, public, pass(self) :: name => get_kernel_name_sum
    ! generic :: assignment(=) => copy_sum
  end type KernelSum

  type, extends(CompositeKernel) :: KernelProd
  contains
    private
    ! procedure, pass(self) :: copy_prod
    procedure, public, pass(self) :: k => k_prod
    procedure, public, pass(self) :: g => g_prod
    procedure, public, pass(self) :: R => R_prod
    procedure, public, pass(self) :: name => get_kernel_name_prod
    ! generic :: assignment(=) => copy_prod
  end type KernelProd

  type, extends(Kernel) :: ScaledKernel
    real(kind=wp) :: scale
    class(Kernel), allocatable :: k1
  contains
    private
    procedure, public, pass(self) :: init => scaled_init
    ! procedure, pass(self) :: copy_scaled
    procedure, public, pass(self) :: k => k_scaled
    procedure, public, pass(self) :: g => g_scaled
    procedure, public, pass(self) :: R => R_scaled
    procedure, public, pass(self) :: get_params => get_params_scaled
    procedure, public, pass(self) :: set_params => set_params_scaled
    procedure, public, pass(self) :: nparams => nparams_scaled
    procedure, public, pass(self) :: write => write_scaled
    procedure, public, pass(self) :: nkernels => n_kernels_scaled
    procedure, public, pass(self) :: name => get_kernel_name_scaled
    procedure, public, pass(self) :: init_cyclic_pi => init_cyclic_pi_scaled
    ! generic :: assignment(=) => copy_scaled
    procedure, public, pass(self) :: cleanup => cleanup_scaled
  end type ScaledKernel

  interface operator (.eq.)
    module procedure eq_kernels
  end interface

contains

  function nkernels(self) result(n)
    class(Kernel), intent(in) :: self
    integer :: n
    n = 1
  end function nkernels

  function get_kernel_name(self) result(kernel_name)
    class(Kernel), intent(inout) :: self
    character(len=:), allocatable :: kernel_name

    if (.not.allocated(self%kernel_name)) then
      call self%set_kernel_name()
    end if

    allocate(kernel_name, source=self%kernel_name)
    kernel_name = self%kernel_name
  end function get_kernel_name

  subroutine set_kernel_name(self)
    class(Kernel), intent(inout) :: self
    integer :: n_characters

    kernel_counter = kernel_counter + 1

    n_characters = floor(log10(real(kernel_counter, kind=wp))) + 1 + 1
    allocate(character(len=n_characters) :: self%kernel_name)
    write(self%kernel_name, "(A, I0)") "k", kernel_counter
  end subroutine set_kernel_name

  subroutine R_interface(self, x, R)
    class(Kernel), target, intent(inout) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
    real(kind=wp), dimension(:,:), intent(inout) :: R
    integer :: i, j

    do i = 1, size(x, 1)
      do j = 1, size(x, 2)
        R(i, j) = self%k(x(i,:), x(j,:))
      end do
    end do
  end subroutine R_interface

  subroutine init_cyclic_pi(self, x_std)
    class(Kernel), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: x_std
    !> Override if cyclic pi required
  end subroutine init_cyclic_pi

  recursive function get_params_composite(self) result(params)
    class(CompositeKernel), intent(in) :: self
    real(kind=wp), dimension(:), allocatable :: params

    allocate(params(self%k1%nparams() + self%k2%nparams()))
    params = stack(self%k1%get_params(), self%k2%get_params())
  end function get_params_composite

  recursive subroutine set_params_composite(self, params)
    class(CompositeKernel), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: params
    integer :: n1, n2

    n1 = self%k1%nparams()
    n2 = self%k2%nparams()

    call self%k1%set_params(params(1:n1))
    call self%k2%set_params(params(n1+1:n1+n2))
  end subroutine set_params_composite

  recursive subroutine cleanup_composite(self)
    class(CompositeKernel), intent(inout) :: self

    call self%k1%cleanup()
    call self%k1%cleanup()
  end subroutine cleanup_composite

  subroutine comp_init(self, cfg)
    class(CompositeKernel), intent(inout) :: self
    class(KernelConfig), intent(in) :: cfg
  end subroutine comp_init

  subroutine scaled_init(self, cfg)
    class(ScaledKernel), intent(inout) :: self
    class(KernelConfig), intent(in) :: cfg
  end subroutine scaled_init

  recursive function nparams_composite(self) result(nparams)
    class(CompositeKernel), intent(in) :: self
    integer :: nparams
    nparams = self%k1%nparams() + self%k2%nparams()
  end function nparams_composite

  recursive function n_kernels_composite(self) result(n)
    class(CompositeKernel), intent(in) :: self
    integer :: n
    n = self%k1%nkernels() + self%k2%nkernels()
  end function n_kernels_composite

  recursive subroutine init_cyclic_pi_composite(self, x_std)
    class(CompositeKernel), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: x_std

    call self%k1%init_cyclic_pi(x_std)
    call self%k2%init_cyclic_pi(x_std)
  end subroutine init_cyclic_pi_composite

  recursive subroutine write_composite(self, unit)
    class(CompositeKernel), intent(in) :: self
    integer, intent(in) :: unit

    call self%k1%write(unit)
    write(unit, "(A)")
    call self%k2%write(unit)
  end subroutine write_composite

  recursive function k_sum(self, xi, xj) result(k)
    class(KernelSum), intent(in) :: self
    real(kind=wp), dimension(:), intent(in) :: xi, xj
    real(kind=wp) :: k

    k = self%k1%k(xi, xj) + self%k2%k(xi, xj)
  end function k_sum

  recursive function g_sum(self, xi, xj) result(g)
    class(KernelSum), intent(in) :: self
    real(kind=wp), dimension(:), intent(in) :: xi, xj
    real(kind=wp), dimension(:), allocatable :: g

    g = stack(self%k1%g(xi, xj), self%k2%g(xi, xj))
  end function g_sum

  recursive subroutine R_sum(self, x, R)
    class(KernelSum), target, intent(inout) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
    real(kind=wp), dimension(:,:), intent(inout) :: R
    real(kind=wp), dimension(:, :), allocatable :: R1, R2

    !$omp parallel sections
    !$omp section
    allocate(R1(size(x, 1), size(x, 1)))
    call self%k1%R(x, R1)
    !$omp section 
    allocate(R2(size(x, 1), size(x, 1)))
    call self%k2%R(x, R2)
    !$omp end parallel sections

    R = R1 + R2
  end subroutine R_sum

  recursive function get_kernel_name_sum(self) result(kernel_name)
    class(KernelSum), intent(inout) :: self
    character(len=:), allocatable :: kernel_name
    character(len=:), allocatable :: k1_name, k2_name

    k1_name = self%k1%name()
    k2_name = self%k2%name()

    allocate(character(len=len(k1_name)+len(k2_name)+3) :: kernel_name)
    kernel_name = "(" // k1_name // "+" // k2_name // ")"
  end function get_kernel_name_sum

  recursive function k_prod(self, xi, xj) result(k)
    class(KernelProd), intent(in) :: self
    real(kind=wp), dimension(:), intent(in) :: xi, xj
    real(kind=wp) :: k

    k = self%k1%k(xi, xj) * self%k2%k(xi, xj)
  end function k_prod

  recursive function g_prod(self, xi, xj) result(g)
    class(KernelProd), intent(in) :: self
    real(kind=wp), dimension(:), intent(in) :: xi, xj
    real(kind=wp), dimension(:), allocatable :: g
    real(kind=wp) :: k1, k2

    k1 = self%k1%k(xi, xj)
    k2 = self%k2%k(xi, xj)

    g = stack(k2*self%k1%g(xi, xj), k1*self%k2%g(xi, xj))
  end function g_prod

  recursive subroutine R_prod(self, x, R)
    class(KernelProd), target, intent(inout) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
    real(kind=wp), dimension(:,:), intent(inout) :: R
    real(kind=wp), dimension(:, :), allocatable :: R1, R2

    !$omp parallel sections
    !$omp section
    allocate(R1(size(x, 1), size(x, 1)))
    call self%k1%R(x, R1)
    !$omp section 
    allocate(R2(size(x, 1), size(x, 1)))
    call self%k2%R(x, R2)
    !$omp end parallel sections

    R = R1 * R2
  end subroutine R_prod

  recursive function get_kernel_name_prod(self) result(kernel_name)
    class(KernelProd), intent(inout) :: self
    character(len=:), allocatable :: kernel_name
    character(len=:), allocatable :: k1_name, k2_name

    k1_name = self%k1%name()
    k2_name = self%k2%name()

    allocate(character(len=len(k1_name)+len(k2_name)+3) :: kernel_name)
    kernel_name = "(" // k1_name // "*" // k2_name // ")"
  end function get_kernel_name_prod

  recursive function add_kernels(k1, k2) result(ksum)
    class(Kernel), intent(in) :: k1
    class(Kernel), intent(in) :: k2
    type(KernelSum), allocatable, target :: ksum

    allocate(ksum)
    call ksum%init_composite(k1, k2)
  end function add_kernels

  recursive function mult_kernels(k1, k2) result(kprod)
    class(Kernel), target, intent(in) :: k1
    class(Kernel), target, intent(in) :: k2
    type(KernelProd), allocatable :: kprod

    allocate(kprod)
    call kprod%init_composite(k1, k2)
  end function mult_kernels

  recursive subroutine init_composite(self, k1, k2)
    class(CompositeKernel), intent(inout) :: self
    class(Kernel), intent(in) :: k1
    class(Kernel), intent(in) :: k2

    allocate(self%k1, source=k1)
    allocate(self%k2, source=k2)
  end subroutine init_composite

  recursive function init_scaled_kernel(scale, k1) result(k)
    real(kind=wp), intent(in) :: scale
    class(Kernel), intent(in) :: k1
    type(ScaledKernel), allocatable :: k

    allocate(k)
    k%scale = scale
    allocate(k%k1, source=k1)
    k%k1 = k1
  end function init_scaled_kernel

  recursive function k_scaled(self, xi, xj) result(k)
    class(ScaledKernel), intent(in) :: self
    real(kind=wp), dimension(:), intent(in) :: xi, xj
    real(kind=wp) :: k

    k = self%scale*self%scale * self%k1%k(xi, xj)
  end function k_scaled

  recursive function g_scaled(self, xi, xj) result(g)
    class(ScaledKernel), intent(in) :: self
    real(kind=wp), dimension(:), intent(in) :: xi, xj
    real(kind=wp), dimension(:), allocatable :: g
    real(kind=wp), dimension(:), allocatable :: k1g

    k1g = self%k1%g(xi, xj)
    allocate(g(size(k1g)+1))
    g(1) = 2*self%scale
    g(2:size(k1g)+1) = k1g
  end function g_scaled

  recursive subroutine R_scaled(self, x, R)
    class(ScaledKernel), target, intent(inout) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
    real(kind=wp), dimension(:,:), intent(inout) :: R

    call self%k1%R(x, R)
    R = self%scale*self%scale * R
  end subroutine R_scaled

  recursive function get_params_scaled(self) result(params)
    class(ScaledKernel), intent(in) :: self
    real(kind=wp), dimension(:), allocatable :: params

    params = stack(self%k1%get_params(), (/self%scale/))
  end function get_params_scaled

  recursive subroutine set_params_scaled(self, params)
    class(ScaledKernel), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: params
    integer :: n1

    n1 = self%k1%nparams()

    call self%k1%set_params(params(1:n1))
    self%scale = params(n1+1)
  end subroutine set_params_scaled

  recursive function nparams_scaled(self) result(nparams)
    class(ScaledKernel), intent(in) :: self
    integer :: nparams

    nparams = self%k1%nparams() + 1
  end function nparams_scaled

  recursive subroutine write_scaled(self, unit)
    class(ScaledKernel), intent(in) :: self
    integer, intent(in) :: unit

    write(unit, "(A, A, A)") "[kernel.", self%kernel_name, "]"
    write(unit, "(A, 1x, A)") "type", "scaled"
    write(unit, "(A, 1x, I0)") "number_of_dimensions", self%nparams()
    write(unit, "(A, 1x, A)") "active_dimensions", "<TODO>"
    write(unit, "(A, 1x, (*(G0, 1x)))") "scale", self%scale
    write(unit, "(A)")
    call self%k1%write(unit)
  end subroutine write_scaled

  recursive subroutine init_cyclic_pi_scaled(self, x_std)
    class(ScaledKernel), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: x_std

    call self%k1%init_cyclic_pi(x_std)
  end subroutine init_cyclic_pi_scaled

  recursive function get_kernel_name_scaled(self) result(kernel_name)
    class(ScaledKernel), intent(inout) :: self
    character(len=:), allocatable :: kernel_name
    character(len=:), allocatable :: self_name, k1_name

    self_name = self%kernel_name
    k1_name = self%k1%name()

    allocate(character(len=len(self_name)+len(k1_name)+4) :: kernel_name)
    kernel_name = "(" // self_name // "(" // k1_name // "))"
  end function get_kernel_name_scaled

  recursive function n_kernels_scaled(self) result(n)
    class(ScaledKernel), intent(in) :: self
    integer :: n
    n = self%k1%nkernels() + 1
  end function n_kernels_scaled

  recursive subroutine cleanup_scaled(self)
    class(ScaledKernel), intent(inout) :: self

    call self%k1%cleanup()
  end subroutine cleanup_scaled

  function eq_kernels(k1, k2) result(equal)
    class(Kernel), intent(in) :: k1
    class(Kernel), intent(in) :: k2
    logical :: equal

    real(kind=wp), dimension(:), allocatable :: params1, params2
    integer :: i

    params1 = k1%get_params()
    params2 = k2%get_params()

    equal = same_type_as(k1, k2)
    if (equal) then
      equal = size(params1) .eq. size(params2)
      if (equal) then
        do i = 1, size(params1)
          equal = params1(i) .eq. params2(i)
          if (.not.equal) exit
        end do
      end if
    end if
  end function eq_kernels

end module kernels_module
