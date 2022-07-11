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

module gpr_module
  use kinds, only: wp
  use writable_module, only: Writable
  use data_set_module, only: DataSet, StandardisedDataSet
  use kernels, only: Kernel, operator(.eq.)
  use kernel_interpreter_module, only: KernelInterpreter
  use mean_module, only: MeanType, ConstantMean, ZeroMean, LinearMean, QuadraticMean
  use utils, only: identity, ones, print_matrix
  use constants, only: LOG_TWOPI, OUTPUT_UNIT
  use config_module, only: SlicedConfig,                         &
                           MeanConfig,                           &
                           ConstantMeanConfig,                   &
                           ZeroMeanConfig,                       &
                           LinearMeanConfig,                     &
                           QuadraticMeanConfig,                  &
                           OptimiserConfig,                      &
                           ParticleSwarmConfig,                  &
                           RelativeChangeStoppingCriteriaConfig
  use config_module, only: RBFConfig, RBFCyclicConfig
  use kernels, only: RBF, RBFCyclic
  use likelihood_module, only: Likelihood, ConcentratedLikelihood, MarginalLikelihood
  use optimiser_iter_state, only: IterState
  use optimiser_iter_result, only: IterResult
  use distance_matrix_module, only: cyclic_distance_matrix

  implicit none
  private
  public :: GaussianProcessRegressor

  type, abstract :: Optimisable
  contains
    private
    procedure(cost_function_interface), public, deferred, pass(self) :: cost_function
    procedure(set_params_interface), public, deferred, pass(self) :: set_params
    procedure(get_params_interface), public, deferred, pass(self) :: get_params
    procedure(nparams_interface), public, deferred, pass(self) :: nparams
    ! procedure(optimise_interface), public, deferred, pass(self) :: optimise
  end type Optimisable

  abstract interface
    subroutine set_params_interface(self, params)
      import Optimisable
      import wp
      class(Optimisable), intent(inout) :: self
      real(kind=wp), dimension(:), intent(in) :: params
    end subroutine set_params_interface

    function get_params_interface(self) result(params)
      import Optimisable
      import wp
      class(Optimisable), intent(in) :: self
      real(kind=wp), dimension(:), allocatable :: params
    end function get_params_interface

    function nparams_interface(self) result(nparams)
      import Optimisable
      class(Optimisable), intent(in) :: self
      integer :: nparams
    end function nparams_interface

    function cost_function_interface(self, param, gradient, hessian) result(result)
      import Optimisable
      import IterResult
      import wp
      class(Optimisable), intent(inout) :: self
      real(kind=wp), dimension(:), intent(in) :: param
      logical, optional, intent(in) :: gradient, hessian
      type(IterResult), allocatable :: result
    end function cost_function_interface

    ! subroutine optimise_interface(self)
    !   import Optimisable
    !   class(Optimisable), intent(inout) :: self
    ! end subroutine optimise_interface
  end interface

  type, extends(Optimisable) :: GaussianProcessRegressor
    type(SlicedConfig), pointer :: config => null()
    class(MeanType), allocatable :: mean
    class(Kernel), allocatable :: k
    class(DataSet), pointer :: training_set => null()
    class(Likelihood), allocatable :: likelihood
    real(kind=wp) :: nugget
    logical :: optimise_nugget = .true.
    real(kind=wp), dimension(:), allocatable :: cached_mean
  contains
    private
    procedure, public, pass(self) :: init => init_gpr
    procedure, public, pass(self) :: log_likelihood
    procedure, public, pass(self) :: R => compute_R
    procedure, public, pass(self) :: cost_function => negative_log_likelihood
    procedure, public, pass(self) :: set_params => set_params_gpr
    procedure, public, pass(self) :: get_params => get_params_gpr
    procedure, public, pass(self) :: nparams => nparams_gpr
    procedure, public, pass(self) :: search_min => search_min_gpr
    procedure, public, pass(self) :: search_max => search_max_gpr
    procedure, public, pass(self) :: write => write_gpr
    procedure, public, pass(self) :: cleanup => cleanup_gpr
  end type GaussianProcessRegressor

contains
  subroutine init_gpr(self, config, training_set, verbose)
    class(GaussianProcessRegressor), intent(inout) :: self
    type(SlicedConfig), target, intent(in) :: config
    class(DataSet), target, intent(in) :: training_set
    logical, optional, intent(in) :: verbose

    type(KernelInterpreter), allocatable :: kint
    real(kind=wp), dimension(:), allocatable :: tmp
    integer :: i
    logical :: verbose_

    if (.not.present(verbose)) then
      verbose_ = .false.
    else
      verbose_ = verbose
    end if

    self%config => config
    allocate(kint)
    self%k = kint%interpret(self%config%model%kernel, self%config%kernels, verbose=verbose_)

    self%training_set => training_set
    select type (ts => self%training_set)
      type is (StandardisedDataSet)
        call self%k%init_cyclic_pi(ts%x_std)
      class default
        allocate(tmp(ts%x%nfeats))
        tmp = 1.0_wp
        call self%k%init_cyclic_pi(tmp)
    end select

    self%nugget = self%config%model%noise
    self%optimise_nugget = self%config%model%optimise_noise

    select type (mc => self%config%model%mean)
      type is (ConstantMeanConfig)
        allocate(ConstantMean :: self%mean)
      type is (ZeroMeanConfig)
        allocate(ZeroMean :: self%mean)
      type is (LinearMeanConfig)
        allocate(LinearMean :: self%mean)
      type is (QuadraticMeanConfig)
        allocate(QuadraticMean :: self%mean)
    end select
    call self%mean%init(self%training_set%x%data, self%training_set%y%data)

    allocate(self%cached_mean(self%training_set%x%ntrain))
    call self%mean%value(self%training_set%x%data, self%cached_mean)

    select case (self%config%model%likelihood)
      case ("marginal")
        allocate(MarginalLikelihood :: self%likelihood)
      case ("concentrated")
        allocate(ConcentratedLikelihood :: self%likelihood)
      case default
        print*, "Unknown likelihood function:", self%config%model%likelihood
        stop
    end select
    call self%likelihood%init(self%nparams(), self%training_set%x%ntrain)
  end subroutine init_gpr

  subroutine compute_R(self)
    class(GaussianProcessRegressor), intent(inout) :: self
    call self%k%R(self%training_set%x%data, self%likelihood%covariance_matrix)
    self%likelihood%covariance_matrix = self%likelihood%covariance_matrix + self%nugget*identity(self%training_set%x%ntrain)
  end subroutine compute_R

  subroutine set_params_gpr(self, params)
    class(GaussianProcessRegressor), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: params
    integer :: nkernel_params

    nkernel_params = self%k%nparams()
    call self%k%set_params(params(1:nkernel_params))

    if (self%optimise_nugget) then
      self%nugget = params(nkernel_params+1)
    end if
  end subroutine set_params_gpr

  function get_params_gpr(self) result(params)
    class(GaussianProcessRegressor), intent(in) :: self
    real(kind=wp), dimension(:), allocatable :: params
    real(kind=wp), dimension(:), allocatable :: kernel_params
    real(kind=wp), dimension(:), allocatable :: gp_params

    kernel_params = self%k%get_params()

    if (self%optimise_nugget.eqv..true.) then
      allocate(params(size(kernel_params)+1))
      params(size(kernel_params)+1) = self%nugget
    else
      allocate(params(size(kernel_params)))
    end if
    params(1:size(kernel_params)) = kernel_params
  end function get_params_gpr

  subroutine search_min_gpr(self, min_)
    class(GaussianProcessRegressor), intent(in) :: self
    real(kind=wp), dimension(:), intent(out) :: min_
    integer :: nkernel_params

    nkernel_params = self%k%nparams()

    if (self%optimise_nugget) then
      min_(nkernel_params+1) = 1e-12_wp
    end if
    min_(1:nkernel_params) = self%config%model%optimiser%search_min
  end subroutine search_min_gpr

  subroutine search_max_gpr(self, max_)
    class(GaussianProcessRegressor), intent(in) :: self
    real(kind=wp), dimension(:), allocatable :: max_
    integer :: nkernel_params

    nkernel_params = self%k%nparams()

    if (self%optimise_nugget) then
      max_(nkernel_params+1) = 1e-6_wp
    end if
    max_(1:nkernel_params) = self%config%model%optimiser%search_max
  end subroutine search_max_gpr


  function nparams_gpr(self) result(nparams)
    class(GaussianProcessRegressor), intent(in) :: self
    integer :: nparams
    nparams = self%k%nparams()

    if (self%optimise_nugget.eqv..true.) then
      nparams = nparams + 1
    end if
  end function nparams_gpr

  function negative_log_likelihood(self, param, gradient, hessian) result(result) !> Cost Function
    class(GaussianProcessRegressor), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: param
    logical, optional, intent(in) :: gradient, hessian
    type(IterResult), allocatable :: result
    real(kind=wp) :: nll
    ! real(kind=wp), dimension(:), allocatable :: current_params
    logical :: compute_gradient, compute_hessian
    integer :: info

    real(kind=wp) :: log_likelihood

    call self%set_params(param)

    if (.not.present(gradient)) then
      compute_gradient = .false.
    else
      compute_gradient = gradient
    end if

    if (.not.present(hessian)) then
      compute_hessian = .false.
    else
      compute_hessian = hessian
    end if

    log_likelihood = self%log_likelihood(info=info)
    nll = -log_likelihood

    allocate(result)
    call result%init(param, nll, info)
  end function negative_log_likelihood

  function log_likelihood(self, info) result(ll)
    class(GaussianProcessRegressor), intent(inout) :: self
    integer, optional, intent(out) :: info
    integer :: ntrain
    real(kind=wp) :: ll

    call self%R()


    ll = self%likelihood%ll(        &
          self%get_params(),        &
          self%cached_mean,         &
          self%training_set%x%data, &
          self%training_set%y%data, &
          info                      &
        )
  end function log_likelihood

  ! subroutine optimise_gpr(self)
  !   class(GaussianProcessRegressor), intent(inout) :: self
  !   type(ParticleSwarm), allocatable :: opt
  !   ! class(OptimiserConfig), pointer :: opt_config
    
  !   print*, "initialising pso"
  !   ! select type (opt_config => self%config%model%optimiser)
  !   !   type is (ParticleSwarmConfig)
  !   !     allocate(ParticleSwarm :: opt)
  !   ! end select
  !   allocate(opt)
  !   call opt%init(self, self%config%model%optimiser)

  !   print*, "running pso"
  !   call opt%run()
  !   print*, "Best Cost:", opt%state%best_cost
  !   print*, "Best Params:", opt%state%best_param
  !   call self%set_params(opt%state%best_param)
  ! end subroutine optimise_gpr

  subroutine write_gpr(self, system_name, atom_name)
    class(GaussianProcessRegressor), intent(inout) :: self
    character(len=*), intent(in) :: system_name
    character(len=*), intent(in) :: atom_name

    character(len=:), allocatable :: fname
    real(kind=wp) :: ll
    real(kind=wp), dimension(3) :: p
    integer :: i

    allocate(character(len=len(system_name)+len(atom_name)+len(self%training_set%y%label)+8) :: fname)
    fname = system_name // "_" // self%training_set%y%label // "_" // atom_name // ".model"

    call self%R()

    ll = self%likelihood%ll(        &
          self%get_params(),        &
          self%cached_mean,         &
          self%training_set%x%data, &
          self%training_set%y%data  &
        )

    open(unit=10, file=fname, status='replace')
    write(10, '(A)') "# [metadata]"
    write(10, '(A)') "# program ferebus"
    write(10, '(A)') "# version 7.2.0"
    write(10, '(A, 1x, G0)') "# nugget", self%nugget
    write(10, '(A, 1x, G0)') "# likelihood", ll
    if (allocated(self%config%notes)) then
      do i = 1, size(self%config%notes)
        write(10, '(A, 1x, A, 1x, A, 1x, A)') "#", self%config%notes(i)%key, "=", self%config%notes(i)%value
      end do
    end if
    write(10, '(A)') ""
    write(10, '(A)') "[system]"
    write(10, '(A, 1x, A)') "name", system_name
    write(10, '(A, 1x, A)') "atom", atom_name
    write(10, '(A, 1x, A)') "property", self%training_set%y%label
    call self%config%system%atom%alf%write(10)
    write(10, '(A)')
    write(10, '(A)') "[dimensions]"
    write(10, '(A, 1x, I0)') "number_of_atoms", self%config%system%natoms
    write(10, '(A, 1x, I0)') "number_of_features", self%training_set%x%nfeats
    write(10, '(A, 1x, I0)') "number_of_training_points", self%training_set%x%ntrain
    write(10, '(A)')
    select type (ll => self%likelihood)
      type is (ConcentratedLikelihood)
        write(10, '(A)') "[mean]"
        write(10, '(A, 1x, A)') "type", "constant"
        write(10, '(A, 1x, G0)') "value", ll%mean(1)
      class default
        call self%mean%write(10)
    end select
    write(10, '(A)')
    write(10, '(A)') "[kernels]"
    write(10, '(A, 1x, I0)') "number_of_kernels", self%k%nkernels()
    write(10, '(A, 1x, A)') "composition", self%k%name()
    write(10, '(A)')
    call self%k%write(10)
    write(10, '(A)')
    call self%training_set%write(10)
    write(10, '(A)')
    call self%likelihood%write(10)
    write(10, '(A)')
    close(10)

    deallocate(fname)

    call self%cleanup()
  end subroutine write_gpr

  subroutine cleanup_gpr(self)
    class(GaussianProcessRegressor), intent(inout) :: self

    call self%k%cleanup()
    !$acc wait
  end subroutine cleanup_gpr
end module gpr_module
  
