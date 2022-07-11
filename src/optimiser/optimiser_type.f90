module optimiser_type
  use kinds, only: wp
  use constants, only: INFINITY, OUTPUT_UNIT
  use optimiser_iter_state, only: IterState
  use optimiser_iter_result, only: IterResult
  use config_module, only: OptimiserConfig
  use gpr_module, only: GaussianProcessRegressor
  implicit none

  type, abstract :: Optimiser
    type(GaussianProcessRegressor), allocatable :: op !allocatable :: op
    logical :: require_gradient = .false.
    logical :: require_hessian = .false.
    type(IterState), allocatable :: state
  contains
    private
    ! procedure(init_interface), public, deferred, pass(self) :: init
    procedure, public, pass(self) :: init_iter_state
    procedure(next_iter_interface), public, deferred, pass(self) :: next_iter
    procedure, public, pass(self) :: update => update_state
    procedure, public, pass(self) :: run
    procedure, public, pass(self) :: optimise => optimise_gpr
  end type Optimiser

  abstract interface
    ! subroutine init_interface(self, op, cfg)
    !   import Optimiser
    !   import GaussianProcessRegressor
    !   import OptimiserConfig
    !   class(Optimiser), intent(inout) :: self
    !   type(GaussianProcessRegressor), target, intent(in) :: op
    !   class(OptimiserConfig), intent(in) :: cfg
    ! end subroutine init_interface

    function next_iter_interface(self) result(result)
      import Optimiser
      import IterResult
      class(Optimiser), intent(inout) :: self
      type(IterResult), allocatable :: result
    end function next_iter_interface
  end interface

contains

  subroutine init_iter_state(self, max_iter)
    class(Optimiser), intent(inout) :: self
    integer, intent(in) :: max_iter
    real(kind=wp), dimension(:), allocatable :: params
    integer :: nparams

    nparams = self%op%nparams()
    allocate(params(nparams))
    params = self%op%get_params()
    if (allocated(self%state)) deallocate(self%state)
    allocate(self%state)
    allocate(self%state%param(nparams))
    allocate(self%state%prev_param(nparams))
    allocate(self%state%best_param(nparams))
    allocate(self%state%prev_best_param(nparams))
    if (self%require_gradient) allocate(self%state%gradient(nparams))
    if (self%require_gradient) allocate(self%state%prev_gradient(nparams))
    if (self%require_hessian) allocate(self%state%hessian(nparams,nparams))
    if (self%require_hessian) allocate(self%state%prev_hessian(nparams,nparams))

    self%state%param = params
    self%state%prev_param = params
    self%state%best_param = params
    self%state%prev_best_param = params
    self%state%cost = INFINITY()
    self%state%prev_cost = INFINITY()
    self%state%best_cost = INFINITY()

    if (self%require_gradient) then
      self%state%gradient = 0.0_wp
      self%state%prev_gradient = 0.0_wp
    end if

    if (self%require_hessian) then
      self%state%hessian = 0.0_wp
      self%state%prev_hessian = 0.0_wp
    end if

    self%state%iter = 1
    self%state%max_iter = max_iter
    self%state%terminated = .false.

    deallocate(params)
  end subroutine init_iter_state

  subroutine update_state(self, iter_result)
    class(Optimiser), intent(inout) :: self
    type(IterResult), intent(in) :: iter_result
    call self%state%update(iter_result)
  end subroutine update_state

  subroutine run(self)
    class(Optimiser), intent(inout) :: self
    type(IterResult), allocatable :: iter_result
    
    do while (.not.self%state%terminated)
      ! write(unit=OUTPUT_UNIT, id=id, fmt=*, asynchronous="YES"), "Iter:", self%state%iter, "Best Cost:", self%state%best_cost, "Best Param:", self%state%best_param
      print*, "Iter:", self%state%iter, "Best Cost:", self%state%best_cost, "Best Param:", self%state%best_param
      iter_result = self%next_iter()
      call self%update(iter_result)
      if (self%state%iter .ge. self%state%max_iter) then
        self%state%terminated = .true.
      end if
      call self%state%increment()
      !wait(unit=OUTPUT_UNIT, id=id)
    end do
  end subroutine run

  subroutine optimise_gpr(self)
    class(Optimiser), intent(inout) :: self
    ! class(OptimiserConfig), pointer :: opt_config
    print*, "running pso"
    call self%run()
    print*, "Best Cost:", self%state%best_cost
    print*, "Best Params:", self%state%best_param
    call self%op%set_params(self%state%best_param)
  end subroutine optimise_gpr
end module optimiser_type
