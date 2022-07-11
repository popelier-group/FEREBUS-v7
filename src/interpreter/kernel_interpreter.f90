module kernel_interpreter_module
  use parser_module, only: Parser
  use ast_module, only: ASTNode
  use kernels, only: Kernel
  use kernel_config_module, only: KernelConfigList

  implicit none
  private
  public :: KernelInterpreter

  type :: KernelInterpreter
    type(Parser), allocatable :: psr
    type(KernelConfigList), allocatable :: global_state
  contains
    private
    procedure, public, pass(self) :: interpret
  end type KernelInterpreter

contains

  function interpret(self, text, global_state, verbose) result(k)
    class(KernelInterpreter), intent(inout) :: self
    character(len=*), intent(in) :: text
    type(KernelConfigList), intent(in) :: global_state
    logical, optional, intent(in) :: verbose

    class(Kernel), allocatable :: k
    class(ASTNode), allocatable :: tree
    logical :: verbose_

    if (.not.present(verbose)) then
      verbose_ = .false.
    else
      verbose_ = verbose
    end if

    allocate(self%psr)
    call self%psr%init(text)
    allocate(self%global_state, source=global_state)
    self%global_state = global_state

    call self%psr%parse(tree)
    if (verbose_) then
      print*, "Kernel AST: ", tree%display()
    end if
    allocate(k, source=tree%visit(self%global_state))
  end function interpret

end module kernel_interpreter_module
