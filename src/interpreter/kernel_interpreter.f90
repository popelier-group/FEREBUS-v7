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
