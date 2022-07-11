module ast_module
  use kinds, only: wp
  use token_module, only: Token,        &
                          TokenType,    &
                          NumberToken, &
                          PlusToken,    &
                          MinusToken,   &
                          MulToken,     &
                          DivToken,     &
                          LParenToken,  &
                          RParenToken,  &
                          IdToken,      &
                          EofToken
  use kernels, only: Kernel,       &
                      RBF,         &
                      RBFCyclic,   &
                      Constant,    &
                      PeriodicKernel, &
                      add_kernels, &
                      mult_kernels
  use kernel_config_module, only: KernelConfig,    &
                                  RBFConfig,       &
                                  RBFCyclicConfig, &
                                  PeriodicConfig, &
                                  KernelConfigList

  implicit none
  private
  public :: ASTNode,     &
            BinOpNode,   &
            UnaryOpNode, &
            NumNode,     &
            VarNode,     &
            HeadNode

  type, abstract :: ASTNode
  contains
    private
    procedure(visit_interface), public, deferred, pass(self) :: visit
    procedure(display_interface), public, deferred, pass(self) :: display
  end type ASTNode

  abstract interface
    function visit_interface(self, global_state) result(k)
      import ASTNode
      import KernelConfigList
      import Kernel
      class(ASTNode), intent(in) :: self
      type(KernelConfigList), intent(in) :: global_state
      class(Kernel), allocatable :: k
    end function visit_interface

    function display_interface(self) result(str)
      import ASTNode
      class(ASTNode), intent(in) :: self
      character(len=:), allocatable :: str
    end function display_interface
  end interface

  type, extends(ASTNode) :: HeadNode
    class(ASTNode), allocatable :: node
  contains
    private
    procedure, public, pass(self) :: init => init_head_node
    procedure, public, pass(self) :: visit => visit_head_node
    procedure, public, pass(self) :: display => display_head_node
  end type HeadNode

  type, extends(ASTNode) :: BinOpNode
    class(ASTNode), allocatable :: left
    class(Token), allocatable :: op
    class(ASTNode), allocatable :: right
  contains
    private
    procedure, public, pass(self) :: init => init_binop
    procedure, public, pass(self) :: visit => visit_binop
    procedure, public, pass(self) :: display => display_binop
  end type BinOpNode

  type, extends(ASTNode) :: UnaryOpNode
    class(Token), allocatable :: op
    class(ASTNode), allocatable :: expr
  contains
    private
    procedure, public, pass(self) :: init => init_unaryop
    procedure, public, pass(self) :: visit => visit_unaryop
    procedure, public, pass(self) :: display => display_unaryop
  end type UnaryOpNode

  type, extends(ASTNode) :: VarNode
    class(Token), allocatable :: var
  contains
    private
    procedure, public, pass(self) :: init => init_var
    procedure, public, pass(self) :: visit => visit_varop
    procedure, public, pass(self) :: display => display_varop
  end type VarNode

  type, extends(ASTNode) :: NumNode
    class(Token), allocatable :: num
  contains
    private
    procedure, public, pass(self) :: init => init_num
    procedure, public, pass(self) :: visit => visit_numop
    procedure, public, pass(self) :: display => display_numop
  end type NumNode

contains

  subroutine init_head_node(self, node)
    class(HeadNode), intent(inout) :: self
    class(ASTNode), intent(in) :: node

    allocate(self%node, mold=node)
    self%node = node
  end subroutine init_head_node

  function visit_head_node(self, global_state) result(k)
    class(HeadNode), intent(in) :: self
    type(KernelConfigList), intent(in) :: global_state
    class(Kernel), allocatable :: k

    select type (n => self%node)
      type is (BinOpNode)
        k = n%visit(global_state)
      type is (UnaryOpNode)
        k = n%visit(global_state)
      type is (NumNode)
        k = n%visit(global_state)
      type is (VarNode)
        k = n%visit(global_state)
    end select
  end function visit_head_node

  subroutine init_binop(self, left, op, right)
    class(BinOpNode), intent(inout) :: self
    class(ASTNode), intent(in) :: left
    class(Token), intent(in) :: op
    class(ASTNode), intent(in) :: right

    allocate(self%left, source=left)
    self%left = left
    allocate(self%op, source=op)
    self%op = op
    allocate(self%right, source=right)
    self%right = right
  end subroutine init_binop

  function visit_binop(self, global_state) result(k)
    class(BinOpNode), intent(in) :: self
    type(KernelConfigList), intent(in) :: global_state
    class(kernel), allocatable :: k

    select type (n => self%op)
      type is (PlusToken)
        k = add_kernels(self%left%visit(global_state),self%right%visit(global_state))
      type is (MinusToken)
        !> TODO: Implement Minus Kernel
        print*, "Error: Kernel minus not yet implemented"
        stop
      type is (MulToken)
        k = mult_kernels(self%left%visit(global_state),self%right%visit(global_state))
      type is (DivToken)
        !> TODO: Implement Divide Kernel
        print*, "Error: Kernel divide not yet implemented"
        stop
    end select
  end function visit_binop

  subroutine init_unaryop(self, op, expr)
    class(UnaryOpNode), intent(inout) :: self
    class(Token), intent(in) :: op
    class(ASTNode), intent(in) :: expr

    allocate(self%op, source=op)
    self%op = op
    allocate(self%expr, source=expr)
    self%expr = expr
  end subroutine init_unaryop

  function visit_unaryop(self, global_state) result(k)
    class(UnaryOpNode), intent(in) :: self
    type(KernelConfigList), intent(in) :: global_state
    class(kernel), allocatable :: k

    select type (t => self%op)
      type is (PlusToken)
        k = self%expr%visit(global_state)
      type is (MinusToken)
        !> TODO: Implement Minus Kernel
      print*, "Error: Kernel minus not yet implemented"
      stop
    end select
  end function visit_unaryop

  subroutine init_var(self, var)
    class(VarNode), intent(inout) :: self
    class(Token), intent(in) :: var

    allocate(self%var, source=var)
    self%var = var
  end subroutine init_var

  function visit_varop(self, global_state) result(k)
    class(VarNode), intent(in) :: self
    type(KernelConfigList), intent(in) :: global_state
    class(kernel), allocatable :: k

    select type(t => self%var)
      type is  (IdToken)
        ! select type (kc => global_state%get_name(t%value))
        !   type is (RBFConfig)
        !     k = RBF(kc)
        !   type is (RBFCyclicConfig)
        !     k = RBFCyclic(kc)
        ! end select
        select type (kc => global_state%get_name(t%value))
          type is (RBFConfig)
            allocate(RBF :: k)
          type is (RBFCyclicConfig)
            allocate(RBFCyclic :: k)
          type is (PeriodicConfig)
            allocate(PeriodicKernel :: k)
        end select
        call k%init(global_state%get_name(t%value))
    end select
  end function visit_varop

  subroutine init_num(self, num)
    class(NumNode), intent(inout) :: self
    class(Token), intent(in) :: num

    allocate(self%num, source=num)
    self%num = num
  end subroutine init_num

  function visit_numop(self, global_state) result(k)
    class(NumNode), intent(in) :: self
    type(KernelConfigList), intent(in) :: global_state
    class(Kernel), allocatable :: k

    select type(t => self%num)
      type is  (NumberToken)
        allocate(Constant :: k)
        k%kernel_name = "c"
        select type (k)
          type is (Constant)
            k%value = t%value
        end select
    end select
  end function visit_numop

  function display_head_node(self) result(str)
    class(HeadNode), intent(in) :: self
    character(len=:), allocatable :: str

    str = self%node%display()
  end function display_head_node

  function display_binop(self) result(str)
    class(BinOpNode), intent(in) :: self
    character(len=:), allocatable :: str
    character :: op_str

    if (self%op%type .eq. TokenType%Plus) then
      op_str = '+'
    else if (self%op%type .eq. TokenType%Minus) then
      op_str = '-'
    else if (self%op%type .eq. TokenType%Mul) then
      op_str = '*'
    else if (self%op%type .eq. TokenType%Div) then
      op_str = '/'
    end if

    str = "BinOp(" // self%left%display() // op_str // self%right%display() // ")"
  end function display_binop

  function display_unaryop(self) result(str)
    class(UnaryOpNode), intent(in) :: self
    character(len=:), allocatable :: str
    character :: op_str

    if (self%op%type .eq. TokenType%Plus) then
      op_str = '+'
    else if (self%op%type .eq. TokenType%Minus) then
      op_str = '-'
    end if

    str = "UnaryOp(" // op_str // self%expr%display() // ")"
  end function display_unaryop

  function display_varop(self) result(str)
    class(VarNode), intent(in) :: self
    character(len=:), allocatable :: str
    character(len=:), allocatable :: var_str

    select type(t => self%var)
      type is  (IdToken)
        var_str= t%value
    end select

    str = "Var(" // var_str  // ")"
  end function display_varop

  function display_numop(self) result(str)
    class(NumNode), intent(in) :: self
    character(len=:), allocatable :: str
    character(len=:), allocatable :: num_str
    character(len=128) :: tmp

    select type(t => self%num)
      type is  (NumberToken)
        write(tmp, *) t%value
    end select
    num_str = trim(tmp)

    str = "Num(" // num_str  // ")"
  end function display_numop
end module ast_module
