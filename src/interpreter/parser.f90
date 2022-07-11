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

module parser_module
  use lexer_module, only: Lexer
  use token_module, only: Token,        &
                          TokenType,    &
                          NumberToken
  
  use ast_module, only: ASTNode,     &
                        BinOpNode,   &
                        UnaryOpNode, &
                        NumNode,     &
                        VarNode,     &
                        HeadNode
  
  implicit none

  type :: Parser
    type(Lexer), allocatable :: lxr
    class(Token), allocatable :: current_token
  contains
    private
    procedure, public, pass(self) :: init => init_parser
    procedure, public, pass(self) :: error
    procedure, public, pass(self) :: eat
    procedure, public, pass(self) :: number
    procedure, public, pass(self) :: variable
    procedure, public, pass(self) :: factor
    procedure, public, pass(self) :: term
    procedure, public, pass(self) :: expr
    procedure, public, pass(self) :: parse
  end type Parser

contains

  subroutine init_parser(self, text)
    class(Parser), intent(inout) :: self
    character(len=*), intent(in) :: text

    allocate(self%lxr)
    call self%lxr%init(text)
    self%current_token = self%lxr%get_next_token()
  end subroutine init_parser

  subroutine error(self)
    class(Parser), intent(in) :: self
    
    !> TODO: Convert this to fatal error
    print*, "Invalid Syntax"
    stop
  end subroutine error

  subroutine eat(self, token_type)
    class(Parser), intent(inout) :: self
    integer, intent(in) :: token_type

    if (self%current_token%type .eq. token_type) then
      self%current_token = self%lxr%get_next_token()
    else
      call self%error()
    end if
  end subroutine eat

  function variable(self) result(node)
    class(Parser), intent(inout) :: self
    type(VarNode), allocatable :: node

    allocate(node)
    call node%init(self%current_token)
    call self%eat(TokenType%Id)
  end function variable

  function number(self, tkn) result(node)
    class(Parser), intent(inout) :: self
    class(Token), intent(in) :: tkn
    type(NumNode), allocatable :: node

    allocate(node)
    call node%init(tkn)
  end function number

  recursive subroutine factor(self, node)
    class(Parser), intent(inout) :: self
    class(ASTNode), allocatable, intent(inout) :: node
    class(ASTNode), allocatable :: fctr
    class(Token), allocatable :: tkn

    tkn = self%current_token
    if (tkn%type .eq. TokenType%Plus) then
      call self%eat(TokenType%Plus)
      allocate(UnaryOpNode :: node)
      select type (node)
        type is (UnaryOpNode)
          call self%factor(fctr)
          call node%init(tkn, fctr)
      end select
      return
    else if (tkn%type .eq. TokenType%Minus) then
      call self%eat(TokenType%Minus)
      allocate(UnaryOpNode :: node)
      select type (node)
        type is (UnaryOpNode)
          call self%factor(fctr)
          call node%init(tkn, fctr)
      end select
      return
    else if (tkn%type .eq. TokenType%Number) then
      call self%eat(TokenType%Number)
      node = self%number(tkn)
      return
    else if (tkn%type .eq. TokenType%LParen) then
      call self%eat(TokenType%LParen)
      call self%expr(node)
      call self%eat(TokenType%RParen)
      return
    else
      node = self%variable()
      return
    end if
  end subroutine factor

  recursive subroutine term(self, node)
    class(Parser), intent(inout) :: self
    class(ASTNode), allocatable, intent(inout) :: node
    class(ASTNode), allocatable :: new_node
    class(ASTNode), allocatable :: trm
    class(Token), allocatable :: tkn

    call self%factor(node)
    do while (any(self%current_token%type .eq. (/TokenType%Mul, TokenType%Div/)))
      tkn = self%current_token
      if (tkn%type .eq. TokenType%Mul) then
        call self%eat(TokenType%Mul)
      else if (tkn%type .eq. TokenType%Div) then
        call self%eat(TokenType%Div)
      end if
      allocate(BinOpNode :: new_node)
      select type (new_node)
        type is (BinOpNode)
          call self%term(trm)
          call new_node%init(node, tkn, trm)
      end select
      deallocate(node)
      allocate(node, source=new_node)
      node = new_node
      deallocate(new_node)
    end do
  end subroutine term

  recursive subroutine expr(self, node)
    class(Parser), intent(inout) :: self
    class(ASTNode), allocatable, intent(out) :: node
    class(ASTNode), allocatable :: new_node
    class(ASTNode), allocatable :: trm
    class(Token), allocatable :: tkn

    call self%term(node)
    do while (any(self%current_token%type .eq. (/TokenType%Plus, TokenType%Minus/)))
      tkn = self%current_token
      if (tkn%type .eq. TokenType%Plus) then
        call self%eat(TokenType%Plus)
      else if (tkn%type .eq. TokenType%Minus) then
        call self%eat(TokenType%Minus)
      end if

      allocate(BinOpNode :: new_node)
      select type (new_node)
        type is (BinOpNode)
          call self%term(trm)
          call new_node%init(node, tkn, trm)
      end select
      deallocate(node)
      allocate(node, source=new_node)
      node = new_node
      deallocate(new_node)
    end do
  end subroutine expr

  subroutine parse(self, expr)
    class(Parser), intent(inout) :: self
    class(ASTNode), allocatable, intent(out) :: expr

    call self%expr(expr)
  end subroutine parse

end module parser_module
