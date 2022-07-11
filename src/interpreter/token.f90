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

module token_module
  use kinds, only: wp
  use token_type_module, only: TokenType!,    &
                              !  IntegerToken, &
                              !  PlusToken,    &
                              !  MinusToken,   &
                              !  MulToken,     &
                              !  DivToken,     &
                              !  LParenToken,  &
                              !  RParenToken,  &
                              !  IdToken,      &
                              !  EofToken
  implicit none
  private
  public :: Token,       &
            TokenType,   &
            NumberToken, &
            PlusToken,   &
            MinusToken,  &
            MulToken,    &
            DivToken,    &
            LParenToken, &
            RParenToken, &
            IdToken,     &
            EofToken

  type :: Token
    integer :: type
  contains
    private
    procedure, public, pass(self) :: init_token_general
    procedure, public, pass(self) :: init_token_number
    procedure, public, pass(self) :: init_token_id
    generic, public :: init => init_token_general, init_token_number, init_token_id
  end type Token

  type, extends(Token) :: NumberToken
    real(kind=wp) :: value
  end type NumberToken

  type, extends(Token) :: PlusToken
  end type PlusToken

  type, extends(Token) :: MinusToken
  end type MinusToken

  type, extends(Token) :: MulToken
  end type MulToken

  type, extends(Token) :: DivToken
  end type DivToken

  type, extends(Token) :: LParenToken
  end type LParenToken

  type, extends(Token) :: RParenToken
  end type RParenToken

  type, extends(Token) :: IdToken
    character(len=:), allocatable :: value
  end type IdToken

  type, extends(Token) :: EofToken
  end type EofToken

  ! interface Token
  !   module procedure init_number_token
  !   module procedure init_id_token
  !   module procedure init_token
  ! end interface Token

contains

  subroutine error()
    !> TODO: Convert this to fatal error
    print*, "Token Type Does Not Match Known Value"
    stop
  end subroutine error

  subroutine check_type(t1, t2)
    integer, intent(in) :: t1
    integer, intent(in) :: t2

    if (t1 .ne. t2) call error()
  end subroutine check_type

  subroutine init_token_number(self, token_type, val)
    class(Token), intent(inout) :: self
    integer, intent(in) :: token_type
    real(kind=wp), intent(in) :: val

    call check_type(token_type, TokenType%Number)
    self%type = token_type
    select type (self)
      type is (NumberToken)
        self%value = val
    end select
  end subroutine init_token_number

  subroutine init_token_id(self, token_type, val)
    class(Token), intent(inout) :: self
    integer, intent(in) :: token_type
    character(len=*), intent(in) :: val

    call check_type(token_type, TokenType%Id)
    self%type = token_type
    select type (self)
      type is (IdToken)
        allocate(self%value, source=val)
        self%value = val
    end select
  end subroutine init_token_id

  subroutine init_token_general(self, token_type)
    class(Token), intent(inout) :: self
    integer, intent(in) :: token_type

    self%type = token_type
  end subroutine init_token_general
end module token_module