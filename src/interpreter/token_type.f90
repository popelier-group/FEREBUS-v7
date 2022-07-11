module token_type_module
  implicit none
  private
  public :: TokenType

  type :: TokenTypeEnum
    integer :: Eof = -1
    integer :: Number = 1
    integer :: Plus = 2
    integer :: Minus = 3
    integer :: Mul = 4
    integer :: Div = 5
    integer :: LParen = 6
    integer :: RParen = 7
    integer :: Id = 8
  end type TokenTypeEnum

  type(TokenTypeEnum) :: TokenType = TokenTypeEnum()

end module token_type_module