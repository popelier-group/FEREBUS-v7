module lexer_module
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
  use utils, only: is_space, is_digit, is_alpha, is_alphanum, to_lower, concat

  implicit none

  type :: Lexer
    character(len=:), allocatable :: text
    integer :: position
    character :: current_char
  contains
    private
    procedure, public, pass(self) :: init => init_lexer
    procedure, public, pass(self) :: error
    procedure, public, pass(self) :: advance
    procedure, public, pass(self) :: skip_whitespace
    procedure, public, pass(self) :: number
    procedure, public, pass(self) :: id
    procedure, public, pass(self) :: get_next_token
  end type Lexer

contains

  subroutine init_lexer(self, text)
    class(Lexer), intent(inout) :: self
    character(len=*), intent(in) :: text    

    allocate(self%text, source=text)
    self%text = text
    self%position = 1
    self%current_char = self%text(self%position:self%position)
  end subroutine init_lexer

  subroutine error(self)
    class(Lexer), intent(in) :: self

    !> TODO: Convert this to fatal error
    print*, self%text, "(", self%position, ")"
    print*, "Invalid Character"
    stop
  end subroutine error

  subroutine advance(self)
    class(Lexer), intent(inout) :: self

    self%position = self%position + 1
    if (self%position .gt. len(self%text)) then
      self%current_char = char(0) ! <- NUL Character Used to indicate the end of the string
    else
      self%current_char = self%text(self%position:self%position)
    end if
  end subroutine advance

  subroutine skip_whitespace(self)
    class(Lexer), intent(inout) :: self

    do while ((self%current_char .ne. char(0)) .and. (is_space(self%current_char) .eqv. .true.))
      call self%advance()
    end do
  end subroutine skip_whitespace

  function number(self) result(num)
    class(Lexer), intent(inout) :: self
    real(kind=wp) :: num
    character(len=:), allocatable :: result

    result = ""
    do while ((self%current_char .ne. char(0)) .and. (is_digit(self%current_char) .eqv. .true.))
      result = concat(result, self%current_char)
      call self%advance()
    end do

    ! Check real
    if (self%current_char .eq. ".") then
      result = concat(result, self%current_char)
      call self%advance()
      do while ((self%current_char .ne. char(0)) .and. (is_digit(self%current_char) .eqv. .true.))
        result = concat(result, self%current_char)
        call self%advance()
      end do
    end if

    ! Check scientific notation
    if (to_lower(self%current_char) .eq. "e") then
      result = concat(result, to_lower(self%current_char))
      call self%advance()
      if (any(to_lower(self%current_char) .eq. (/"+", "-"/))) then
        result = concat(result, self%current_char)
        call self%advance()
      end if
      do while ((self%current_char .ne. char(0)) .and. (is_digit(self%current_char) .eqv. .true.))
        result = concat(result, self%current_char)
        call self%advance()
      end do
    end if

    read(result, *) num
  end function number

  function id(self) result(var)
    class(Lexer), intent(inout) :: self
    character(len=:), allocatable :: var

    var = ""
    do while ((self%current_char .ne. char(0)) .and. (is_alphanum(self%current_char) .eqv. .true.))
      var = concat(var, self%current_char)
      call self%advance()
    end do
  end function id

  function get_next_token(self) result(t)
    class(Lexer), intent(inout) :: self
    class(Token), allocatable :: t

    do while (self%current_char .ne. char(0))
      if (is_space(self%current_char) .eqv. .true.) then
        call self%skip_whitespace()
        continue
      end if

      if (is_alpha(self%current_char) .eqv. .true.) then
        allocate(IdToken :: t)
        call t%init(TokenType%Id, self%id())
        exit
      end if

      if (is_digit(self%current_char) .eqv. .true.) then
        allocate(NumberToken :: t)
        call t%init(TokenType%Number, self%number())
        exit
      end if

      if (self%current_char .eq. '+') then
        allocate(PlusToken :: t)
        call t%init(TokenType%Plus)
        call self%advance()
        exit
      end if

      if (self%current_char .eq. '-') then
        allocate(MinusToken :: t)
        call t%init(TokenType%Minus)
        call self%advance()
        exit
      end if

      if (self%current_char .eq. '*') then
        allocate(MulToken :: t)
        call t%init(TokenType%Mul)
        call self%advance()
        exit
      end if

      if (self%current_char .eq. '/') then
        allocate(DivToken :: t)
        call t%init(TokenType%Div)
        call self%advance()
        exit
      end if

      if (self%current_char .eq. '(') then
        allocate(LParenToken :: t)
        call t%init(TokenType%LParen)
        call self%advance()
        exit
      end if

      if (self%current_char .eq. ')') then
        allocate(RParenToken :: t)
        call t%init(TokenType%RParen)
        call self%advance()
        exit
      end if

      call self%error()
    end do

    if (.not.allocated(t)) then
      allocate(EofToken :: t)
      call t%init(TokenType%Eof)
    end if
  end function get_next_token
end module lexer_module
