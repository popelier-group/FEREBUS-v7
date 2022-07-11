module linalg_utils
  use kinds, only: sp, dp
  implicit none
  private
  public :: potrf, potrs, posv

  interface potrf
    module procedure potrf_sp
    module procedure potrf_dp
  end interface potrf

  interface potrs
    module procedure potrs_sp
    module procedure potrs_dp
  end interface potrs

  interface posv
    module procedure posv_sp
    module procedure posv_dp
  end interface posv

  character, parameter :: UPLO_LAPACK = 'L'
contains

  subroutine potrf_sp(A, UPLO, N, LDA, INFO)
    real(kind=sp), dimension(:,:), intent(inout) :: A

    character, optional, intent(in) :: UPLO
    integer, optional, intent(in) :: N
    integer, optional, intent(in) :: LDA
    integer, optional, intent(out) :: INFO

    character :: UPLO_
    integer :: N_
    integer :: LDA_
    integer :: INFO_

    UPLO_ = UPLO_LAPACK
    N_ = size(A, 1)
    LDA_ = size(A, 1)

    if (present(UPLO)) UPLO_ = UPLO
    if (present(N)) N_ = N
    if (present(LDA)) LDA_ = LDA

    call spotrf(UPLO_, N_, A, LDA_, INFO_)

    if (present(INFO)) INFO = INFO_
  end subroutine potrf_sp

  subroutine potrf_dp(A, UPLO, N, LDA, INFO)
    real(kind=dp), dimension(:,:), intent(inout) :: A

    character, optional, intent(in) :: UPLO
    integer, optional, intent(in) :: N
    integer, optional, intent(in) :: LDA
    integer, optional, intent(out) :: INFO

    character :: UPLO_
    integer :: N_
    integer :: LDA_
    integer :: INFO_

    UPLO_ = UPLO_LAPACK
    N_ = size(A, 1)
    LDA_ = size(A, 1)

    if (present(UPLO)) UPLO_ = UPLO
    if (present(N)) N_ = N
    if (present(LDA)) LDA_ = LDA

    
    call dpotrf(UPLO_, N_, A, LDA_, INFO_)

    if (present(INFO)) INFO = INFO_
  end subroutine potrf_dp

  subroutine potrs_sp(A, B, UPLO, N, NRHS, LDA, LDB, INFO)
    real(kind=sp), dimension(:,:), intent(in) :: A
    real(kind=sp), dimension(:,:), intent(inout) :: B

    character, optional, intent(in) :: UPLO
    integer, optional, intent(in) :: N
    integer, optional, intent(in) :: NRHS
    integer, optional, intent(in) :: LDA
    integer, optional, intent(in) :: LDB
    integer, optional, intent(out) :: INFO

    character :: UPLO_
    integer :: N_
    integer :: NRHS_
    integer :: LDA_
    integer :: LDB_
    integer :: INFO_

    UPLO_ = UPLO_LAPACK
    N_ = size(A, 1)
    NRHS_ = size(B, 2)
    LDA_ = size(A, 1)
    LDB_ = size(B, 1)

    if (present(UPLO)) UPLO_ = UPLO
    if (present(N)) N_ = N
    if (present(NRHS)) NRHS_ = NRHS
    if (present(LDA)) LDA_ = LDA
    if (present(LDB)) LDB_ = LDB

    call spotrs(UPLO_, N_, NRHS_, A, LDA_, B, LDB_, INFO_)

    if (present(INFO)) INFO = INFO_
  end subroutine potrs_sp

  subroutine potrs_dp(A, B, UPLO, N, NRHS, LDA, LDB, INFO)
    real(kind=dp), dimension(:,:), intent(in) :: A
    real(kind=dp), dimension(:,:), intent(inout) :: B

    character, optional, intent(in) :: UPLO
    integer, optional, intent(in) :: N
    integer, optional, intent(in) :: NRHS
    integer, optional, intent(in) :: LDA
    integer, optional, intent(in) :: LDB
    integer, optional, intent(out) :: INFO

    character :: UPLO_
    integer :: N_
    integer :: NRHS_
    integer :: LDA_
    integer :: LDB_
    integer :: INFO_

    UPLO_ = UPLO_LAPACK
    N_ = size(A, 1)
    NRHS_ = size(B, 2)
    LDA_ = size(A, 1)
    LDB_ = size(B, 1)

    if (present(UPLO)) UPLO_ = UPLO
    if (present(N)) N_ = N
    if (present(NRHS)) NRHS_ = NRHS
    if (present(LDA)) LDA_ = LDA
    if (present(LDB)) LDB_ = LDB

    call dpotrs(UPLO_, N_, NRHS_, A, LDA_, B, LDB_, INFO_)

    if (present(INFO)) INFO = INFO_
  end subroutine potrs_dp

  subroutine posv_sp(A, B, UPLO, N, NRHS, LDA, LDB, INFO)
    real(kind=sp), dimension(:,:), intent(in) :: A
    real(kind=sp), dimension(:,:), intent(inout) :: B

    character, optional, intent(in) :: UPLO
    integer, optional, intent(in) :: N
    integer, optional, intent(in) :: NRHS
    integer, optional, intent(in) :: LDA
    integer, optional, intent(in) :: LDB
    integer, optional, intent(out) :: INFO

    character :: UPLO_
    integer :: N_
    integer :: NRHS_
    integer :: LDA_
    integer :: LDB_
    integer :: INFO_

    UPLO_ = UPLO_LAPACK
    N_ = size(A, 1)
    NRHS_ = size(B, 2)
    LDA_ = size(A, 1)
    LDB_ = size(B, 1)

    if (present(UPLO)) UPLO_ = UPLO
    if (present(N)) N_ = N
    if (present(NRHS)) NRHS_ = NRHS
    if (present(LDA)) LDA_ = LDA
    if (present(LDB)) LDB_ = LDB

    call sposv(UPLO_, N_, NRHS_, A, LDA_, B, LDB_, INFO_)

    if (present(INFO)) INFO = INFO_
  end subroutine posv_sp

  subroutine posv_dp(A, B, UPLO, N, NRHS, LDA, LDB, INFO)
    real(kind=dp), dimension(:,:), intent(in) :: A
    real(kind=dp), dimension(:,:), intent(inout) :: B

    character, optional, intent(in) :: UPLO
    integer, optional, intent(in) :: N
    integer, optional, intent(in) :: NRHS
    integer, optional, intent(in) :: LDA
    integer, optional, intent(in) :: LDB
    integer, optional, intent(out) :: INFO

    character :: UPLO_
    integer :: N_
    integer :: NRHS_
    integer :: LDA_
    integer :: LDB_
    integer :: INFO_

    UPLO_ = UPLO_LAPACK
    N_ = size(A, 1)
    NRHS_ = size(B, 2)
    LDA_ = size(A, 1)
    LDB_ = size(B, 1)

    if (present(UPLO)) UPLO_ = UPLO
    if (present(N)) N_ = N
    if (present(NRHS)) NRHS_ = NRHS
    if (present(LDA)) LDA_ = LDA
    if (present(LDB)) LDB_ = LDB

    call dposv(UPLO_, N_, NRHS_, A, LDA_, B, LDB_, INFO_)

    if (present(INFO)) INFO = INFO_
  end subroutine posv_dp
end module linalg_utils
