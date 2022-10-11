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

module distance_matrix_module
  use kinds, only: wp
  use constants, only: PI, TWOPI

  implicit none
  private

  real(kind=wp), dimension(:,:,:), allocatable :: distance_matrix
  real(kind=wp), dimension(:,:,:), allocatable :: cyclic_distance_matrix
  real(kind=wp), dimension(:,:,:), allocatable :: periodic_distance_matrix

  logical :: distance_matrix_required = .false.
  logical :: cyclic_distance_matrix_required = .false.
  logical :: periodic_distance_matrix_required = .false.

  public :: distance_matrix, &
            cyclic_distance_matrix, &
            distance_matrix_required, &
            cyclic_distance_matrix_required, &
            compute_distance_matrix, &
            periodic_distance_matrix_required, &
            periodic_distance_matrix

  contains

  subroutine compute_distance_matrix(x)
    real(kind=wp), dimension(:,:), intent(in) :: x !> (nTrain, nFeats)

    if (distance_matrix_required) then
      call compute_standard_distance_matrix(x)
    end if

    if (cyclic_distance_matrix_required) then
      call compute_cyclic_distance_matrix(x)
    end if
    
    if (periodic_distance_matrix_required) then
      call compute_periodic_distance_matrix(x)
    end if
  end subroutine compute_distance_matrix

  subroutine compute_standard_distance_matrix(x)
    real(kind=wp), dimension(:,:), intent(in) :: x !> (nTrain, nFeats)

    real(kind=wp), allocatable, dimension(:,:) :: xt
    real(kind=wp), dimension(:), allocatable :: diff
    integer :: i, j, ntrain, nfeats

    ntrain = size(x, 1)
    nfeats = size(x, 2)

    if (allocated(distance_matrix)) deallocate(distance_matrix)
    allocate(distance_matrix(nfeats, ntrain, ntrain))
    allocate(diff(nfeats))
    allocate(xt(nfeats, ntrain))

    xt = transpose(x)
    !$omp parallel private(i, j, diff) 
    !$omp do schedule(dynamic)
    do j = 1, ntrain
      distance_matrix(:, j, j) = 0.0_wp
      do i = j + 1, ntrain
        diff = xt(:, i) - xt(:, j)
        distance_matrix(:, i, j) = diff*diff
      end do
    end do
    !$omp end do    
    !$omp end parallel

    !$acc enter data copyin(distance_matrix) async(1)
  end subroutine compute_standard_distance_matrix

  subroutine compute_cyclic_distance_matrix(x)
    real(kind=wp), dimension(:,:), intent(in) :: x !> (nTrain, nFeats)

    real(kind=wp), allocatable, dimension(:,:) :: xt
    real(kind=wp), dimension(:), allocatable :: diff
    integer :: i, j, ntrain, nfeats

    ntrain = size(x, 1)
    nfeats = size(x, 2)

    if (allocated(cyclic_distance_matrix)) deallocate(cyclic_distance_matrix)
    allocate(cyclic_distance_matrix(nfeats, ntrain, ntrain))
    allocate(diff(nfeats))
    allocate(xt(nfeats, ntrain))

    cyclic_distance_matrix = 0.0_wp

    xt = transpose(x)
    !$omp parallel private(i, j, diff) 
    !$omp do schedule(dynamic)
    do j = 1, ntrain
      cyclic_distance_matrix(:, j, j) = 0.0_wp
      do i = j + 1, ntrain
        diff = xt(:, i) - xt(:, j)
        diff(3:size(diff,1):3) = sign(1.0_wp,diff(3:size(diff):3))* &
                                    (mod(abs(diff(3:size(diff):3))+PI,TWOPI)- PI)
        cyclic_distance_matrix(:, i, j) = diff*diff
      end do
    end do
    !$omp end do    
    !$omp end parallel

    !$acc enter data copyin(cyclic_distance_matrix) async(1)
  end subroutine compute_cyclic_distance_matrix

  subroutine compute_periodic_distance_matrix(x)
    real(kind=wp), dimension(:,:), intent(in) :: x !> (nTrain, nFeats)

    if (.not. distance_matrix_required) then
      call compute_standard_distance_matrix(x)
    end if

    if (allocated(periodic_distance_matrix)) deallocate(periodic_distance_matrix)
    allocate(periodic_distance_matrix, mold=distance_matrix)

    periodic_distance_matrix = sin(sqrt(distance_matrix) / 2.0_wp)  ! <- is this the best we can do?
    periodic_distance_matrix = periodic_distance_matrix*periodic_distance_matrix
  end subroutine compute_periodic_distance_matrix

end module distance_matrix_module
