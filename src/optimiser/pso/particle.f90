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

module optimiser_pso_particle
  use kinds, only: wp
  use gpr_module, only: GaussianProcessRegressor
  use optimiser_iter_result, only: IterResult
  use utils, only: random
  implicit none
  private
  public :: Particle, init_particle

  type Particle
    integer :: id
    real(kind=wp), dimension(:), allocatable :: position
    real(kind=wp), dimension(:), allocatable :: velocity
    real(kind=wp) :: cost
    real(kind=wp), dimension(:), allocatable :: best_position
    real(kind=wp) :: best_cost
    real(kind=wp), dimension(:), allocatable :: min
    real(kind=wp), dimension(:), allocatable :: max
  contains
    private
    ! procedure, public, pass(self) :: init
    procedure, public, pass(self) :: randomise
    procedure, public, pass(self) :: nfeatures
    procedure, pass(self) :: assign
    procedure, public, pass(self) :: get_cost
    generic, public :: assignment(=) => assign
  end type Particle

contains

  recursive subroutine init_particle(self, id, op, position, velocity, search_min, search_max, result) 
    type(Particle), intent(inout) :: self
    integer, intent(in) :: id
    type(GaussianProcessRegressor), intent(inout) :: op
    real(kind=wp), dimension(:), intent(in) :: position
    real(kind=wp), dimension(:), intent(in) :: velocity
    real(kind=wp), dimension(:), intent(in) :: search_min
    real(kind=wp), dimension(:), intent(in) :: search_max
    type(IterResult), allocatable, intent(inout) :: result

    real(kind=wp), dimension(:), allocatable :: delta, min_delta
    type(IterResult), allocatable :: particle_result
    integer :: nfeatures

    nfeatures = size(position)

    self%id = id

    if (.not.allocated(delta)) allocate(delta(nfeatures))
    delta = search_max - search_min
    if (.not.allocated(min_delta)) allocate(min_delta(nfeatures))
    min_delta = -delta

    if (.not.allocated(self%min)) allocate(self%min, source=search_min)
    if (.not.allocated(self%max)) allocate(self%max, source=search_max)

    if (.not.allocated(self%position)) allocate(self%position(nfeatures))
    if (.not.allocated(self%velocity)) allocate(self%velocity(nfeatures))
    ! call random(self%position, nfeatures, self%min, self%max)
    self%position = position

    result = op%cost_function(self%position)

    ! do while (result%success .neqv. .true.)
    !   call reinitialise(self, op, result)
    ! end do

    self%cost = result%cost

    !> TODO: Need to fix this for reentry
    self%best_cost = result%cost
    self%best_position = self%position

    !> May need to look at a better initialisation of velocity
    !> This initialisation can escape the bounds in the first iteration
    ! call random(self%velocity, nfeatures, min_delta, delta)
    self%velocity = velocity
  end subroutine init_particle

  recursive subroutine reinitialise(self, op, result)
    class(Particle), intent(inout) :: self
    type(GaussianProcessRegressor), intent(inout) :: op
    type(IterResult), allocatable, intent(inout) :: result
    type(Particle), allocatable :: p

    print*, "Error not reinitialising"
    print*, "Singular matrix encountered, aborting"
    stop
    
    call self%randomise()
    call init_particle(p, self%id, op, self%position, self%velocity, self%min, self%max, result)
    self = p
  end subroutine reinitialise

  subroutine randomise(self)
    class(Particle), intent(inout) :: self
    real(kind=wp), dimension(:), allocatable :: delta
    integer :: nfeatures

    nfeatures = size(self%position)
    if (.not.allocated(delta)) allocate(delta(nfeatures))
    delta = self%max - self%min

    call random(self%position, nfeatures, self%min, self%max)
    call random(self%velocity, nfeatures, -delta, delta)
  end subroutine randomise

  function nfeatures(self)
    class(Particle), intent(in) :: self
    integer :: nfeatures
    nfeatures = size(self%position)
  end function nfeatures

  subroutine assign(self, other)
    class(Particle), intent(inout) :: self
    class(Particle), intent(in) :: other
    
    self%position = other%position
    self%velocity = other%velocity
    self%cost = other%cost
    self%best_cost = other%best_cost
    self%best_position = other%best_position
    self%min = other%min
    self%max = other%max
  end subroutine assign

  function get_cost(self, op) result(particle_result)
    class(Particle), intent(inout) :: self
    type(GaussianProcessRegressor), intent(inout) :: op
    type(IterResult), allocatable :: particle_result

    do while (.true.)
      particle_result = op%cost_function(self%position)
      if (particle_result%success) then
        exit
      else
        call self%randomise()
      end if
    end do
  end function get_cost

end module optimiser_pso_particle
