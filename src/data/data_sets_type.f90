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

module data_sets_type_module
  use kinds, only: wp
  use constants, only: MAX_OUTPUT_CHARACTER_LENGTH 
  use data_set_type_module, only: DataSet
  use standardised_data_set_module, only: StandardisedDataSet
  use units_module, only: UnitType
  implicit none
  private
  public :: DataSets

  type DataSetElement
    class(DataSet), allocatable :: element
  contains
    procedure, public, pass(self) :: init => init_dataset_element
  end type DataSetElement

  type DataSets
    type(DataSetElement), dimension(:), allocatable :: data_sets
  contains
    private
    procedure, public, pass(self) :: init => init_datasets
    procedure, public, pass(self) :: nout
  end type DataSets

contains

  subroutine init_datasets(self, x, y, xunits, yunits, y_headers, standardise)
    class(DataSets), intent(inout) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
    real(kind=wp), dimension(:,:), intent(in) :: y
    type(UnitType), dimension(:), intent(in) :: xunits, yunits
    character(len=*), dimension(:), optional, intent(in) :: y_headers
    logical, optional, intent(in) :: standardise

    type(DataSet), allocatable :: ds
    character(len=MAX_OUTPUT_CHARACTER_LENGTH), dimension(:), allocatable :: y_labels
    integer :: i, nout
    logical :: standardise_

    standardise_ = .false.
    if (present(standardise)) standardise_ = standardise

    nout = size(y(1, :))
    allocate(y_labels(nout))
    if (present(y_headers)) then
      y_labels = y_headers
    else
      do i = 1, nout
        write(y_labels(i), "(A, I0)") "p", i
      end do
    end if
    
    allocate(self%data_sets(nout))
    do i = 1, nout
      ! if (standardise_ .eqv. .true.) then
      !   allocate(StandardisedDataSet :: ds)
      ! else
      !   allocate(DataSet :: ds)
      ! end if
      allocate(ds)
      call ds%init(x, y(:,i), y_labels(i), xunits, yunits(i))
      call self%data_sets(i)%init(ds)
      deallocate(ds)
    end do
  end subroutine init_datasets

  subroutine init_dataset_element(self, data_set)
    class(DataSetElement), intent(inout) :: self
    class(DataSet), intent(in) :: data_set

    allocate(self%element, source=data_set)
  end subroutine init_dataset_element

  function nout(self)
    class(DataSets), intent(in) :: self
    integer :: nout

    nout = size(self%data_sets)
  end function nout

end module data_sets_type_module
