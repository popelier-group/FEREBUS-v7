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

module utils_toml
  use kinds, only: wp
  use tomlf, only: toml_table, toml_key, get_value, toml_stat, new_table, toml_array
  use tomlf_type, only : len
  implicit none
  private
  public :: get_value_converted, get_table, is_empty_table

  interface get_value_converted
    module procedure get_value_real
    module procedure get_value_int
    module procedure get_ivalue_real
    module procedure get_ivalue_int
    module procedure get_value_real_arr
    module procedure get_value_int_arr
  end interface get_value_converted

contains

  subroutine get_value_real(table, key, value, default, stat)
    type(toml_table), intent(inout) :: table
    character(len=*), intent(in) :: key
    real(kind=wp), intent(out) :: value
    real(kind=wp), optional, intent(in) :: default
    integer, optional, intent(out) :: stat

    integer :: stat_, tmp

    stat_ = toml_stat%fatal

    if (table%has_key(key).eqv..true.) then
      call get_value(table, key, value, stat=stat_)
      ! print*, stat_
      if (stat_.ne.toml_stat%success) then
        call get_value(table, key, tmp, stat=stat_)
        if (stat_.eq.toml_stat%success) then
          value = real(tmp, kind=wp)  
        end if
      end if
    end if

    if ((present(default)).and.(.not.stat_.eq.toml_stat%success)) then
      value = default
      stat_ = toml_stat%success
    end if

    if (present(stat)) then
      stat = stat_
    end if
  end subroutine get_value_real

  subroutine get_ivalue_real(table, ikey, value, default, stat)
    type(toml_array), intent(inout) :: table
    integer, intent(in) :: ikey
    real(kind=wp), intent(out) :: value
    real(kind=wp), optional, intent(in) :: default
    integer, optional, intent(out) :: stat

    integer :: stat_, tmp

    stat_ = toml_stat%fatal

    call get_value(table, ikey, value, stat=stat_)
    if (stat_.ne.toml_stat%success) then
      call get_value(table, ikey, tmp, stat=stat_)
      if (stat_.eq.toml_stat%success) then
        value = real(tmp, kind=wp)  
      end if
    end if

    if ((present(default)).and.(.not.stat_.eq.toml_stat%success)) then
      value = default
      stat_ = toml_stat%success
    end if

    if (present(stat)) then
      stat = stat_
    end if
  end subroutine get_ivalue_real

  subroutine get_value_int(table, key, value, default, stat)
    type(toml_table), intent(inout) :: table
    character(len=*), intent(in) :: key
    integer, intent(out) :: value
    integer, optional, intent(in) :: default
    integer, optional, intent(out) :: stat

    integer :: stat_
    real(kind=wp) :: tmp

    stat_ = toml_stat%fatal

    if (table%has_key(key).eqv..true.) then
      call get_value(table, key, value, stat=stat_)
      if (stat_.ne.toml_stat%success) then
        call get_value(table, key, tmp, stat=stat_)
        if (stat_.eq.toml_stat%success) then
          value = int(tmp)  
        end if
      end if
    end if

    if ((present(default)).and.(.not.stat_.eq.toml_stat%success)) then
      value = default
      stat_ = toml_stat%success
    end if

    if (present(stat)) then
      stat = stat_
    end if
  end subroutine get_value_int

  subroutine get_ivalue_int(table, ikey, value, default, stat)
    type(toml_array), intent(inout) :: table
    integer, intent(in) :: ikey
    integer, intent(out) :: value
    integer, optional, intent(in) :: default
    integer, optional, intent(out) :: stat

    integer :: stat_
    real(kind=wp) :: tmp

    stat_ = toml_stat%fatal

    call get_value(table, ikey, value)
    if (stat_.ne.toml_stat%success) then
      call get_value(table, ikey, tmp)
      if (stat_.eq.toml_stat%success) then
        value = int(tmp)  
      end if
    end if

    if ((present(default)).and.(.not.stat_.eq.toml_stat%success)) then
      value = default
      stat_ = toml_stat%success
    end if

    if (present(stat)) then
      stat = stat_
    end if
  end subroutine get_ivalue_int

  subroutine get_value_real_arr(table, key, value, default, stat)
    type(toml_table), intent(inout) :: table
    character(len=*), intent(in) :: key
    real(kind=wp), allocatable, dimension(:), intent(out) :: value
    real(kind=wp), dimension(:), optional, intent(in) :: default
    integer, optional, intent(out) :: stat

    type(toml_array), pointer :: children
    integer :: n, i, stat_

    if (table%has_key(key).eqv..true.) then
      call get_value(table, key, children, requested=.false.)
      if (associated(children)) then
        n = len(children)
        allocate(value(n))
        do i = 1, n
          call get_value_converted(children, i, value(i), stat=stat_)
        end do
      end if
    end if

    if ((present(default)).and.(.not.allocated(value))) then
      value = default
      stat_ = toml_stat%success
    end if

    if (present(stat)) then
      stat = stat_
    end if
  end subroutine get_value_real_arr

  subroutine get_value_int_arr(table, key, value, default, stat)
    type(toml_table), intent(inout) :: table
    character(len=*), intent(in) :: key
    integer, allocatable, dimension(:), intent(out) :: value
    integer, dimension(:), optional, intent(in) :: default
    integer, optional, intent(out) :: stat

    type(toml_array), pointer :: children
    integer :: n, i, stat_

    if (table%has_key(key).eqv..true.) then
      call get_value(table, key, children, requested=.false.)
      if (associated(children)) then
        n = len(children)
        allocate(value(n))
        do i = 1, n
          call get_value_converted(children, i, value(i), stat=stat_)
        end do
      end if
    end if

    if ((present(default)).and.(.not.allocated(value))) then
      value = default
      stat_ = toml_stat%success
    end if

    if (present(stat)) then
      stat = stat_
    end if
  end subroutine get_value_int_arr

  subroutine get_table(table, table_name, child)
    type(toml_table), intent(inout) :: table
    character(len=*), intent(in) :: table_name
    type(toml_table), pointer, intent(out) :: child

    call get_value(table, table_name, child)

    if (.not.associated(child)) then
      allocate(child)
      call new_table(child)
    end if
  end subroutine get_table

  function is_empty_table(table) result(is_empty)
    type(toml_table), intent(inout) :: table
    logical :: is_empty
    type(toml_key), dimension(:), allocatable :: keys

    call table%get_keys(keys)
    is_empty = size(keys) .eq. 0
  end function is_empty_table
end module utils_toml