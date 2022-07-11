module csv
  use kinds, only: wp
  use utils_str, only: is_feature_header, contains_substr
  use atom_config_module, only: AtomConfig
  use error_module, only: error, fatal_error, display_error
  use data_set_module, only: DataSets
  use constants, only: MAX_OUTPUT_CHARACTER_LENGTH
  use units_module, only: UnitType
  implicit none
  private 

  public :: read_training_set
  contains

  function read_training_set(training_set_file, atm_data, output_unit, standardise) result(data_set)
    character(len=*), intent(in) :: training_set_file
    type(AtomConfig), intent(in) :: atm_data
    integer, intent(in) :: output_unit
    logical, intent(in) :: standardise
    type(DataSets), allocatable :: data_set

    character(len=1024) :: input_str
    character(len=MAX_OUTPUT_CHARACTER_LENGTH), dimension(:), allocatable :: header
    real(wp),dimension(:),allocatable :: tmp
    logical :: status_ok
    integer,dimension(:),allocatable :: itypes
    integer :: i, j

    integer :: nx, ny, ix, iy, nrows, ncols, io
    integer, dimension(:), allocatable :: input_cols, output_cols, header_type
    integer, parameter :: NONE=0, INPUT=1, OUTPUT=2
    type(error), allocatable :: csv_error
    real(kind=wp), dimension(:), allocatable :: record
    real(kind=wp), dimension(:, :), allocatable :: training_inputs, training_outputs
    character(len=MAX_OUTPUT_CHARACTER_LENGTH), dimension(:), allocatable :: output_headers
    character(len=MAX_OUTPUT_CHARACTER_LENGTH) :: xunit, yunit
    type(UnitType), dimension(:), allocatable :: xunits, yunits

    open(unit=10, file=training_set_file, action='read')
      nrows = 0
      do
        read(10,*, iostat=io)
        if (io .lt. 0) then
          exit
        end if
        nrows = nrows + 1
      end do
      nrows = nrows - 1
      rewind(10)
      read(10, '(A)') input_str
      ncols = 0
      do i = 1, len(input_str)
        if (input_str(i:i) .eq. ',') then
          ncols = ncols + 1
        end if
      end do
      ncols = ncols + 1
      rewind(10)

      allocate(header(ncols))
      read(10, *) header
      allocate(header_type(ncols))

      nx = 0
      ny = 0
      do i = 1, size(header)
        if (is_feature_header(header(i))) then
          header_type(i) = INPUT
        else if (i .eq. 1) then
          header_type(i) = NONE
        else
          if (allocated(atm_data%properties)) then
            do j = 1, size(atm_data%properties)
              if (trim(header(i)) .eq. trim(atm_data%properties(j)%name)) then
                header_type(i) = OUTPUT
                exit
              else
                header_type(i) = NONE
              end if
            end do
          else
            header_type(i) = OUTPUT
          end if
        end if
      end do

      do i = 1, size(header)
        select case(header_type(i))
          case (INPUT)
            nx = nx + 1
          case (OUTPUT)
            ny  = ny + 1
        end select
      end do

      allocate(input_cols(nx))
      allocate(output_cols(ny))

      allocate(training_inputs(nrows, nx))
      allocate(training_outputs(nrows, ny))
      allocate(output_headers(ny))
      allocate(xunits(nx))
      allocate(yunits(ny))

      ix = 1
      iy = 1
      do i = 1, size(header)
        select case(header_type(i))
          case (INPUT)
            input_cols(ix) = i

            if (contains_substr(header(i), "/")) then
              call xunits(ix)%init(header(i)(index(header(i), "/")+1:len(header(i))))
            else
              call xunits(ix)%unknown_input(ix)
            end if

            ix = ix + 1
          case (OUTPUT)
            output_cols(iy) = i
            
            if (contains_substr(header(output_cols(iy)), "/")) then
              call yunits(iy)%init(header(output_cols(iy))(index(header(output_cols(iy)), "/")+1:len(header(output_cols(iy)))))
              output_headers(iy) = trim(header(output_cols(iy))(1:index(header(output_cols(iy)), "/")-1))
            else
              call yunits(iy)%unknown_output(header(output_cols(iy)))
              output_headers(iy) = header(output_cols(iy))
            end if
            iy = iy + 1
        end select
      end do

      allocate(record(ncols))
      do i = 1, nrows
        read(10, *) record
        do ix = 1, nx
          training_inputs(i, ix) = record(input_cols(ix))
        end do

        do iy = 1, ny
          training_outputs(i, iy) = record(output_cols(iy))
        end do
      end do
    close(10)

    allocate(data_set)
    call data_set%init(training_inputs, training_outputs, xunits, yunits, y_headers=output_headers, standardise=standardise)

    deallocate(input_cols)
    deallocate(output_cols)
    deallocate(training_inputs)
    deallocate(training_outputs)
    deallocate(output_headers)
    deallocate(header_type)
  end function read_training_set

end module csv
