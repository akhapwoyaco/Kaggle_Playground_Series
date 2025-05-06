 subroutine read_train_data(filename, dates, sales, num_rows)
    character(len=*), intent(in) :: filename
    character(len=10), allocatable, intent(out) :: dates(:)
    real(real64), allocatable, intent(out) :: sales(:)
    integer, intent(out) :: num_rows
    integer :: unit_num, io_stat, valid_rows, temp_unit
    character(len=1000) :: line
    real(real64) :: sale_value
    logical, allocatable :: valid_mask(:)
    character(len=10), allocatable :: temp_dates(:)
    real(real64), allocatable :: temp_sales(:)

    character(len=32) :: sale_value_str

    ! First pass: Count total rows and valid rows
    open(newunit=unit_num, file=filename, status='old', action='read', iostat=io_stat)
    if (io_stat /= 0) then
      write(*, '(A)') 'Error: Unable to open training file'
      stop
    end if

    num_rows = -1  ! Skip header
    do
      read(unit_num, '(A)', iostat=io_stat) line
      if (io_stat /= 0) exit
      num_rows = num_rows + 1
    end do
    close(unit_num)

    ! Allocate temporary arrays
    allocate(temp_dates(num_rows), temp_sales(num_rows), valid_mask(num_rows))
    valid_mask = .false.
    valid_rows = 0

    ! Second pass: Read data and check for valid sales
    open(newunit=unit_num, file=filename, status='old', action='read')
    read(unit_num, '(A)')  ! Skip header

    do i = 1, num_rows
      read(unit_num, *, iostat=io_stat) temp_dates(i), sale_value
      ! Convert sale_value to a character string for comparison

      write(sale_value_str, '(F16.8)') sale_value

      if (io_stat == 0 .and. sale_value > 0.0 .and. (sale_value /= 0.0 .or. trim(adjustl(sale_value_str)) /= 'None')) then
        temp_sales(i) = log(sale_value)
        valid_mask(i) = .true.
        valid_rows = valid_rows + 1
      end if
    end do
    close(unit_num)

    ! Allocate final arrays with correct size
    allocate(dates(valid_rows), sales(valid_rows))

    ! Copy valid data to final arrays
    j = 1
    do i = 1, num_rows
      if (valid_mask(i)) then
        dates(j) = temp_dates(i)
        sales(j) = temp_sales(i)
        j = j + 1
      end if
    end do

    ! Update num_rows to reflect valid rows
    num_rows = valid_rows

    ! Clean up temporary arrays
    deallocate(temp_dates, temp_sales, valid_mask)

    write(*, '(A,I0,A,I0,A)') 'Processed ', num_rows, ' valid observations out of ', size(valid_mask), ' total records'

  end subroutine read_train_data
