module csv_module
  implicit none

  type :: dynamic_string
    character(len=:), allocatable :: value  ! Allocatable character string
  end type dynamic_string

  type :: csv_file
    character(len=256) :: filename
    integer :: unit_number
  contains
    procedure :: read => read_csv
    procedure :: write => write_csv
    procedure :: get_header => get_csv_header
    procedure :: add => add_row
    procedure :: next_row => next_csv_row
    procedure :: close => close_csv
    procedure :: destroy => destroy_csv_file
  end type csv_file

contains

  ! Read CSV file into an array of dynamic strings
  subroutine read_csv(this, filename, header_row)
    class(csv_file), intent(inout) :: this
    character(len=*), intent(in) :: filename
    integer, intent(in) :: header_row

    character(len=256) :: line
    integer :: ios, nrows

    this%filename = filename

    ! Open the file for reading
    open(newunit=this%unit_number, file=this%filename, status='old', action='read', iostat=ios)
    if (ios /= 0) stop "Error opening file for reading"

    ! Read the header line first (if applicable)
    if (header_row > 0) then
      read(this%unit_number, '(A)', iostat=ios) line
      if (ios /= 0) stop "Error reading header line"
    end if

    ! Count rows (this is a simple approach; could be improved)
    nrows = 0
    do while (.true.)
      read(this%unit_number, '(A)', iostat=ios) line
      if (ios /= 0) exit ! Stop when reaching the end of the file
      nrows = nrows + 1
    end do

    rewind(this%unit_number) ! Go back to the start of the file

  end subroutine read_csv

  ! Write an array of dynamic strings to a CSV file
  subroutine write_csv(this, data)
    class(csv_file), intent(inout) :: this
    type(dynamic_string), dimension(:), intent(in) :: data

    integer :: ios, i

    ! Open the file for writing (overwrite mode)
    open(newunit=this%unit_number, file=this%filename, status='replace', action='write', iostat=ios)
    if (ios /= 0) stop "Error opening file for writing"

    ! Write each row to the CSV file
    do i = 1, size(data)
      write(this%unit_number, '(A)', iostat=ios) trim(data(i)%value)
      if (ios /= 0) stop "Error writing to CSV file"
    end do

  end subroutine write_csv

  ! Get header from the CSV file (returns as an array of dynamic strings)
  subroutine get_csv_header(this, header)
    class(csv_file), intent(inout) :: this
    type(dynamic_string), dimension(:), allocatable :: header

    integer :: ios, i
    character(len=256) :: line

    ! Read the first line as header and split it into an array.
    read(this%unit_number, '(A)', iostat=ios) line
    if (ios == 0) then
      call split(line, header)
      return
    else
      stop "Error reading header"
    endif

  end subroutine get_csv_header

  ! Add a row to the CSV file (append mode)
  subroutine add_row(this, row_data)
    class(csv_file), intent(inout) :: this
    type(dynamic_string), dimension(:), intent(in) :: row_data

    integer :: ios

    ! Open in append mode
    open(newunit=this%unit_number, file=this%filename, status='old', action='write', position='append', iostat=ios)
    if (ios /= 0) stop "Error opening file for appending"

    write(this%unit_number,'(A)',iostat=ios) trim(row_data(1)%value)  ! Assuming single row of data at a time.
    if (ios /= 0) stop "Error writing row to CSV"

    close(this%unit_number)

  end subroutine add_row

  ! Move to the next row in the CSV file (used for writing sequentially)
  subroutine next_csv_row(this)
    class(csv_file), intent(inout):: this

    integer :: ios
    write(this%unit_number,'(A)',iostat=ios)""
    if (ios /= 0) stop "Error writing to CSV"

  end subroutine next_csv_row

  ! Close the CSV file unit properly.
  subroutine close_csv(this)
    class(csv_file), intent(inout):: this

    close(this%unit_number)

  end subroutine close_csv

  ! Destroy the csv_file object and free resources.
  subroutine destroy_csv_file(this)
    class(csv_file), intent(inout):: this

    call close_csv(this)

  end subroutine destroy_csv_file

  ! Split a string into an array based on a delimiter (comma by default).
  subroutine split(input_line, output_array)
    implicit none

    character(len=*), intent(in) :: input_line  ! Input string to split.
    type(dynamic_string), allocatable :: output_array(:)  ! Output array of split strings.

    integer :: count, i, start_pos, end_pos

    count = 0

    ! Count how many tokens there are.
    start_pos = 1

    do while (start_pos <= len_trim(input_line))
      end_pos = index(input_line(start_pos:), ',')  ! Find comma delimiter.
      if (end_pos == 0) then                 ! No more commas found.
        end_pos = len_trim(input_line(start_pos:)) + start_pos - 1  ! Get last token.
      else
        end_pos = start_pos + end_pos - 2  ! Adjust position for substring.
      endif

      count = count + 1
    enddo

    allocate(output_array(count))  ! Allocate the entire array first

    start_pos = 1
    do i = 1, count
      end_pos = index(input_line(start_pos:), ',')  ! Find comma delimiter.
      if (end_pos == 0) then
        end_pos = len_trim(input_line(start_pos:)) + start_pos - 1
      else
        end_pos = start_pos + end_pos - 2
      endif

      allocate(output_array(i))  ! Allocate space for each element within the array
      output_array(i)%value = trim(input_line(start_pos:end_pos))  ! Extract token.
      start_pos = end_pos + 2
    enddo

  end subroutine split

end module csv_module
