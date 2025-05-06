program sales_analysis
  use iso_fortran_env, only: real64
  implicit none

  ! Define variables
  character(len=100) :: train_file, test_file, holidays_file, submission_file
  integer :: i, j, status, num_train_rows, num_test_rows, num_holiday_rows
  real(real64), allocatable :: log_sales(:), train_features(:,:), test_features(:,:)
  real(real64), allocatable :: coefficients(:), predictions(:)
  character(len=10), allocatable :: train_dates(:), test_dates(:), holiday_dates(:)
  character(len=3), allocatable :: train_weekdays(:), test_weekdays(:)
  integer, allocatable :: train_months(:), train_years(:), test_months(:), test_years(:)
  integer, allocatable :: test_ids(:)
  character(len=20), allocatable :: holiday_types(:), train_holiday_types(:), test_holiday_types(:)
  character(len=2), allocatable :: countries(:)

  ! File names
  train_file = 'train.csv'
  test_file = 'test.csv'
  holidays_file = 'global_holidays.csv'
  submission_file = 'predictions.csv'

  ! Read data
  write(*, '(A)') 'Reading training data...'
  call read_train_data(train_file, train_dates, log_sales, num_train_rows)

  write(*, '(A)') 'Reading test data...'
  call read_test_data(test_file, test_dates, test_ids, num_test_rows)

  write(*, '(A)') 'Reading holiday data...'
  call read_holiday_data(holidays_file, holiday_dates, holiday_types, countries, num_holiday_rows)

  ! Process dates
  write(*, '(A)') 'Processing dates...'
  call process_dates(train_dates, train_weekdays, train_months, train_years)
  call process_dates(test_dates, test_weekdays, test_months, test_years)

  ! Merge holidays
  write(*, '(A)') 'Merging holiday data...'
  call merge_holidays(train_dates, "US", holiday_dates, holiday_types, train_holiday_types)  ! Example for US
  call merge_holidays(test_dates, "US", holiday_dates, holiday_types, test_holiday_types)

  ! Create feature matrices
  write(*, '(A)') 'Creating feature matrices...'
  call create_feature_matrix(train_weekdays, train_months, train_years, train_holiday_types, train_features)
  call create_feature_matrix(test_weekdays, test_months, test_years, test_holiday_types, test_features)

  ! Fit regression and predict
  write(*, '(A)') 'Fitting regression model...'
  call fit_linear_regression(train_features, log_sales, coefficients)
  predictions = matmul(test_features, coefficients)

  ! Save results
  write(*, '(A)') 'Saving predictions...'
  call save_predictions(test_ids, predictions, submission_file)

  write(*, '(A)') 'Analysis complete!'

  ! Cleanup
  deallocate(train_dates, test_dates, holiday_dates)
  deallocate(train_weekdays, test_weekdays)
  deallocate(train_months, train_years, test_months, test_years)
  deallocate(holiday_types, train_holiday_types, test_holiday_types)
  deallocate(train_features, test_features, coefficients, predictions)
  deallocate(log_sales, test_ids)

contains

subroutine read_train_data(filename, dates, sales, num_rows)
    character(len=*), intent(in) :: filename
    character(len=10), allocatable, intent(out) :: dates(:)
    real(real64), allocatable, intent(out) :: sales(:)
    integer, intent(out) :: num_rows
    integer :: unit_num, io_stat, valid_rows, comma_pos
    character(len=1000) :: line
    character(len=100) :: date_str, sales_str
    real(real64) :: sale_value
    logical, allocatable :: valid_mask(:)
    character(len=10), allocatable :: temp_dates(:)
    real(real64), allocatable :: temp_sales(:)

    ! First pass: Count total rows
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

    ! Second pass: Read data and validate sales
    open(newunit=unit_num, file=filename, status='old', action='read')
    read(unit_num, '(A)')  ! Skip header

    do i = 1, num_rows
      ! Read the entire line
      read(unit_num, '(A)', iostat=io_stat) line
      if (io_stat /= 0) cycle

      ! Find comma position
      comma_pos = index(line, ',')
      if (comma_pos > 0) then
        ! Split line into date and sales
        date_str = trim(adjustl(line(:comma_pos-1)))
        sales_str = trim(adjustl(line(comma_pos+1:)))

        ! Skip if sales string is empty
        if (len_trim(sales_str) > 0) then!cycle
          !write(*,*) "Line:", line
          !write(*,*) "Date:", date_str
          !write(*,*) "Sales:", sales_str
          ! Try to convert sales string to number
          read(sales_str, *, iostat=io_stat) sale_value
          ! write(*,*) "Sales:", sales_str
          ! if (len_trim(sales_str) >= 0) then
          ! if (io_stat == 0) then ! .and. sale_value >= 0.0 .and. len_trim(sales_str) >= 0) then
          !write(*,*) "Line:", line
          !write(*,*) "Date:", date_str
          !write(*,*) "Sales:", sales_str
          temp_dates(i) = date_str
          temp_sales(i) = log(sale_value)
          valid_mask(i) = .true.
          valid_rows = valid_rows + 1
          ! end if
        end if
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

    write(*, '(A,I0)') 'Number of valid observations: ', num_rows

  end subroutine read_train_data

  subroutine read_test_data(filename, dates, ids, num_rows)
    character(len=*), intent(in) :: filename
    character(len=10), allocatable, intent(out) :: dates(:)
    integer, allocatable, intent(out) :: ids(:)
    integer, intent(out) :: num_rows
    integer :: unit_num, io_stat
    character(len=1000) :: line

    open(newunit=unit_num, file=filename, status='old', action='read', iostat=io_stat)
    if (io_stat /= 0) then
      write(*, '(A)') 'Error: Unable to open test file'
      stop
    end if

    num_rows = -1  ! Skip header
    do
      read(unit_num, '(A)', iostat=io_stat) line
      if (io_stat /= 0) exit
      num_rows = num_rows + 1
    end do
    close(unit_num)

    allocate(dates(num_rows), ids(num_rows))

    open(newunit=unit_num, file=filename, status='old', action='read')
    read(unit_num, '(A)')  ! Skip header
    do i = 1, num_rows
      read(unit_num, *, iostat=io_stat) ids(i), dates(i)
      if (io_stat /= 0) then
        write(*, '(A,I0)') 'Error reading test data at line ', i
        stop
      end if
    end do
    close(unit_num)

    write(*, '(A,I0,A)') 'Read ', num_rows, ' test observations'
  end subroutine read_test_data

  subroutine read_holiday_data(filename, dates, types, countries, num_rows)
    character(len=*), intent(in) :: filename
    character(len=10), allocatable, intent(out) :: dates(:)
    character(len=20), allocatable, intent(out) :: types(:)
    character(len=2), allocatable, intent(out) :: countries(:)
    integer, intent(out) :: num_rows
    integer :: unit_num, io_stat
    character(len=1000) :: line

    open(newunit=unit_num, file=filename, status='old', action='read', iostat=io_stat)
    if (io_stat /= 0) then
      write(*, '(A)') 'Error: Unable to open holiday file'
      stop
    end if

    num_rows = -1  ! Skip header
    do
      read(unit_num, '(A)', iostat=io_stat) line
      if (io_stat /= 0) exit
      num_rows = num_rows + 1
    end do
    close(unit_num)

    allocate(dates(num_rows), types(num_rows), countries(num_rows))

    open(newunit=unit_num, file=filename, status='old', action='read')
    read(unit_num, '(A)')  ! Skip header
    do i = 1, num_rows
      read(unit_num, *, iostat=io_stat) dates(i), countries(i), types(i)
      if (io_stat /= 0) then
        write(*, '(A,I0)') 'Error reading holiday data at line ', i
        stop
      end if
    end do
    close(unit_num)

    write(*, '(A,I0,A)') 'Read ', num_rows, ' holiday records'
  end subroutine read_holiday_data

  subroutine process_dates(dates, weekdays, months, years)
    character(len=10), intent(in) :: dates(:)
    character(len=3), allocatable, intent(out) :: weekdays(:)
    integer, allocatable, intent(out) :: months(:), years(:)
    integer :: i, year, month, day, iostat
    character(len=3), parameter :: days(7) = ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun']

    allocate(weekdays(size(dates)))
    allocate(months(size(dates)))
    allocate(years(size(dates)))

    do i = 1, size(dates)
      ! Parse date (assuming YYYY-MM-DD format)
      ! write(*,*) "Date:", dates(i)
      !read(dates(i)(1:4), *) years(i)
      !read(dates(i)(6:7), *) months(i)
      !read(dates(i)(9:10), *) day

      ! Calculate weekday
      ! weekdays(i) = days(calculate_weekday(years(i), months(i), day))
      !end do

      !do i = 1, size(dates)
      ! Check for empty date string
      if (len_trim(dates(i)) == 0) then
        write(*,*) "Warning: Empty date string encountered at index ", i
        cycle
      endif

      ! Extract year, month, and day
      read(dates(i)(1:4), *, iostat=iostat) year
      !write(*,*) "Year:", year
      if (iostat /= 0) then
        write(*,*) "Error: Failed to read year from date:", dates(i)
        cycle
      endif

      read(dates(i)(6:7), '(I2)', iostat=iostat) month
      !write(*,*) ", Month:", month
      if (iostat /= 0) then
        write(*,*) "Error: Failed to read month from date:", dates(i)
        cycle
      endif

      read(dates(i)(9:10), '(I2)', iostat=iostat) day
      !write(*,*) ", Day:", day
      if (iostat /= 0) then
        write(*,*) "Error: Failed to read day from date:", dates(i)
        cycle
      endif

      ! Validate date components (optional)
      if (month < 1 .or. month > 12) then
        write(*,*) "Warning: Invalid month in date:", dates(i)
        cycle
      endif

      if (day < 1 .or. day > 31) then
        write(*,*) "Warning: Invalid day in date:", dates(i)
        cycle
      endif

    ! Calculate weekday
      weekdays(i) = days(calculate_weekday(years(i), months(i), day))

       ! Print extracted values for debugging
      write(*,*) "Date:", dates(i), ", Year:", year, ", Month:", month, ", Day:", day


    enddo
  end subroutine process_dates



  function calculate_weekday(year, month, day) result(weekday)
    integer, intent(in) :: year, month, day
    integer :: weekday, y, m

    if (month <= 2) then
      m = month + 12
      y = year - 1
    else
      m = month
      y = year
    end if

    weekday = modulo(day + ((13*(m+1))/5) + y + (y/4) - (y/100) + (y/400), 7) + 1
  end function calculate_weekday

  subroutine merge_holidays(dates, country, holidays_dates, holiday_types, merged_types)
    character(len=10), intent(in) :: dates(:)
    character(len=2), intent(in) :: country
    character(len=10), intent(in) :: holidays_dates(:)
    character(len=20), intent(in) :: holiday_types(:)
    character(len=20), allocatable, intent(out) :: merged_types(:)
    integer :: i, j

    allocate(merged_types(size(dates)))
    merged_types = 'None'  ! Default value

    do i = 1, size(dates)
      do j = 1, size(holidays_dates)
        if (dates(i) == holidays_dates(j)) then
          merged_types(i) = holiday_types(j)
          exit
        end if
      end do
    end do
  end subroutine merge_holidays

  subroutine create_feature_matrix(weekdays, months, years, holiday_types, features)
    character(len=3), intent(in) :: weekdays(:)
    integer, intent(in) :: months(:), years(:)
    character(len=20), intent(in) :: holiday_types(:)
    real(real64), allocatable, intent(out) :: features(:,:)
    integer :: n_rows, n_cols, i

    n_rows = size(weekdays)
    n_cols = 7 + 12 + 1 + 5  ! weekdays + months + year + holiday types

    allocate(features(n_rows, n_cols))
    features = 0.0_real64

    do i = 1, n_rows
      ! One-hot encode weekdays
      select case (weekdays(i))
        case ('Mon')
          features(i,1) = 1.0_real64
        case ('Tue')
          features(i,2) = 1.0_real64
        case ('Wed')
          features(i,3) = 1.0_real64
        case ('Thu')
          features(i,4) = 1.0_real64
        case ('Fri')
          features(i,5) = 1.0_real64
        case ('Sat')
          features(i,6) = 1.0_real64
        case ('Sun')
          features(i,7) = 1.0_real64
      end select

      ! One-hot encode months
      features(i,7+months(i)) = 1.0_real64

      ! Year (centered)
      features(i,20) = real(years(i) - 2020, real64)

      ! Holiday types
      select case (holiday_types(i))
        case ('Public holiday')
          features(i,21) = 1.0_real64
        case ('Local holiday')
          features(i,22) = 1.0_real64
        case ('Observance')
          features(i,23) = 1.0_real64
        case ('Local observance')
          features(i,24) = 1.0_real64
        case ('None')
          features(i,25) = 1.0_real64
      end select
    end do
  end subroutine create_feature_matrix

  subroutine fit_linear_regression(X, y, coefficients)
    real(real64), intent(in) :: X(:,:), y(:)
    real(real64), allocatable, intent(out) :: coefficients(:)
    real(real64), allocatable :: XtX(:,:), Xty(:)
    integer :: n, p

    n = size(X, 1)
    p = size(X, 2)

    allocate(XtX(p,p), Xty(p), coefficients(p))

    ! Calculate X'X and X'y
    XtX = matmul(transpose(X), X)
    Xty = matmul(transpose(X), y)

    ! Solve system of equations
    call solve_linear_system(XtX, Xty, coefficients)
  end subroutine fit_linear_regression

  subroutine solve_linear_system(A, b, x)
    real(real64), intent(inout) :: A(:,:)
    real(real64), intent(inout) :: b(:)
    real(real64), intent(out) :: x(:)
    integer :: n, i, j, k
    real(real64) :: factor

    n = size(A, 1)

    ! Forward elimination
    do k = 1, n-1
      do i = k+1, n
        factor = A(i,k) / A(k,k)
        A(i,k+1:n) = A(i,k+1:n) - factor * A(k,k+1:n)
        b(i) = b(i) - factor * b(k)
      end do
    end do

    ! Back substitution
    x(n) = b(n) / A(n,n)
    do i = n-1, 1, -1
      x(i) = (b(i) - sum(A(i,i+1:n) * x(i+1:n))) / A(i,i)
    end do
  end subroutine solve_linear_system

  subroutine save_predictions(test_ids, predictions, filename)
    integer, intent(in) :: test_ids(:)
    real(real64), intent(in) :: predictions(:)
    character(len=*), intent(in) :: filename
    integer :: unit_num, i

    open(newunit=unit_num, file=filename, status='replace', action='write')
    write(unit_num, '(A)') 'id,num_sold'  ! Header
    do i = 1, size(test_ids)
      write(unit_num, '(I0,",",F15.6)') test_ids(i), exp(predictions(i))
    end do
    close(unit_num)
  end subroutine save_predictions

end program sales_analysis
