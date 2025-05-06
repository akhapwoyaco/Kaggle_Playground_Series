program sales_prediction
    implicit none

    ! Variable declarations
    character(len=256) :: line
    character(len=256) :: filename_in, filename_out
    integer :: ios, iunit_in, iunit_out
    integer :: nrows_train, nrows_test, i, j, k, country_count
    character(len=10), dimension(:), allocatable :: date_train, date_test
    character(len=100), dimension(:), allocatable :: country_train, store_train, product_train, country_test, store_test, product_test
    real(8), dimension(:), allocatable :: num_sold_train, log_num_sold_train, log_num_sold_current, store_current
    integer, dimension(:), allocatable :: id_test
    real(8), dimension(:), allocatable :: predictions
    real(8) :: intercept, slope
    character(len=100), dimension(:), allocatable :: unique_countries
    character(len=100), dimension(:), allocatable :: product_current

    ! File names
    filename_in = 'train.csv'
    filename_out = 'predictions.csv'

    ! --- Read Training Data ---
    open(newunit=iunit_in, file=filename_in, status='old', action='read', iostat=ios)
    if (ios /= 0) stop "Error opening input file"
    nrows_train = -1
    do while (.true.)
        read(iunit_in, '(A)', iostat=ios) line
        if (ios /= 0) exit
        nrows_train = nrows_train + 1
    end do
    rewind(iunit_in)
    allocate(date_train(nrows_train), country_train(nrows_train), store_train(nrows_train), product_train(nrows_train))
    allocate(num_sold_train(nrows_train), log_num_sold_train(nrows_train))
    read(iunit_in, '(A)') line ! Skip header
    do i = 1, nrows_train
        read(iunit_in, '(A)', iostat=ios) line
        if (ios == 0) then
            read(line, '(A10,A100,A100,A100,F8.0)', iostat=ios) date_train(i), country_train(i), store_train(i), product_train(i), num_sold_train(i)
            if (ios == 0 .and. num_sold_train(i) > 0) then
                log_num_sold_train(i) = log(num_sold_train(i))
            else
                print *, "Error reading line or invalid num_sold in train data:", line
            end if
        else
            print *, "Error reading line in train data:", line
        end if
    end do
    close(iunit_in)

    ! --- Read Test Data ---
    open(newunit=iunit_out, file='test.csv', status='old', action='read', iostat=ios)
    if (ios /= 0) stop "Error opening test file"
    nrows_test = -1
    do while (.true.)
        read(iunit_out, '(A)', iostat=ios) line
        if (ios /= 0) exit
        nrows_test = nrows_test + 1
    end do
    rewind(iunit_out)
    allocate(date_test(nrows_test), country_test(nrows_test), store_test(nrows_test), product_test(nrows_test), id_test(nrows_test), predictions(nrows_test))
    read(iunit_out, '(A)') line ! Skip header
    do i = 1, nrows_test
        read(iunit_out, '(A)', iostat=ios) line
        if (ios == 0) then
            read(line, '(I10,A10,A100,A100,A100)', iostat=ios) id_test(i), date_test(i), country_test(i), store_test(i), product_test(i)
        else
            print *, "Error reading line in test data:", line
        end if
    end do
    close(iunit_out)

    predictions = 0.0_real8

    ! --- Prediction Logic (Linear Regression per Country) ---
    num_unique_countries = get_unique_countries(country_train, unique_countries)

    do j = 1, num_unique_countries
        allocate(log_num_sold_current(0), store_current(0), product_current(0))
        call fill_country_data(unique_countries(j), country_train, log_num_sold_train, store_train, product_train, log_num_sold_current, store_current, product_current, country_count)

        if (size(log_num_sold_current) > 1) then ! Only fit if there's more than one data point
            call fit_linear_regression(log_num_sold_current, store_current, intercept, slope)

            do i = 1, nrows_test
                if (country_test(i) == unique_countries(j)) then
                    k = find_store_index(store_test(i), store_current)
                    if (k > 0) then
                        predictions(i) = exp(intercept + slope * real(k))
                    else
                        print *, "Warning: No store found in training data for test entry ", i, " (Country: ", unique_countries(j), ", Store: ", store_test(i), ")"
                        predictions(i) = exp(intercept) ! Default to intercept if no store match
                    end if
                end if
            end do
        else
            print *, "Warning: Not enough training data (<=1) for country ", unique_countries(j)
        end if

        deallocate(log_num_sold_current, store_current, product_current)
    end do

    ! --- Write Predictions to File ---
    open(newunit=iunit_out, file=filename_out, status='replace', action='write', iostat=ios)
    if (ios /= 0) stop "Error opening output file"

    write(iunit_out, '(A,A)') 'ID', 'Predicted_Num_Sold'
    do i = 1, nrows_test
        write(iunit_out,'(I10,F10.2)') id_test(i), predictions(i)
    end do

    close(iunit_out)

    print *, 'Predictions saved to', filename_out

contains

    integer function get_unique_countries(countries_array, unique_countries) result(count)
        implicit none
        character(len=100), dimension(:), intent(in) :: countries_array
        character(len=100), dimension(:), allocatable, intent(out) :: unique_countries
        integer :: i, j, count

        count = 0
        allocate(unique_countries(size(countries_array)))
        do i = 1, size(countries_array)
            do j = 1, count
                if (countries_array(i) == unique_countries(j)) then
                    cycle
                end if
            end do
            count = count + 1
            unique_countries(count) = countries_array(i)
        end do
        if (count > 0) then
            deallocate(unique_countries(count+1:size(countries_array)))
        end if
    end function get_unique_countries

    integer function count_rows_for_country(country_name, countries_array) result(count)
        implicit none
        character(len=100), intent(in) :: country_name
        character(len=100), dimension(:), intent(in) :: countries_array
        integer :: i, count

        count = 0
        do i = 1, size(countries_array)
            if (countries_array(i) == country_name) then
                count = count + 1
            end if
        end do
    end function count_rows_for_country

    subroutine fill_country_data(country_name, country_train, log_num_sold_train, store_train, product_train, log_num_sold_current, store_current, product_
        implicit none
        character(len=100), intent(in) :: country_name
        character(len=100), dimension(:), intent(in) :: country_train, store_train, product_train
        real(8), dimension(:), intent(in) :: log_num_sold_train
        real(8), dimension(:), allocatable, intent(out) :: log_num_sold_current
        character(len=100), dimension(:), allocatable, intent(out) :: store_current, product_current
        integer :: i, count, k

        count = count_rows_for_country(country_name, country_train)
        if (count > 0) then
            allocate(log_num_sold_current(count), store_current(count), product_current(count))
            k = 1
            do i = 1, size(country_train)
                if (country_train(i) == country_name) then
                    log_num_sold_current(k) = log_num_sold_train(i)
                    store_current(k) = store_train(i)
                    product_current(k) = product_train(i)
                    k = k + 1
                end if
            end do
        end if
    end subroutine fill_country_data

    subroutine fit_linear_regression(y, x, intercept, slope)
        implicit none
        real(8), dimension(:), intent(in) :: y, x
        real(8), intent(out) :: intercept, slope
        integer :: n, i
        real(8) :: sum_x, sum_y, sum_xy, sum_x2

        n = size(y)
        if (n /= size(x)) error stop "Input arrays must have the same size"

        sum_x = 0.0_real8
        sum_y = 0.0_real8
        sum_xy = 0.0_real8
        sum_x2 = 0.0_real8

        do i = 1, n
            sum_x = sum_x + real(find_store_index(x(i),x))
            sum_y = sum_y + y(i)
            sum_xy = sum_xy + real(find_store_index(x(i),x)) * y(i)
            sum_x2 = sum_x2 + real(find_store_index(x(i),x))**2
        end do

        if (n*sum_x2 - sum_x**2 == 0.0) then
            slope = 0.0_real8
            intercept = sum_y / n
            print *, "Warning: Division by zero in linear regression. Setting slope to 0."
        else
            slope = (n*sum_xy - sum_x*sum_y) / (n*sum_x2 - sum_x**2)
            intercept = (sum_y - slope * sum_x) / n
        end if
    end subroutine fit_linear_regression

    integer function find_store_index(store_name, store_array) result(index)
        implicit none
        character(len=100), intent(in) :: store_name
        character(len=100), dimension(:), intent(in) :: store_array
        integer :: i

        index = 0
        do i = 1, size(store_array)
            if (store_name == store_array(i)) then
                index = i
                exit
            end if
        end do
    end function find_store_index

end program sales_prediction
