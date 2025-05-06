program sales_prediction
    use csv_module
    implicit none

    type(csv_file) :: train_file, test_file, output_file
    character(len=30), dimension(:), allocatable :: header_train, header_test
    character(len=10), dimension(:), allocatable :: date_train, date_test
    character(len=100), dimension(:), allocatable :: country_train, country_test, store_train, store_test, product_train, product_test
    real(8), dimension(:), allocatable :: num_sold_train, log_num_sold_train

	! New variables for storing IDs from test set and their predictions
	integer:: id_tmp(1000)
	real(8):: predictions(1000)

	integer::nrows_id,nrow_pred

	integer::nrows_train,nrows_test,i,j

	! Open the training CSV file and read headers
	call train_file%read('train.csv', header_row=1)
	call train_file%get_header(header_train)

	! Count valid rows for allocation in training data
	nrows_train = size(header_train)

	! Allocate arrays for training data
	allocate(date_train(nrows_train))
	allocate(country_train(nrows_train))
	allocate(store_train(nrows_train))
	allocate(product_train(nrows_training))
	allocate(num_sold_training(nrow_training))
	allocate(log_num_sold_training(nrow_training))

	! Read training data and process it
	nrow_training =0
	do i=1,nrow_training
	    read(header_training(i),'(A10,A10,A20,A20,A20,A20)')
			id_tmp(id),
			date_training(id),
			country_training(id),
			store_training(id),
			product_training(id),
			num_sold_tmp(id)

	    if(num_sold_tmp(id)/='') then
	        num_sold_training(id)=real(num_sold_tmp(id))
	        log_num_sold_training(id)=log(num_sold_training(id))
	        nrow_training=nrow_training+1
	    end if
	end do

	! Close the training file after reading
	call train_file%destroy()

	! Open the test CSV file and read headers
	call test_file%read('test.csv', header_row=1)
	call test_file%get_header(header_testing)

	! Count valid rows for allocation in test data
	nrow_testing=size(header_testing)

	! Allocate arrays for test data
	allocate(date_testing(nrow_testing))
	allocate(country_testing(nrow_testing))
	allocate(store_testing(nrow_testing))
	allocate(product_testing(nrow_testing))

	! Read test data (assuming similar structure)
	nrow_testing=0
	do i=1,nrow_testing
	    read(header_testing(i),'(A10,A10,A20,A20,A20)')
			id_tmp(id),
			date_testing(id),
			country_testing(id),
			store_testing(id),
			product_testing(id)
	    nrow_testing=nrow_testing+1
	end do

	! Close the test file after reading
	call test_file%destroy()

	! Prepare to store predictions for the test set.
	predictions=zeros(real,nrow_testing)

	! Loop through unique countries to fit models and predict values.
	character(len=100)::unique_countries(10)! Adjust size as needed.
	integer::unique_country_count

	unique_country_count=get_unique_countries(country_training ,unique_countries)

	do j=1 ,unique_country_count
	    character(len=100)::current_country
	    current_country=unique_countries(j)

	    integer::country_rows_count
	    country_rows_count=count_rows_for_country(current_country ,country_training )

	    real(8)::log_num_sold_current(country_rows_count ),store_current(country_rows_count )
	    character(len=100)::product_current(country_rows_count )

	    call fill_country_data(current_country ,log_num_sold_current ,store_current ,product_current )

	    real(8)::intercept,slope_store_product
	    call fit_linear_regression(log_num_sold_current ,store_current ,product_current ,intercept ,slope_store_product )

	    predictions=predict_sales(test_data_array(current_country ),intercept ,slope_store_product )
	end do

	call output_file%open('predictions.csv' ,status_ok=status_ok )
	call output_file%add(['id' ,'predicted_num_sold'])
	call output_file%next_row()

	do i=1,nrow_testing
	    call output_file%add([id_tmp(i ) ,predictions(i )])
	    call output_file%next_row()
	end do

	call output_file%close(status_ok )

	print*,'Predictions saved to predictions.csv'

end program sales_prediction


integer function get_unique_countries(countries_array ,unique_countries )
	implicit none

	character(len=100 ),dimension(:)::countries_array
	character(len=100 ),dimension(:)::unique_countries

	integer::i,j,count

	count=0

	do i=1,size(countries_array )
		logical::found
		found=.false.

		do j=1,count
			if(countries_array(i)==unique_countries(j)) then
				found=.true.
				exit
			end if
		end do

		if(.not.found ) then
			count=count+1
			unique_countries(count)=countries_array(i )
		end if
	end do

	get_unique_countries=count

end function get_unique_countries


integer function count_rows_for_country(country_name,countries_array )
	implicit none

	character(len=100)::country_name
	character(len=100 ),dimension(:)::countries_array

	integer::i,count

	count=0

	do i=1,size(countries_array )
		if(countries_array(i)==country_name ) then
			count=count+1
		end if
	end do

	count_rows_for_country=count

end function count_rows_for_country


subroutine fill_country_data(country_name ,log_num_sold_current ,store_current ,product_current )
	implicit none

	character(len=100)::country_name

	real(8 ),dimension(:)::log_num_sold_current
	character(len=100 ),dimension(:)::store_current ,product_current

	integer::i,k

	k=0

	do i=1,size(country_name )
		if(country_name==country_name ) then
			k=k+1
			log_num_sold_current(k)=log_num_sold_training(i )
			store_current(k)=store_training(i )
			product_current(k)=product_training(i )
		end if

	end do

end subroutine fill_country_data


subroutine fit_linear_regression(log_num_sold_data ,store_data ,
                                  intercept,slope_store_product )

	implicit none

	real(8 ),dimension(:)::log_num_sold_data
	character(len=100 ),dimension(:)::store_data
	integer::n,i,j,k

	n=size(log_num_sold_data )

	double precision::sum_x,sum_y,sum_xy,sum_x2,sum_y2

	sum_x=sum_y=sum_xy=sum_x2=sum_y2=0.0d0

	do i=1,n

	   sum_x=sum_x+real(store_data(i ))
	   sum_y=sum_y+log_num_sold_data(i )
	   sum_xy=sum_xy+real(store_data(i ))*log_num_sold_data(i )
	   sum_x2=sum_x2+real(store_data(i ))**2
	   sum_y2=sum_y2+(log_num_sold_data(i ))**2
	end do

	slope_store_product=(n*sum_xy-sum_x*sum_y)/(n*sum_x2-sum_x**2 )

	intercept=sum_y/n-slope_store_product*(sum_x/n )

end subroutine fit_linear_regression


real(8 ) function predict_sales(test_data_array,current_country ,
                               intercept,slope_store_product )

implicit none

character(len=100)::current_country

real(8)::intercept,slope_store_product

integer::i,n

n=size(test_data_array )

do i=1,n

	predict_sales=(intercept+slope_store_product*real(test_data_array(current_country )) )

end function predict_sales


