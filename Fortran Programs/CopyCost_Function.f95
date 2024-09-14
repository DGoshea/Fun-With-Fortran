program CopyCost
  implicit none
  character(len=50) :: name
  real :: numofcopies !number of copies 
  real :: price !cost of copies 
  real :: compute_copy_cost !function

  print *, 'Enter your name: '
  read (*,*) name
  print *, 'Enter the number of copies you would like to print:'
  read (*,*) numofcopies 

  !Call the function 
  price = compute_copy_cost(numofcopies)

  !Display the results 
  print *, 'Name: ', name
  write(*, '(A,F6.2)') ' Total Cost: $', price

end program CopyCost

real function compute_copy_cost(a)
  real, intent(in) :: a
  if (a < 200) then 
    compute_copy_cost = a * 0.12
  else 
    compute_copy_cost = 200 * 0.12 + (a - 200) * 0.06
  end if 
end function compute_copy_cost