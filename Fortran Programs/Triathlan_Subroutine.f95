!--------------------------
!The Triathlon Problem
!--------------------------
program Triathlon
  IMPLICIT NONE
  REAL:: cyclehours !num of hours cycled (205 calories/hr) 
  REAL:: runninghours !num of hours ran (455 calories/hr)
  REAL:: swimminghours !num of hours swam (295 calories/hr)
  REAL:: calsburned !number of calories burned 
  REAL:: weight !weight in pounds 

  print *, 'Enter the amount of hours cycled: '
  read *, cyclehours

  print *, 'Enter the amount of hours ran: '
  read *, runninghours 

  print *, 'Enter the amount of hours swam: '
  read *, swimminghours 

  print *, 'Cycle Hours: ', cyclehours
  print *, 'Run Hours: ', runninghours
  print *, 'Swim Hours: ', swimminghours

  !Call the subroutine to calc weight loss
  call calc_weight_loss(cyclehours, runninghours, swimminghours, calsburned, weight)

  !Display Results 
  print*, "The amount of calories burned is:", calsburned
  print*, "The weight loss in pounds is: ", weight

end program Triathlon

subroutine calc_weight_loss(a,b,c,d,e)
  real, intent(in):: a,b,c
  real, intent(out):: d,e
  d = (a * 205) + (b * 455) + (c * 295)
  e = d / 3805
end subroutine calc_weight_loss






