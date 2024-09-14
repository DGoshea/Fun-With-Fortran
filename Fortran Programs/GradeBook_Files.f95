!--------------------------------------------------------------------
!GradeBook in Fortran by Divine Goshea
!Use a file to get the number of students, and their respective grades
!Get user input for the names of students
!Calculate grade avg for each student and the course
!Display grades, low scores, high scores, and good students
!---------------------------------------------------------------------


program GradeBook_File 
  implicit none 
!------------------------------------------------------------- 
  !Variables 
  integer :: num_students, i, j, good_students
  integer, parameter :: num_grades = 4
  integer, allocatable, dimension(:,:) :: grades
  real :: totalsum, course_average
  real, allocatable, dimension(:) :: averages
  integer, dimension(num_grades) :: high_grades, low_grades
  character(len=20), allocatable, dimension(:) :: students
  logical, allocatable, dimension(:) :: is_good_student
  
!------------------------------------------------------------- 
  !Read the number of students from file grades.dat

  open(unit = 10, file = "grades.dat", action = 'read', status = 'old')
  read (10,*) num_students

!------------------------------------------------------------- 
  !Allocate the grades, students, averages, and good students arrays
  allocate(students(num_students))
  allocate(grades(num_students, num_grades))
  allocate(averages(num_students))
  allocate(is_good_student(num_students))

!------------------------------------------------------------- 
  !Loop to name each student

  do i = 1, num_students
    print*, 'What is the name of student:', i
    read(*,*) students(i)
  end do

!------------------------------------------------------------- 
  !Loop to read grades into grade matrix 

  do i = 1, num_students 
    do j = 1, num_grades
       read(10,*) grades(i,j)
    end do
  end do
  close(10)

!------------------------------------------------------------- 
  !Get student averages 

  do i = 1, num_students
    averages(i) = sum(grades(i,:)) / num_grades !Sums all four of each students grades
  end do

!------------------------------------------------------------- 
  !Get Calculate Course Average

  totalsum = sum(averages)
  course_average = totalsum / num_students

!------------------------------------------------------------- 
  !Find Highest and Lowest Grades 

  do j = 1, num_grades
    high_grades(j) = maxval(grades(:,j))
    low_grades(j) = minval(grades(:,j))
  end do 

!------------------------------------------------------------- 
  !Identify students with average equal to or above course average
  good_students = 0
  do i = 1, num_students
    if (averages(i) >= course_average) then
      is_good_student(i) = .true.
      good_students = good_students + 1
    else
      is_good_student(i) = .false.
    end if
  end do

!------------------------------------------------------------- 
  !Print The Results & Deallocate Arrays

  print*, "Student Grades and Averages:"
  do i = 1, num_students
    print '(A, A, A, 4I4, A, F6.2)', "Student: ", students(i), "Grades:", grades(i,:), " Average: ", averages(i)
  end do
  print *, "----------------------------------------------"
  print '(A, F6.2)', "Course Average:", course_average
  print *, "----------------------------------------------"
  print *, "Highest Grades In Order Of Assignment: ", high_grades
  print *, "Lowest Grades In Order Of Assignment: ", low_grades
  print *, "Students with average equal to or above course average:"
  do i = 1, num_students
    if (is_good_student(i)) then
      print '(A, A, A, F6.2)', "Student: ", students(i), " Average: ", averages(i)
    end if
  end do

!Deallocate Arrays
  deallocate(grades)
  deallocate(averages)
  deallocate(students)
  deallocate(is_good_student)

end program GradeBook_File 