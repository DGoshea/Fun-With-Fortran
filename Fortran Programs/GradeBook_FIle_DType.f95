!--------------------------------------------------------------------
!GradeBook in Fortran by Divine Goshea (modified)
!Use a file to get the number of students, and their respective grades
!Get user input for the names of students
!Calculate grade avg for each student and the course
!Display grades, low scores, high scores, and good students
!Instead of the use of arrays 
!---------------------------------------------------------------------

program GradeBook_File 
  implicit none 

  !Variables and Derived Type: Student_Type
  integer :: num_students, i, j
  integer, parameter :: num_grades = 4
  real :: totalsum, course_average
  integer, dimension(num_grades) :: high_grades, low_grades
  
  type Student_Type
    character(len=20) :: name
    integer, dimension(num_grades) :: grades
    real :: average
    logical :: is_good_student
  end type Student_Type
  
  type(Student_Type), allocatable, dimension(:) :: students

!------------------------------------------------------------- 
  !Read the number of students from file grades.dat

  open(unit = 10, file = "grades.dat", action = 'read', status = 'old')
  read (10,*) num_students

!------------------------------------------------------------- 
  !Allocate the students array
  allocate(students(num_students))

!------------------------------------------------------------- 
  !Loop to name each student and read grades

  do i = 1, num_students
    print*, 'What is the name of student:', i
    read(*,*) students(i)%name
    
    do j = 1, num_grades
       read(10,*) students(i)%grades(j)
    end do
    
    !Calculate student average
    students(i)%average = sum(students(i)%grades) / real(num_grades)
  end do
  close(10)

!------------------------------------------------------------- 
  !Calculate Course Average

  totalsum = sum(students%average)
  course_average = totalsum / num_students

!------------------------------------------------------------- 
  !Find Highest and Lowest Grades for eahc assignment

    do j = 1, num_grades 
    !Figure out way to set grades for each assignment??
    !Four high and low grades 8 total
    high_grades(j) = maxval(students%grades(j))
    low_grades(j) = minval(students%grades(j))
  end do  

!------------------------------------------------------------- 
  !Identify students with average equal to or above course average
  do i = 1, num_students
    if (students(i)%average >= course_average) then
      students(i)%is_good_student = .true.
    else
      students(i)%is_good_student = .false.
    end if
  end do

!------------------------------------------------------------- 
  !Print The Results & Deallocate Array

  print*, "Student Grades and Averages:"
  do i = 1, num_students
    print '(A, A, A, 4I4, A, F6.2)', "Student: ", students(i)%name, "Grades:", students(i)%grades, " Average: ", students(i)%average
  end do
  print *, "----------------------------------------------"
  print '(A, F6.2)', "Course Average:", course_average
  print *, "----------------------------------------------"
  print *, "Highest Grades In Order Of Assignment: ", high_grades
  print *, "Lowest Grades In Order Of Assignment: ", low_grades
  print *, "Students with average equal to or above course average:"
  do i = 1, num_students
    if (students(i)%is_good_student) then
      print '(A, A, A, F6.2)', "Student: ", students(i)%name, " Average: ", students(i)%average
    end if
  end do

!Deallocate Array
  deallocate(students)

end program GradeBook_File