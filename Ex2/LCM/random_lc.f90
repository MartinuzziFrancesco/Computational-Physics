!
!     generation of pseudorandom numbers - linear congruential method
!
program random_lcm
  implicit  none
  !
  !     declaration of variables
  integer :: i, number, old, seed, x, a,m,c,j, period
  !
  !     supply initial values of some variables:
  !     seed:   to start; a:
  !     number: how many numbers we want to generate
  print*,'Use LCM: x_(i+1)=mod(a*x_i+c,m)'
  print*, 'Per calcolare il periodo generare almeno 2m numeri!'
  print*,'Insert seed (=x_0), a, m, c >'
  read (*,*) seed, a, m, c



  print*,' How many numbers do you want to generate ?'
  read(*,*)number
  !
  OPEN(unit=1, file="random.dat", status="replace", action="write")
  old = seed
  !
  do i = 1, number
     x = mod ((a*old+c), m)
     WRITE (unit=1,fmt=*) x
     old = x

     if (i==m)  j=x
     if (j==x .and. i>m)  then
       period = i - m
       print*,j,x,i,m,period
       exit
     end if
   end do
  close(1)
  print*,' data saved in random.dat'
  print*,"Periodo:",period
end program random_lcm
