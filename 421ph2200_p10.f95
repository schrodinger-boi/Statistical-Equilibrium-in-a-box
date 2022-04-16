program statprob1
    implicit none 
    integer :: N,c,i,y0,k, m, i0,l,sum,count_left=0, count_right=0,flag=0
    real :: x,y,z, NLbar, diff
    real, dimension(:), allocatable :: box
    print*,"Enter the number of particles N"
    read*, N
    print*,"Enter the number of iterations required"
    read*, c
    allocate(box(1:N))

    open(unit=1,file="statdata.txt",action="write")
    open(unit=4,file="i0.txt",action="write")
    open(unit=2,file="histo.txt",action="write")

    !%%% Initial Condition where all particles are in the left side of the box
    do i=1,N
        call random_number(x)
        if(x>0.5) then
            x=x-0.5
        endif
        box(i)=x
    enddo
     
    !%%% Particles allowed to diffuse freely
    do i=1,c
        !%%% Integer random number generator to pick particles randomly
        call random_number(y)
        y0=int((y*N)+1)

        !%%% Random number to assign new values to array elements, i.e to move particles from LHS to RHS or vice versa
        call random_number(z)

        !%%% If particle is in RHS of the box, sending it to LHS     
        if(box(y0)>0.5) then                
            if(z>0.5) then
                z=z/2
            endif
            box(y0)=z   
        
        !%%% If particle is in LHS of the box, sending it to RHS
        else
            if(z<=0.5) then
                z=z/2
                z=z+0.5
            endif
            box(y0)=z  
        endif

        !%%% Counting number of particles in LHS and RHS of the box
        do m=1,N
            if (box(m)>0.5) then
                count_right=count_right+1
            else
                count_left=count_left+1
            endif
        enddo
             
        !%%% Locating time when NL=N/2 for the 1st time - call it point 1
        if (count_left==(N/2)) then
            flag=1      !Flag remains equal to 1 once NL=N/2 occurs for the 1st time
            i0=i        !i0 notes the time units whenever NL=N/2
            write(unit=4,fmt=*) i0
        endif      

        !%%% Storing values of NL from point 1 in a new file
        if(flag==1) then
            write(unit=2,fmt=100) count_left
        endif

        !%%% Storing values of time, NL, NR & N/2 from time=0 till the end of simulation
        write(unit=1,fmt=101) i, count_left, count_right, N/2

        100 format(1x,I8)
        101 format(1x,I8,1x,I8,1x,I8,1xI8)

        count_right=0
        count_left=0  
    enddo
     
    close(unit=2)
    open(unit=2,file="histo.txt",action="read")
    close(unit=4)
    open(unit=4,file="i0.txt",action="read")

    !%%% Skipping a certain time interval from point 1 to locate actual equilibrium point, call it point equi
    do k=1,20
       read(unit=4,fmt=*)   !Skipping first 20 time units from point 1
       read(unit=2,fmt=*)   !Skipping first 20 NL values from point 1
    enddo  

    read(unit=4,fmt=*) l    !Noting the time when NL=N/2 for 21st time i.e. taken to be point equi

    !%%% Suming all NL values after point equi
    do i=1,(c-l)
       read(unit=2,fmt=100) count_left
       sum=count_left+sum
    enddo

    NLbar=real(sum)/(c-l)
    print*,"NLbar", NLbar

    open(unit=3,file="histo2.txt",action="write")
    rewind(unit=2)
    do k=1,20
        read(unit=2,fmt=*)   !Skipping first 20 NL values from point 1
    enddo 

    !%%% Calculating (NL-NLbar)/N
    do i=1,(c-l)
        read(unit=2,fmt=100) count_left
        diff=(count_left-NLbar)/N
        write(unit=3,fmt=102) diff
        102 format(1x,F8.4)  
        diff=0.0
    enddo
    !%%% File statdata for fluctuations
    !%%% File histo for average variations from equilibrium
    !%%% File histo2 for plot of (NL-NLbar)/N
end program statprob1