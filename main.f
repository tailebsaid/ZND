		PROGRAM init

!		Declaration des variables utilisée 

		IMPLICIT NONE

		integer :: nb_periodes_initial, nb_periodes_final 
		integer :: nb_periodes_max, nb_periodes_sec, nb_periodes 

		real :: temps_elapsed 

		INTEGER*4 :: ierr, i, np, kim,id,j
		INTEGER*4, PARAMETER :: len_default = 50
		INTEGER*4, PARAMETER :: unit_input = 1, unit_input_bis = 2

		real*8 :: Lx,dx,n_proc,bx,nn
		real*8, dimension(:), allocatable :: V_kim
		real*8, dimension(:), allocatable :: x,rho,p,u,Y,T,M,ii,dotu,q
		real*8, dimension(:,:), allocatable :: mat

		CHARACTER (len_default), PARAMETER :: file_input  = "zndmax.dat"
		CHARACTER (len_default), PARAMETER :: file_output = "output.dat"

!		Initialisation 

		Lx = 6.40d-3
		n_proc = 8.0d0

		dx = 9.079d-6

		bx = Lx/n_proc
		kim = bx/dx 

		i = 0 

!               Initialisations 

		call system_clock(count_rate = nb_periodes_sec, count_max = 
     &          nb_periodes_max) 
		call system_clock(count=nb_periodes_initial)

!		Initialisation de la matrice contenant le block. 



	 	OPEN (unit=unit_input, file=file_input, form = "formatted"
     &     , iostat = ierr)


		IF (ierr /= 0) THEN 

			STOP
			WRITE(*,*) "ERROR allocate : in Lecture"
		
		END IF 

		DO 

			i = i+1

			READ(unit_input,*, iostat = ierr) q		
        
        	IF (ierr .LT. 0) exit 

		ENDDO

		CLOSE(unit_input)


		np = i

		allocate(dotu(1:np), stat =ierr)
		allocate(ii(1:np), stat =ierr)
		allocate(M(1:np), stat =ierr)
		allocate(mat(1:kim,1:5), stat =ierr)
		allocate(p(1:np), stat =ierr)
		allocate(q(1:np), stat =ierr)
		allocate(rho(1:np), stat =ierr)
		allocate(T(1:np), stat =ierr)
		allocate(u(1:np), stat =ierr)
		allocate(V_kim(1:kim), stat = ierr) 
		allocate(x(1:np), stat =ierr)
		allocate(Y(1:np), stat =ierr)

		IF (ierr /= 0) THEN 

			STOP
			WRITE(*,*) "ERROR allocate : in Lecture"
		
		END IF 

		mat(:,:) = 0
		
		write(*,*) "Dimension de la matrice mat suivant x",
     &		size(mat,dim = 1) 

		write(*,*) "Dimension de la matrice mat suivant y", 
     &          size(mat,dim = 2)
 
      	OPEN (unit=unit_input, file=file_input, form = "formatted"
     &     , iostat = ierr)
        
		IF (ierr /= 0) THEN 

			STOP
		WRITE(*,*) "ERROR allocate : in Lecture"
		
		END IF 

		Do i = 1, np

		READ(unit_input,*,iostat = ierr) x(i),rho(i),p(i),u(i),Y(i),T(i),	
     &     dotu(i),q(i)


        	IF (ierr .LT. 0) exit 


		END DO
		CLOSE(unit_input)


		V_kim = (/(i*dx,i=0,kim)/)

!     	x = dotu 


		DO i = 1,kim 
		DO j = 1,size(x)

            nn = V_kim(i) + (V_kim(i+1)-V_kim(i))/500

 			IF (x(j) .GE. V_kim(i) .AND. x(j) .LT. nn) THEN 

				mat(i,1) = V_kim(i)
				mat(i,2) = rho(j)
				mat(i,3) = p(j)
				mat(i,4) = u(j)
				mat(i,5) = Y(j)

			ENDIF

		END DO
		END DO 


		WRITE(*,*) 'kim = ', V_kim(kim)
		WRITE(*,*) 'V_kim = ', V_kim(3)
		WRITE(*,*)'mat = ', mat(3,1)		
		WRITE(*,*)'rho = ', rho(3) 
		WRITE(*,*)'x = ', x(2)
		WRITE(*,*) 'tailel de Mat', shape(mat)

!		 size(mat,dim=1)

      	OPEN (unit=unit_input_bis, file=file_output, form = "formatted"
     &     , iostat = ierr)

     	IF (ierr /= 0)	THEN
     		STOP 
     		WRITE(*,*) "ERROR in opening : file_output"
     	END IF 


     	DO i=1, kim

     		WRITE(unit_input_bis,*) mat(i,1), mat(i,2), mat(i,3), mat(i,4) 
     &   	, mat(i,5)

     	END DO

     	CLOSE(unit_input_bis)

	write(*,*) "nb_periodes_initial", nb_periodes_initial
	write(*,*) "nb_periodes_final", nb_periodes_final

	call system_clock(count=nb_periodes_final)

!	if (nb_periodes_final < nb_periodes_initial) 
	
	nb_periodes =  nb_periodes + nb_periodes_max 

	temps_elapsed = real(nb_periodes) / nb_periodes_sec
	write(*,*) "temps_elapsed : ", temps_elapsed


















































		END PROGRAM
