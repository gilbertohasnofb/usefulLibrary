! ********************************************************************************************************************************************************** !
!																																																																														 !
!                                                        Library of Useful Fortran Subroutines v1.0                                                          !
!																																			  LINUX VERSION 																																			 !
!																																																																														 !
!                                                     by Gilberto Agostinho (gilbertohasnofb@gmail.com)                                                      !
!																																																																														 !
!                                                          (current version created on 25/05/2015)                                                           !
!																																																																														 !
! ********************************************************************************************************************************************************** !
!																																																																														 !
! List of all subroutines and functions included in this library:																																														 !
!																																																																														 !
! INIT_RANDOM_SEED()																																																																				 !												 				 
! RANDOM_INT(integer,integer,integer)																																																												 !																				 
! LCASE(character) 																																																																					 !																 
! BUBBLE_SORT(integer) 																																																																			 !																	 
! SLEEP_MS(integer)																																																																					 !																 
! PERCENTAGE(real,real,real,real)																																																					                   !
! SERIALIZE(integer,integer,integer)																																																												 !																					 
! MtoF(integer)                                                                                                                                              !
! FtoM(real)	                                                                                                                                               !
! MtoL(integer)                                                                                                                                              !
! LtoM(character)                                                                                                                                            !
!																																																																														 !
! ********************************************************************************************************************************************************** ! 

module usefulLibrary
implicit none

contains

! **********************************************************************************************************************************************************

	! Subroutine INIT_RANDOM_SEED: selects a random value for RANDOM_SEED by using the computer clock, which assures that RANDOM will output different numbers each run
	! Adapted from: http://fortranwiki.org/fortran/show/random_seed
	subroutine INIT_RANDOM_SEED()

		integer :: i, N, x
		integer, dimension(:), allocatable :: seed

		call SYSTEM_CLOCK(count=x)

		call RANDOM_SEED(size=N)
		allocate(seed(N))

		seed=x+37*(/ (i - 1, i = 1, n) /)

		call RANDOM_SEED(put=seed)

		deallocate(seed)

	end subroutine INIT_RANDOM_SEED

	! **********************************************************************************************************************************************************
		
	! Subroutine RANDOM INTEGER
	subroutine RANDOM_INT(x,N,offset) ! i.e., if offset=0: generates an integer x between 0 and (N-1). If offset=/=0: generates an integer x between (offset) and ((N-1)+offset). There are always N possibilities.

		integer, intent(OUT) :: x
		integer, intent(IN),optional :: N, offset
		integer :: N_AUX, offset_AUX
		real :: random_AUX

		! if called without N, N is set to 2 (i.e., two options: 0 and 1)
		N_AUX=2
		if (present(N)) N_AUX=N
		! if called without offset, offset is set to 0.
		offset_AUX=0
		if (present(offset)) offset_AUX=offset

		call RANDOM_NUMBER(random_AUX)
		x=floor(random_AUX*N_AUX) + offset_AUX ! i.e., random 3 = 1, 2 or 3      

	end subroutine RANDOM_INT

	! **********************************************************************************************************************************************************

	! Converts any word into lower case
	subroutine LCASE(text_string) 

		character (LEN=*) , intent(INOUT) :: text_string
		integer :: i, N, check

		N = LEN(text_string) 

		do i=1,N
			check = ICHAR(text_string(i:i)) 
			if (check >= 65 .AND. check < 90) then
				text_string(i:i) = CHAR(check+32)
			endif
		enddo

	end subroutine LCASE

	! **********************************************************************************************************************************************************

	! Bubble Sort algorithm
	! Adapted from http://rosettacode.org/wiki/Sorting_algorithms/Bubble_sort#Fortran

	subroutine BUBBLE_SORT(a)

		integer, intent(INOUT), dimension(:) :: a
		real :: aux
		integer :: i, j
		logical :: swapped

		do j = (SIZE(a)-1), 1, -1
			swapped = .FALSE.
			do i = 1, j
				if (a(i) > a(i+1)) then
				  aux = a(i)
				  a(i) = a(i+1)
				  a(i+1) = aux
				  swapped = .TRUE.
				endif
			enddo
			if (.NOT. swapped) exit
		enddo

	end subroutine BUBBLE_SORT


	! **********************************************************************************************************************************************************

	! SLEEP_MS sleeps for N milliseconds
	! Adapted from: http://stackoverflow.com/questions/6931846/sleep-in-fortran
	subroutine SLEEP_MS(rate)

		integer, intent(IN) :: rate ! desired sleep interval [ms]
		integer, dimension(8) :: t ! arguments for date_and_time
		integer :: ms1, ms2  ! start and end times [ms]

		call date_and_time(values=t)
		ms1 = (t(5)*3600 + t(6)*60 + t(7)) * 1000 + t(8)

		do ! check time:
			call date_and_time(values=t)
			ms2 = (t(5)*3600 + t(6)*60 + t(7)) * 1000 + t(8)
			if ((ms2 - ms1) >= rate) exit
		enddo

	end subroutine SLEEP_MS

	! **********************************************************************************************************************************************************

	! Calculates the percentage of a value in relation to a total (max). Can also take a argument min, which is subtracted from both total and value (thus calculating the percentage of a delta)
	subroutine PERCENTAGE(pct,value,max,min)

		real, intent(OUT) :: pct
		real, intent(IN) :: value, max
		real, optional, intent(IN) :: min

		if (.NOT. present(min)) then
			pct = value / max
			else
				pct = (value - min) / (max - min)
		endif

	end subroutine PERCENTAGE

	! **********************************************************************************************************************************************************

	! outputs a vector with numbers from 0 until vectorSize-1 (also accepts an offset) in random order, without repetition.
	subroutine SERIALIZE(vector,vectorSize,offset)

		integer, dimension(:), intent(OUT) :: vector
		integer, intent(IN) :: vectorSize
		integer, intent(IN), optional :: offset
		integer :: i, j, offset_AUX
		logical :: condition

		if (present(offset)) then
			offset_AUX = offset
			else
				offset_AUX = 0
		endif

		do i=1,vectorSize

			condition = .TRUE.

			do while (condition)
	
				call RANDOM_INT(vector(i),vectorSize,offset_AUX)
		
				if (i==1) then			
					condition = .FALSE. ! for first number, there is no repetition, so condition (which controls the do whole loop) is set to false
					else				
						condition = .FALSE. ! here condition is set to false unless the loop below finds a repeated number
						do j=1,(i-1)					
							if (vector(i)==vector(j)) then
								condition = .TRUE.
							endif
						enddo					
				endif
				
			enddo	
	
		enddo	

	end subroutine SERIALIZE

! **********************************************************************************************************************************************************

  ! function MIDI -> frequency
	function MtoF(MIDI)
	real :: MtoF
	integer :: MIDI, MIDIinterval
	
	MIDIinterval =  MIDI - 69 ! since A4 is reference
	MtoF = 440 * 2**(real(MIDIinterval) / 12)
	
	end function MtoF

! **********************************************************************************************************************************************************

  ! function frequency -> MIDI
	function FtoM(freq)
	real :: FtoM
	real :: freq, freqInterval
	
	freqInterval = freq / 440. ! A4 is reference			
	
	FtoM = 12 * ( log(freqInterval) / log(2.)) + 69
	
	end function FtoM
	
! **********************************************************************************************************************************************************

	! MIDI number into LilyPond notation 
	function MtoL(pitchM)

	integer :: pitchM ! pitch in MIDI notation
	character (LEN=9) :: MtoL ! pitch in LilyPond notation
	integer :: octave, pitchM_AUX
	
	octave = 0
	pitchM_AUX = pitchM
	do while (pitchM_AUX >= 12)
		pitchM_AUX = pitchM_AUX - 12
		octave = octave + 1
	enddo

	select case (pitchM_AUX)
		case (0)
		  MtoL = "c"
		case (1)
		  MtoL = "cis"
		case (2)
		  MtoL = "d"
		case (3)
		  MtoL = "dis"
		case (4)
		  MtoL  = "e"
		case (5)
		  MtoL = "f"
		case (6)
		  MtoL = "fis"
		case (7)
		  MtoL = "g"
		case (8)
		  MtoL = "aes"
		case (9)
		  MtoL = "a"
		case (10)
		  MtoL = "bes"
		case (11)
		  MtoL = "b"
	end select
	
	select case (octave)
		case(-1)
			MtoL = TRIM(MtoL)//",,,,,"
		case(0)
		  MtoL = TRIM(MtoL)//",,,,"
		case(1)
		  MtoL = TRIM(MtoL)//",,,"
		case(2)
		  MtoL = TRIM(MtoL)//",,"
		case(3)
		  MtoL = TRIM(MtoL)//","
		case(5)
		  MtoL = TRIM(MtoL)//"'"
		case(6)
		  MtoL = TRIM(MtoL)//"''"
		case(7)
		  MtoL = TRIM(MtoL)//"'''"
		case(8)
		  MtoL = TRIM(MtoL)//"''''"
		case(9)
		  MtoL = TRIM(MtoL)//"'''''"
	end select

	end function MtoL

! **********************************************************************************************************************************************************

	! Lilypond notation into MIDI number
	function LtoM(pitchL)

	character (LEN=*) :: pitchL ! pitch in LilyPond notation
	integer :: LtoM ! pitch in MIDI notation
	integer :: octave, i

	octave = 4 ! since the notes c d e ... b in Lilypond are = to 48 + pitch_class in MIDI
	do i=1,LEN(pitchL)
		if (pitchL(i:i) == "'") octave = octave + 1
		if (pitchL(i:i) == ",") octave = octave - 1
	enddo

	select case (pitchL(1:1))
		case ("c")
		  LtoM = 0
		case ("d")
		  LtoM = 2
		case ("e")
		  LtoM = 4
		case ("f")
		  LtoM = 5
		case ("g")
		  LtoM = 7
		case ("a")
		  LtoM = 9
		case ("b")
		  LtoM = 11
		case default
	end select

	if (pitchL(2:3)=="is") LtoM = LtoM + 1
	if (pitchL(2:3)=="es") LtoM = LtoM - 1
	LtoM = LtoM + 12 * octave

	end function LtoM

! **********************************************************************************************************************************************************

end module usefulLibrary
