! ********************************************************************************************************************************************************** !
!                                                                                                                                                            !
!                                                        Library of Useful Fortran Subroutines v1.1                                                          !
!                                                                        LINUX VERSION                                                                       !
!                                                                                                                                                            !
!                                                     by Gilberto Agostinho (gilbertohasnofb@gmail.com)                                                      !
!                                                                                                                                                            !
! ********************************************************************************************************************************************************** !
!                                                                                                                                                            !
! List of all subroutines and functions included in this library:                                                                                            !
!                                                                                                                                                            !
! INIT_RANDOM_SEED()                                                                                                                                         !
! SET_RANDOM_SEED()                                                                                                                                          !
! RANDOM_INT(integer,integer,integer)                                                                                                                        !
! LCASE(character)                                                                                                                                           !
! BUBBLE_SORT(integer)                                                                                                                                       !
! SLEEP_MS(integer)                                                                                                                                          !
! PERCENTAGE(real,real,real,real)                                                                                                                            !
! SERIALIZE(integer,integer,integer)                                                                                                                         !
! MtoF(integer)                                                                                                                                              !
! FtoM(real)                                                                                                                                                 !
! MtoL(integer)                                                                                                                                              !
! LtoM(character)                                                                                                                                            !
! RANDOM_INT_GAUSSIAN(integer,integer,integer,real,integer)                                                                                                  !
! FACTORIAL(integer)                                                                                                                                         !
! QUICKSORT(integer,integer,integer)                                                                                                                         !
! NDIGITS(integer)                                                                                                                                           !
! NDIGITS_16(integer)                                                                                                                                        !
! EXTRACT_DIGITS(integer,integer)                                                                                                                            !
! EXTRACT_DIGITS_16(integer,integer)                                                                                                                         !
! EXTRACT_SINGLE_DIGIT(integer,integer)                                                                                                                      !
! EXTRACT_SINGLE_DIGIT_16(integer,integer)                                                                                                                   !
! DEC2BIN(integer)                                                                                                                                           !
! DEC2BIN_16(integer)                                                                                                                                        !
! NDIGITSBIN(integer)                                                                                                                                        !
! RETROGRADE_VECTOR(integer)                                                                                                                                 !
! UNIQUE_ELEMENTS(integer)                                                                                                                                   !
!                                                                                                                                                            !
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

    seed = x + 37 * (/ (i - 1, i = 1, N) /)

    call RANDOM_SEED(put=seed)

    deallocate(seed)

  end subroutine INIT_RANDOM_SEED

 ! **********************************************************************************************************************************************************

   ! Subroutine SET_RANDOM_SEED: sets the random seed of the PRNG using an input integer
   subroutine SET_RANDOM_SEED(x)

     integer, intent(IN) :: x
     integer :: i, N
     integer, dimension(:), allocatable :: seed

     call RANDOM_SEED(size=N)
     allocate(seed(N))

     seed = x + 37 * (/ (i - 1, i = 1, N) /)

     call RANDOM_SEED(put=seed)

     deallocate(seed)

   end subroutine SET_RANDOM_SEED

  ! **********************************************************************************************************************************************************

  ! Subroutine RANDOM INTEGER
  subroutine RANDOM_INT(x,N,offset) ! i.e., if offset=0: generates an integer x between 0 and (N-1). If offset=/=0: generates an integer x between (offset) and ((N-1)+offset). There are always N possibilities.

    integer, intent(OUT) :: x
    integer, intent(IN), optional :: N, offset
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

    character (LEN=*), intent(INOUT) :: text_string
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
  real function MtoF(MIDI)

    integer, intent(IN) :: MIDI
    integer :: MIDIinterval

    MIDIinterval =  MIDI - 69 ! since A4 is reference
    MtoF = 440 * 2**(real(MIDIinterval) / 12)

  end function MtoF

! **********************************************************************************************************************************************************

  ! function frequency -> MIDI
  real function FtoM(freq)

    real, intent(IN) :: freq
    real :: freqInterval

    freqInterval = freq / 440. ! A4 is reference

    FtoM = 12 * ( log(freqInterval) / log(2.)) + 69

  end function FtoM

! **********************************************************************************************************************************************************

  ! MIDI number into LilyPond notation
  character (LEN=9) function MtoL(pitchM)

    integer, intent(IN) :: pitchM ! pitch in MIDI notation
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
  integer function LtoM(pitchL)

    character (LEN=*), intent(IN) :: pitchL ! pitch in LilyPond notation
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

  ! generates a random integer using a discrete gaussian distribution
  subroutine RANDOM_INT_GAUSSIAN(output,N,centre,standard_deviation,offset)

    integer, intent(OUT) :: output
    integer, intent(IN) :: N, centre
    integer, intent(IN), optional :: offset ! default = 0
    real, intent(IN) :: standard_deviation ! values in the range centre +- standard_deviation will have 68% of chance of being choosen, while values in the range centre +- 2*standard_deviation have 95% of chance
    real :: gauss, gauss_sum, x, normalization_factor
    integer :: i, offset_aux

    offset_aux = 0
    if (present(offset)) offset_aux = offset

    normalization_factor = 0.0
    do i=0,(N-1)
      gauss = exp( (real(i + offset_aux) - centre)**2 / (-2 * standard_deviation**2) )
      normalization_factor = normalization_factor + gauss
    enddo

    call RANDOM_NUMBER(x) ! random number satisfying 0 <= x < 1

    gauss_sum = 0.0
    do i=0,(N-1)
      gauss = exp( (real(i + offset_aux) - centre)**2 / (-2 * standard_deviation**2) ) / normalization_factor
      gauss_sum = gauss_sum + gauss
      if (x < gauss_sum) then
        output = i + offset_aux
        exit
      endif
    enddo

  end subroutine RANDOM_INT_GAUSSIAN

! **********************************************************************************************************************************************************

  ! factorial function
  integer function FACTORIAL(n)

    integer, intent(IN) :: n
    integer :: i

    FACTORIAL = 1

    do i=n,1,-1
      FACTORIAL = FACTORIAL * i
    enddo

  end function FACTORIAL

! **********************************************************************************************************************************************************

  ! Bubble Sort algorithm
  ! Adapted from https://gist.github.com/t-nissie/479f0f16966925fa29ea

  recursive subroutine QUICKSORT(vector, first_index, last_index)

    integer, intent(INOUT), dimension(:) :: vector
    integer, intent(IN) :: first_index, last_index
    integer :: pivot, temp
    integer :: i, j

    pivot = vector((first_index + last_index) / 2)
    i = first_index
    j = last_index

    do
      do while (vector(i) < pivot)
        i = i + 1
      end do
      do while (pivot < vector(j))
        j = j - 1
      end do
      if (i >= j) exit
      temp = vector(i)
      vector(i) = vector(j)
      vector(j) = temp
      i = i + 1
      j = j - 1
    enddo

    if (first_index < (i - 1)) call QUICKSORT(vector, first_index, (i - 1))
    if ((j + 1) < last_index)  call QUICKSORT(vector, (j + 1), last_index)

  end subroutine QUICKSORT

! **********************************************************************************************************************************************************

  ! returns the number of digits of an input integer
  integer function NDIGITS(input)

    integer, intent(IN) :: input
    real :: aux

    aux = abs(real(input)) ! abs is used so that negative numbers can also be input
    NDIGITS = floor(log10(aux)) + 1

  end function NDIGITS

! **********************************************************************************************************************************************************

  ! returns the number of digits of an input integer of kind 16
  integer function NDIGITS_16(input)

    integer (kind=16), intent(IN) :: input
    real :: aux

    aux = abs(real(input)) ! abs is used so that negative numbers can also be input
    NDIGITS_16 = floor(log10(aux)) + 1

  end function NDIGITS_16

! **********************************************************************************************************************************************************

  subroutine EXTRACT_DIGITS(input,output_vector)

    integer, intent(IN) :: input
    integer, dimension(:), intent(OUT) :: output_vector
    integer :: i, aux, num_digits

    output_vector = 0
    num_digits = NDIGITS(input)

    aux = input
    do i = 1,num_digits
      output_vector(i) = aux - (aux/10)*10
      aux = aux / 10
    enddo

  end subroutine EXTRACT_DIGITS

! **********************************************************************************************************************************************************

  subroutine EXTRACT_DIGITS_16(input,output_vector)

    integer (kind=16), intent(IN) :: input
    integer, dimension(:), intent(OUT) :: output_vector
    integer (kind=16) :: aux
    integer :: i, num_digits

    output_vector = 0
    num_digits = NDIGITS_16(input)

    aux = input
    do i = 1,num_digits
      output_vector(i) = aux - (aux/10)*10
      aux = aux / 10
    enddo

  end subroutine EXTRACT_DIGITS_16

! **********************************************************************************************************************************************************

  integer function EXTRACT_SINGLE_DIGIT(input,pos)

    integer, intent(IN) :: input, pos
    integer :: aux

    aux = input
    aux = aux / (10 ** (pos - 1))
    EXTRACT_SINGLE_DIGIT = aux - (aux/10)*10

  end function EXTRACT_SINGLE_DIGIT

! **********************************************************************************************************************************************************

  integer function EXTRACT_SINGLE_DIGIT_16(input,pos)

    integer (kind=16), intent(IN) :: input
    integer, intent(IN) :: pos
    integer (kind=16) :: aux

    aux = input
    aux = aux / (10_16 ** (pos - 1))
    EXTRACT_SINGLE_DIGIT_16 = aux - (aux / 10) * 10

  end function EXTRACT_SINGLE_DIGIT_16

! **********************************************************************************************************************************************************

  ! converts decimal to binary, output is a vector. Best to use together with NDIGITSBIN as to allocate the output_vector the correct size, avoiding trailing zeroes
  subroutine DEC2BIN(input,output_vector)

    integer, intent(IN) :: input
    integer, dimension(:), intent(OUT) :: output_vector
    integer :: aux
    integer :: counter

    output_vector = 0
    aux = input
    counter = 1

    do
      output_vector(counter) = mod(aux,2)
      aux = aux / 2
      counter = counter + 1
      if (aux == 0) exit
    enddo

  end subroutine DEC2BIN

! **********************************************************************************************************************************************************

  integer (kind=16) function DEC2BIN_16(input)

    integer (kind=16), intent(IN) :: input
    integer (kind=16) :: aux
    integer :: counter

    aux = input
    counter = 0
    DEC2BIN_16 = 0

    do
      DEC2BIN_16 = DEC2BIN_16 + mod(aux,2) * 10_16 ** counter
      aux = aux / 2
      counter = counter + 1
      if (aux == 0) exit
    enddo

  end function DEC2BIN_16

! **********************************************************************************************************************************************************

  ! returns the number of digits of a decimal input integer as if it was a binary number (e.g. 16 returns 5, since 16 = 10000 in binary, which has 5 digits)
  integer function NDIGITSBIN(input)

    integer, intent(IN) :: input
    integer :: aux

    aux = input
    NDIGITSBIN = 0

    do
      aux = aux / 2
      NDIGITSBIN = NDIGITSBIN + 1
      if (aux == 0) exit
    enddo

  end function NDIGITSBIN

! **********************************************************************************************************************************************************

  subroutine RETROGRADE_VECTOR(vector)

    integer, dimension(:), intent(INOUT) :: vector
    integer, dimension(:), allocatable :: vector_aux
    integer :: i

    allocate(vector_aux(SIZE(vector)))

    vector_aux = vector

    do i = 1, SIZE(vector)
      vector(i) = vector_aux(SIZE(vector) + 1 - i)
    enddo

  end subroutine RETROGRADE_VECTOR

! **********************************************************************************************************************************************************

  logical function UNIQUE_ELEMENTS(vector)

    integer, dimension(:), intent(IN) :: vector
    integer :: i, j

    UNIQUE_ELEMENTS = .TRUE.

    do i = 1, SIZE(vector)
      do j = i + 1, SIZE(vector)
        if (vector(i) == vector(j)) then
          UNIQUE_ELEMENTS = .FALSE.
          goto 10
        endif
      enddo
    enddo

    10 return

  end function UNIQUE_ELEMENTS

! **********************************************************************************************************************************************************

end module usefulLibrary
