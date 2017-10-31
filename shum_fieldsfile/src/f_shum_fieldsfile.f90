! *********************************COPYRIGHT************************************
! (C) Crown copyright Met Office. All rights reserved.                       
! For further details please refer to the file LICENCE.txt                   
! which you should have received as part of this distribution.               
! *********************************COPYRIGHT************************************
!                                                                            
! This file is part of the UM Shared Library project.                        
!                                                                            
! The UM Shared Library is free software: you can redistribute it            
! and/or modify it under the terms of the Modified BSD License, as           
! published by the Open Source Initiative.                                   
!                                                                            
! The UM Shared Library is distributed in the hope that it will be           
! useful, but WITHOUT ANY WARRANTY; without even the implied warranty        
! of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           
! Modified BSD License for more details.                                     
!                                                                            
! You should have received a copy of the Modified BSD License                
! along with the UM Shared Library.                                          
! If not, see <http://opensource.org/licenses/BSD-3-Clause>.                 
!*******************************************************************************
! Description: Methods for reading and writing UM fieldsfiles.
!
MODULE f_shum_fieldsfile_mod

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE

USE f_shum_lookup_indices_mod, ONLY:                                           &
  lblrec, lbpack, lbegin, lbnrec, lbuser1

IMPLICIT NONE 

PRIVATE

PUBLIC :: f_shum_open_file,                                                    &
          f_shum_create_file,                                                  &
          f_shum_close_file,                                                   &
          f_shum_read_fixed_length_header,                                     &
          f_shum_read_integer_constants,                                       &
          f_shum_read_real_constants,                                          &
          f_shum_read_level_dependent_constants,                               &
          f_shum_read_row_dependent_constants,                                 &
          f_shum_read_column_dependent_constants,                              &
          f_shum_read_additional_parameters,                                   &
          f_shum_read_extra_constants,                                         &
          f_shum_read_temp_histfile,                                           &
          f_shum_read_compressed_index,                                        &
          f_shum_read_lookup,                                                  &
          f_shum_read_field_data

INTERFACE f_shum_read_field_data                                      
  MODULE PROCEDURE                                                             &
          f_shum_read_field_data_real64,                                       &
          f_shum_read_field_data_int64,                                        &
          f_shum_read_field_data_real32,                                       &
          f_shum_read_field_data_int32
END INTERFACE

!------------------------------------------------------------------------------!
! We're going to use the types from the ISO_C_BINDING module, since although   !
! the REALs aren't 100% guaranteed to correspond to the sizes we want to       !
! enforce, they should be good enough on the majority of systems.              !
!                                                                              !
! Additional protection for the case that FLOAT/DOUBLE do not conform to the   !
! sizes we expect is provided via the "precision_bomb" macro-file              !
!------------------------------------------------------------------------------!
  INTEGER, PARAMETER :: int64  = C_INT64_T
  INTEGER, PARAMETER :: int32  = C_INT32_T
  INTEGER, PARAMETER :: real64 = C_DOUBLE
  INTEGER, PARAMETER :: real32 = C_FLOAT                                       
!------------------------------------------------------------------------------!

INTEGER(KIND=int64), PARAMETER :: f_shum_fixed_length_header_len = 256
INTEGER(KIND=int64), PARAMETER :: f_shum_lookup_dim1_len = 64

INTEGER(KIND=int64), PARAMETER :: imdi = -32768

! Constants for positional elements of fixed length header
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_int_const_start          = 100
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_int_const_dim            = 101
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_real_const_start         = 105
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_real_const_dim           = 106
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_lev_dep_const_start      = 110
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_lev_dep_const_dim1       = 111
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_lev_dep_const_dim2       = 112
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_row_dep_const_start      = 115
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_row_dep_const_dim1       = 116
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_row_dep_const_dim2       = 117
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_col_dep_const_start      = 120
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_col_dep_const_dim1       = 121
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_col_dep_const_dim2       = 122
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_additional_const_start   = 125
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_additional_const_dim1    = 126
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_additional_const_dim2    = 127
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_extra_const_start        = 130
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_extra_const_dim          = 131
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_temp_histfile_start      = 135
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_temp_histfile_dim        = 136
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_comp_field_index1_start  = 140
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_comp_field_index1_dim    = 141
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_comp_field_index2_start  = 142
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_comp_field_index2_dim    = 143
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_comp_field_index3_start  = 144
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_comp_field_index3_dim    = 145
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_lookup_start             = 150
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_lookup_dim1              = 151
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_lookup_dim2              = 152
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_data_start               = 160 
INTEGER(KIND=int64), PARAMETER :: f_shum_flh_data_dim                 = 161

! A linked-list element object storing information on an open file; for each
! file we store an "id" that identifies it, a controlled copy of both the
! fixed length header and lookup and the path to the file
TYPE ff_type
  INTEGER(KIND=int64) :: unique_id
  INTEGER(KIND=int64) :: fixed_length_header(f_shum_fixed_length_header_len)
  INTEGER(KIND=int64), ALLOCATABLE :: lookup(:, :)
  CHARACTER(LEN=:),    ALLOCATABLE :: filename
  TYPE(ff_type), POINTER :: next => NULL()
  TYPE(ff_type), POINTER :: prev => NULL()
END TYPE ff_type

! An object to act as the access point and counter of the elements above
TYPE ff_list_type
  INTEGER(KIND=int64) :: length = 0
  TYPE(ff_type), POINTER :: head => NULL()
  TYPE(ff_type), POINTER :: tail => NULL()
END TYPE ff_list_type

TYPE(ff_list_type), SAVE :: ff_list

CONTAINS

!------------------------------------------------------------------------------!
! Creates a new ff object and assigns it a given filename and "id"
SUBROUTINE create_ff_type(id, filename, ff) 
IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(IN) :: id
CHARACTER(LEN=*),    INTENT(IN) :: filename
TYPE(ff_type),       INTENT(OUT), POINTER :: ff

ALLOCATE(ff)
ff % filename = TRIM(filename)
ff % unique_id = id
ff % fixed_length_header(:) = imdi

END SUBROUTINE create_ff_type

!------------------------------------------------------------------------------!
! Cleanup the memory used by an ff object when it is no longer needed
SUBROUTINE destroy_ff_type(ff) 
IMPLICIT NONE 

TYPE(ff_type), INTENT(INOUT), POINTER :: ff

DEALLOCATE(ff % lookup)
DEALLOCATE(ff % filename)
DEALLOCATE(ff)

END SUBROUTINE destroy_ff_type

!------------------------------------------------------------------------------!
! Insert an ff object onto the end of the list, updating the pointers
SUBROUTINE append_to_file_list(ff)
IMPLICIT NONE 

TYPE(ff_type), INTENT(INOUT), POINTER :: ff

NULLIFY(ff % next)
NULLIFY(ff % prev)

IF (ff_list % length == 0) THEN
  ! If the list is empty the ff object becomes the start and end of the list
  ff_list % head => ff
  ff_list % tail => ff
ELSE
  ! If the list is not empty the ff object is attached as the next link from
  ! the final member of the list, the current final member becomes the previous
  ! link of the ff object and then the ff object becomes the new final member
  ff_list % tail % next => ff
  ff % prev => ff_list % tail
  ff_list % tail => ff
END IF
ff_list % length = ff_list % length + 1

END SUBROUTINE append_to_file_list

!------------------------------------------------------------------------------!
! Remove a given ff object from the list, updating the pointers
SUBROUTINE remove_from_file_list(ff)
IMPLICIT NONE 

TYPE(ff_type), INTENT(INOUT), POINTER :: ff
TYPE(ff_type), POINTER :: ff_check

LOGICAL :: found

! First ensure the object is in the list in the first place
NULLIFY(ff_check)
found = .FALSE.
ff_check => ff_list % head
DO WHILE (ASSOCIATED(ff_check))
  IF (ff_check % unique_id == ff % unique_id) THEN
    found = .TRUE.
    EXIT
  END IF
  ff_check => ff_check % next
END DO

IF (found) THEN
  IF (ff_list % length > 1) THEN
    IF (.NOT. ASSOCIATED(ff % next)) THEN
      ! In the case that ff is the final member of the list, make the previous
      ! link the new final member and break its forward link (to ff)
      ff_list % tail => ff % prev
      NULLIFY(ff_list % tail % next)
    ELSE IF (.NOT. ASSOCIATED(ff % prev)) THEN
      ! In the case that ff is the first member of the list, make the next
      ! link the new first member and break its backward link (to ff)
      ff_list % head => ff % next
      NULLIFY(ff_list % head % prev)
    ELSE
      ! If ff is an intermediate member of the list, update the links before
      ! and after it to bypass it (by pointing to its own next/previous links)
      ff % prev % next => ff % next
      ff % next % prev => ff % prev
    END IF
  ELSE
    ! If this list only has one member left the list can just be reset
    NULLIFY(ff_list % head)
    NULLIFY(ff_list % tail)
  END IF
  ff_list % length = ff_list % length - 1
END IF

END SUBROUTINE remove_from_file_list

!------------------------------------------------------------------------------!
! This is the mechanism to retreive a pointer to a member of the list given
! its unique id
FUNCTION unique_id_to_ff(id) RESULT(ff)
IMPLICIT NONE 
INTEGER(KIND=int64), INTENT(IN) :: id
TYPE(ff_type),          POINTER :: ff

NULLIFY(ff)
ff => ff_list % head
DO WHILE (ASSOCIATED(ff))
  IF (ff % unique_id == id) THEN
    RETURN
  END IF
  ff => ff % next
END DO
! Note that the loop returns once the id is found; therefore if we reach this
! point we have failed to find the object we should clear the pointer to avoid
! returning the last element in the list
NULLIFY(ff)

END FUNCTION

!------------------------------------------------------------------------------!

FUNCTION f_shum_open_file(filename, ff_id, message) RESULT(status)
IMPLICIT NONE 

CHARACTER(LEN=*),    INTENT(IN)  :: filename
INTEGER(KIND=int64), INTENT(OUT) :: ff_id
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64)    :: status
TYPE(ff_type), POINTER :: ff
REAL(KIND=real64)      :: rand
LOGICAL                :: is_open

! TODO: Replace this with portio; for now we're just using intrinsic read
! and write methods, so the ff_id is just file unit; but we'll randomise it
! and keep it above a certain range to try and avoid clashes with genuine
! file units
is_open = .TRUE.
DO WHILE (is_open)
  CALL RANDOM_NUMBER(rand)
  ff_id = 50000 + FLOOR(10000*rand)
  INQUIRE(UNIT=ff_id, OPENED=is_open)
END DO

OPEN(UNIT=ff_id, ACCESS="STREAM", FORM="UNFORMATTED", FILE=filename,           &
                                                    IOSTAT=status, STATUS="OLD")
IF (status /= 0) THEN
  message = "Failed to open file"
  RETURN
END IF

! Register this file with the module
CALL create_ff_type(ff_id, filename, ff)
CALL append_to_file_list(ff)

! Extract the fixed length header and save it to the private array
status = f_shum_read_fixed_length_header(                                      &
                                       ff_id, ff % fixed_length_header, message)
IF (status /= 0) RETURN

! Extract the lookup and save it to the private array
status = f_shum_read_lookup(ff_id, ff % lookup, message)
IF (status /= 0) RETURN

END FUNCTION f_shum_open_file

!------------------------------------------------------------------------------!

FUNCTION f_shum_create_file(filename, n_lookups, ff_id, message, overwrite)    &
                                                                  RESULT(status)
IMPLICIT NONE 

CHARACTER(LEN=*),    INTENT(IN)  :: filename
INTEGER(KIND=int64), INTENT(IN)  :: n_lookups
INTEGER(KIND=int64), INTENT(OUT) :: ff_id
CHARACTER(LEN=*),    INTENT(OUT) :: message
LOGICAL, OPTIONAL,   INTENT(IN)  :: overwrite

INTEGER(KIND=int64)    :: status
TYPE(ff_type), POINTER :: ff
REAL(KIND=real64)      :: rand
LOGICAL                :: is_open
CHARACTER(LEN=7)       :: open_status

! TODO: Replace this with portio; for now we're just using intrinsic read
! and write methods, so the ff_id is just file unit; but we'll randomise it
! and keep it above a certain range to try and avoid clashes with genuine
! file units
is_open = .TRUE.
DO WHILE (is_open)
  CALL RANDOM_NUMBER(rand)
  ff_id = 50000 + FLOOR(10000*rand)
  INQUIRE(UNIT=ff_id, OPENED=is_open)
END DO

! Check if we can overwrite (default is that we can)
open_status = "REPLACE"
IF (PRESENT(overwrite)) THEN
  IF (.NOT. overwrite) THEN
    open_status = "NEW"
  END IF
END IF

OPEN(UNIT=ff_id, ACCESS="STREAM", FORM="UNFORMATTED", FILE=filename,           &
                                        IOSTAT=status, STATUS=TRIM(open_status))
IF (status /= 0) THEN
  message = "Failed to create file"
  RETURN
END IF

! Register this file with the module
CALL create_ff_type(ff_id, filename, ff)
CALL append_to_file_list(ff)

! Allocate the lookup array
ALLOCATE(ff % lookup(f_shum_lookup_dim1_len, n_lookups))

! Populate it with empty values
ff % lookup(1:45, :) = -99
ff % lookup(46:, :) = TRANSFER(0.0_real64, 0_int64)

! Set the lookup dimensions in the fixed_length_header
ff % fixed_length_header(f_shum_flh_lookup_dim1) = f_shum_lookup_dim1_len
ff % fixed_length_header(f_shum_flh_lookup_dim2) = n_lookups

END FUNCTION f_shum_create_file

!------------------------------------------------------------------------------!

FUNCTION f_shum_close_file(ff_id, message) RESULT(status)
IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(IN)  :: ff_id
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64) :: status

TYPE(ff_type), POINTER :: ff

ff => unique_id_to_ff(ff_id)

CALL remove_from_file_list(ff)
CALL destroy_ff_type(ff)

CLOSE(ff_id, IOSTAT=status)
IF (status /= 0) THEN
  message = "Failed to close file"
END IF

END FUNCTION f_shum_close_file

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_fixed_length_header(                                      &
                             ff_id, fixed_length_header, message) RESULT(status)
IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(IN)  :: ff_id
INTEGER(KIND=int64), INTENT(OUT) ::                                            &
                             fixed_length_header(f_shum_fixed_length_header_len)
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64) :: status

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
READ(ff_id, POS=1_int64, IOSTAT=status) fixed_length_header(:)
IF (status /= 0) THEN
  message = "Failed to read fixed_length_header"
  RETURN
END IF

END FUNCTION f_shum_read_fixed_length_header

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_integer_constants(                                        &
                               ff_id, integer_constants, message) RESULT(status)
IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(IN)                 :: ff_id
INTEGER(KIND=int64), INTENT(INOUT), ALLOCATABLE :: integer_constants(:)
CHARACTER(LEN=*),    INTENT(OUT)                :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: dim

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64

! Retrieve stored fixed length header
ff => unique_id_to_ff(ff_id)

! Get the start position and dimensions from the fixed_length_header
start = ff % fixed_length_header(f_shum_flh_int_const_start)
dim = ff % fixed_length_header(f_shum_flh_int_const_dim)

! Check that these make sense
IF ((start == imdi) .OR. (dim <= 0_int64)) THEN
  status = -1_int64
  message = "File does not specify any integer_constants"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first, 
! unless it happens to be exactly the right size already
IF (ALLOCATED(integer_constants) .AND. (SIZE(integer_constants) /= dim)) THEN
  DEALLOCATE(integer_constants, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed integer_constants"
    RETURN
  END IF
END IF
ALLOCATE(integer_constants(dim), STAT=status)
IF (status /= 0) THEN
  message = "Unable to allocate memory for integer_constants"
  RETURN
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" is inclusive of the given word (hence the "-1") and is in 64-bit 
! (8-byte) words but "POS" is in bytes (hence the "*8") and is offset by one
! byte so that "POS=1" is the start of the file (hence the "+1") 
READ(ff_id, POS=(start-1)*8+1, IOSTAT=status) integer_constants
IF (status /= 0) THEN
  message = "Failed to read integer_constants"
  RETURN
END IF

END FUNCTION f_shum_read_integer_constants

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_real_constants(                                           &
                                  ff_id, real_constants, message) RESULT(status)
IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(IN)                 :: ff_id
REAL(KIND=real64),   INTENT(INOUT), ALLOCATABLE :: real_constants(:)
CHARACTER(LEN=*),    INTENT(OUT)                :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: dim

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64

! Retrieve stored fixed length header
ff => unique_id_to_ff(ff_id)

! Get the start position and dimensions from the fixed_length_header
start = ff % fixed_length_header(f_shum_flh_real_const_start)
dim = ff % fixed_length_header(f_shum_flh_real_const_dim)

! Check that these make sense
IF ((start == imdi) .OR. (dim <= 0_int64)) THEN
  status = -1_int64
  message = "File does not specify any real_constants"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first, 
! unless it happens to be exactly the right size already
IF (ALLOCATED(real_constants) .AND. (SIZE(real_constants) /= dim)) THEN
  DEALLOCATE(real_constants, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed real_constants"
    RETURN
  END IF
END IF
ALLOCATE(real_constants(dim), STAT=status)
IF (status /= 0) THEN
  message = "Unable to allocate memory for real_constants"
  RETURN
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" is inclusive of the given word (hence the "-1") and is in 64-bit 
! (8-byte) words but "POS" is in bytes (hence the "*8") and is offset by one
! byte so that "POS=1" is the start of the file (hence the "+1") 
READ(ff_id, POS=(start-1)*8+1, IOSTAT=status) real_constants
IF (status /= 0) THEN
  message = "Failed to read real_constants"
  RETURN
END IF

END FUNCTION f_shum_read_real_constants

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_level_dependent_constants(                                &
                       ff_id, level_dependent_constants, message) RESULT(status)
IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(IN)     :: ff_id
REAL(KIND=real64),   INTENT(INOUT),                                            &
                     ALLOCATABLE    :: level_dependent_constants(:, :)
CHARACTER(LEN=*),    INTENT(OUT)    :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: dim1
INTEGER(KIND=int64) :: dim2

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64

! Retrieve stored fixed length header
ff => unique_id_to_ff(ff_id)

! Get the start position and dimensions from the fixed_length_header
start = ff % fixed_length_header(f_shum_flh_lev_dep_const_start)
dim1 = ff % fixed_length_header(f_shum_flh_lev_dep_const_dim1)
dim2 = ff % fixed_length_header(f_shum_flh_lev_dep_const_dim2)

! Check that these make sense
IF ((start == imdi) .OR. (dim1 <= 0_int64) .OR. (dim2 <= 0_int64)) THEN
  status = -1_int64
  message = "File does not specify any level_dependent_constants"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first, 
! unless it happens to be exactly the right size already
IF (ALLOCATED(level_dependent_constants)                                       &
    .AND. (SIZE(level_dependent_constants, 1) /= dim1)                         &
    .AND. (SIZE(level_dependent_constants, 2) /= dim2)) THEN
  DEALLOCATE(level_dependent_constants, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed level_dependent_constants"
    RETURN
  END IF
END IF
ALLOCATE(level_dependent_constants(dim1, dim2), STAT=status)
IF (status /= 0) THEN
  message = "Unable to allocate memory for level_dependent_constants"
  RETURN
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" is inclusive of the given word (hence the "-1") and is in 64-bit 
! (8-byte) words but "POS" is in bytes (hence the "*8") and is offset by one
! byte so that "POS=1" is the start of the file (hence the "+1") 
READ(ff_id, POS=(start-1)*8+1, IOSTAT=status) level_dependent_constants
IF (status /= 0) THEN
  message = "Failed to read level_dependent_constants"
  RETURN
END IF

END FUNCTION f_shum_read_level_dependent_constants

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_row_dependent_constants(                                  &
                         ff_id, row_dependent_constants, message) RESULT(status)
IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(IN)     :: ff_id
REAL(KIND=real64),   INTENT(INOUT),                                            &
                     ALLOCATABLE    :: row_dependent_constants(:, :)
CHARACTER(LEN=*),    INTENT(OUT)    :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: dim1
INTEGER(KIND=int64) :: dim2

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64

! Retrieve stored fixed length header
ff => unique_id_to_ff(ff_id)

! Get the start position and dimensions from the fixed_length_header
start = ff % fixed_length_header(f_shum_flh_row_dep_const_start)
dim1 = ff % fixed_length_header(f_shum_flh_row_dep_const_dim1)
dim2 = ff % fixed_length_header(f_shum_flh_row_dep_const_dim2)

! Check that these make sense
IF ((start == imdi) .OR. (dim1 <= 0_int64) .OR. (dim2 <= 0_int64)) THEN
  status = -1_int64
  message = "File does not specify any row_dependent_constants"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first, 
! unless it happens to be exactly the right size already
IF (ALLOCATED(row_dependent_constants)                                         &
    .AND. (SIZE(row_dependent_constants, 1) /= dim1)                           &
    .AND. (SIZE(row_dependent_constants, 2) /= dim2)) THEN
  DEALLOCATE(row_dependent_constants, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed row_dependent_constants"
    RETURN
  END IF
END IF
ALLOCATE(row_dependent_constants(dim1, dim2), STAT=status)
IF (status /= 0) THEN
  message = "Unable to allocate memory for row_dependent_constants"
  RETURN
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" is inclusive of the given word (hence the "-1") and is in 64-bit 
! (8-byte) words but "POS" is in bytes (hence the "*8") and is offset by one
! byte so that "POS=1" is the start of the file (hence the "+1") 
READ(ff_id, POS=(start-1)*8+1, IOSTAT=status) row_dependent_constants
IF (status /= 0) THEN
  message = "Failed to read row_dependent_constants"
  RETURN
END IF

END FUNCTION f_shum_read_row_dependent_constants

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_column_dependent_constants(                               &
                      ff_id, column_dependent_constants, message) RESULT(status)
IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(IN)     :: ff_id
REAL(KIND=real64),   INTENT(INOUT),                                            &
                     ALLOCATABLE    :: column_dependent_constants(:, :)
CHARACTER(LEN=*),    INTENT(OUT)    :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: dim1
INTEGER(KIND=int64) :: dim2

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64

! Retrieve stored fixed length header
ff => unique_id_to_ff(ff_id)

! Get the start position and dimensions from the fixed_length_header
start = ff % fixed_length_header(f_shum_flh_col_dep_const_start)
dim1 = ff % fixed_length_header(f_shum_flh_col_dep_const_dim1)
dim2 = ff % fixed_length_header(f_shum_flh_col_dep_const_dim2)

! Check that these make sense
IF ((start == imdi) .OR. (dim1 <= 0_int64) .OR. (dim2 <= 0_int64)) THEN
  status = -1_int64
  message = "File does not specify any column_dependent_constants"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first, 
! unless it happens to be exactly the right size already
IF (ALLOCATED(column_dependent_constants)                                      &
    .AND. (SIZE(column_dependent_constants, 1) /= dim1)                        &
    .AND. (SIZE(column_dependent_constants, 2) /= dim2)) THEN
  DEALLOCATE(column_dependent_constants, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed column_dependent_constants"
    RETURN
  END IF
END IF
ALLOCATE(column_dependent_constants(dim1, dim2), STAT=status)
IF (status /= 0) THEN
  message = "Unable to allocate memory for column_dependent_constants"
  RETURN
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" is inclusive of the given word (hence the "-1") and is in 64-bit 
! (8-byte) words but "POS" is in bytes (hence the "*8") and is offset by one
! byte so that "POS=1" is the start of the file (hence the "+1") 
READ(ff_id, POS=(start-1)*8+1, IOSTAT=status) column_dependent_constants
IF (status /= 0) THEN
  message = "Failed to read column_dependent_constants"
  RETURN
END IF

END FUNCTION f_shum_read_column_dependent_constants

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_additional_parameters(                                    &
                           ff_id, additional_parameters, message) RESULT(status)
IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(IN)     :: ff_id
REAL(KIND=real64),   INTENT(INOUT),                                            &
                     ALLOCATABLE    :: additional_parameters(:, :)
CHARACTER(LEN=*),    INTENT(OUT)    :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: dim1
INTEGER(KIND=int64) :: dim2

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64

! Retrieve stored fixed length header
ff => unique_id_to_ff(ff_id)

! Get the start position and dimensions from the fixed_length_header
start = ff % fixed_length_header(f_shum_flh_additional_const_start)
dim1 = ff % fixed_length_header(f_shum_flh_additional_const_dim1)
dim2 = ff % fixed_length_header(f_shum_flh_additional_const_dim2)

! Check that these make sense
IF ((start == imdi) .OR. (dim1 <= 0_int64) .OR. (dim2 <= 0_int64)) THEN
  status = -1_int64
  message = "File does not specify any additional_parameters"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first, 
! unless it happens to be exactly the right size already
IF (ALLOCATED(additional_parameters)                                      &
    .AND. (SIZE(additional_parameters, 1) /= dim1)                        &
    .AND. (SIZE(additional_parameters, 2) /= dim2)) THEN
  DEALLOCATE(additional_parameters, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed additional_parameters"
    RETURN
  END IF
END IF
ALLOCATE(additional_parameters(dim1, dim2), STAT=status)
IF (status /= 0) THEN
  message = "Unable to allocate memory for additional_parameters"
  RETURN
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" is inclusive of the given word (hence the "-1") and is in 64-bit 
! (8-byte) words but "POS" is in bytes (hence the "*8") and is offset by one
! byte so that "POS=1" is the start of the file (hence the "+1") 
READ(ff_id, POS=(start-1)*8+1, IOSTAT=status) additional_parameters
IF (status /= 0) THEN
  message = "Failed to read additional_parameters"
  RETURN
END IF

END FUNCTION f_shum_read_additional_parameters

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_extra_constants(                                          &
                                 ff_id, extra_constants, message) RESULT(status)
IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(IN)     :: ff_id
REAL(KIND=real64),   INTENT(INOUT),                                            &
                     ALLOCATABLE    :: extra_constants(:)
CHARACTER(LEN=*),    INTENT(OUT)    :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: dim

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64

! Retrieve stored fixed length header
ff => unique_id_to_ff(ff_id)

! Get the start position and dimensions from the fixed_length_header
start = ff % fixed_length_header(f_shum_flh_extra_const_start)
dim = ff % fixed_length_header(f_shum_flh_extra_const_dim)

! Check that these make sense
IF ((start == imdi) .OR. (dim <= 0_int64)) THEN
  status = -1_int64
  message = "File does not specify any extra_constants"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first, 
! unless it happens to be exactly the right size already
IF (ALLOCATED(extra_constants) .AND. (SIZE(extra_constants) /= dim)) THEN
  DEALLOCATE(extra_constants, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed extra_constants"
    RETURN
  END IF
END IF
ALLOCATE(extra_constants(dim), STAT=status)
IF (status /= 0) THEN
  message = "Unable to allocate memory for extra_constants"
  RETURN
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" is inclusive of the given word (hence the "-1") and is in 64-bit 
! (8-byte) words but "POS" is in bytes (hence the "*8") and is offset by one
! byte so that "POS=1" is the start of the file (hence the "+1") 
READ(ff_id, POS=(start-1)*8+1, IOSTAT=status) extra_constants
IF (status /= 0) THEN
  message = "Failed to read extra_constants"
  RETURN
END IF

END FUNCTION f_shum_read_extra_constants

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_temp_histfile(ff_id, temp_histfile, message) RESULT(status)
IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(IN)     :: ff_id
REAL(KIND=real64),   INTENT(INOUT),                                            &
                     ALLOCATABLE    :: temp_histfile(:)
CHARACTER(LEN=*),    INTENT(OUT)    :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: dim

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64

! Retrieve stored fixed length header
ff => unique_id_to_ff(ff_id)

! Get the start position and dimensions from the fixed_length_header
start = ff % fixed_length_header(f_shum_flh_temp_histfile_start)
dim = ff % fixed_length_header(f_shum_flh_temp_histfile_dim)

! Check that these make sense
IF ((start == imdi) .OR. (dim <= 0_int64)) THEN
  status = -1_int64
  message = "File does not specify any temp_histfile"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first, 
! unless it happens to be exactly the right size already
IF (ALLOCATED(temp_histfile) .AND. (SIZE(temp_histfile) /= dim)) THEN
  DEALLOCATE(temp_histfile, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed temp_histfile"
    RETURN
  END IF
END IF
ALLOCATE(temp_histfile(dim), STAT=status)
IF (status /= 0) THEN
  message = "Unable to allocate memory for temp_histfile"
  RETURN
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" is inclusive of the given word (hence the "-1") and is in 64-bit 
! (8-byte) words but "POS" is in bytes (hence the "*8") and is offset by one
! byte so that "POS=1" is the start of the file (hence the "+1") 
READ(ff_id, POS=(start-1)*8+1, IOSTAT=status) temp_histfile
IF (status /= 0) THEN
  message = "Failed to read temp_histfile"
  RETURN
END IF

END FUNCTION f_shum_read_temp_histfile

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_compressed_index(                                         &
                         ff_id, compressed_index, index, message) RESULT(status)
IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(IN)    :: ff_id
REAL(KIND=real64),   INTENT(INOUT),                                            &
                     ALLOCATABLE   :: compressed_index(:)
INTEGER(KIND=int64), INTENT(IN)    :: index
CHARACTER(LEN=*),    INTENT(OUT)   :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: dim

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64

! Retrieve stored fixed length header
ff => unique_id_to_ff(ff_id)

! Get the start position and dimensions from the fixed_length_header
SELECT CASE(index)
  CASE(1_int64)
    start = ff % fixed_length_header(f_shum_flh_comp_field_index1_start)
    dim = ff % fixed_length_header(f_shum_flh_comp_field_index1_dim)
  CASE(2_int64)
    start = ff % fixed_length_header(f_shum_flh_comp_field_index2_start)
    dim = ff % fixed_length_header(f_shum_flh_comp_field_index2_dim)
  CASE(3_int64)
    start = ff % fixed_length_header(f_shum_flh_comp_field_index3_start)
    dim = ff % fixed_length_header(f_shum_flh_comp_field_index3_dim)
  CASE DEFAULT
    status = 1_int64
    message = "Invalid compressed index requested, must be 1, 2 or 3"
    RETURN
END SELECT

! Check that these make sense
IF ((start == imdi) .OR. (dim <= 0_int64)) THEN
  status = -1_int64
  message = "File does not specify the requested compressed_index"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first, 
! unless it happens to be exactly the right size already
IF (ALLOCATED(compressed_index) .AND. (SIZE(compressed_index) /= dim)) THEN
  DEALLOCATE(compressed_index, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed compressed_index"
    RETURN
  END IF
END IF
ALLOCATE(compressed_index(dim), STAT=status)
IF (status /= 0) THEN
  message = "Unable to allocate memory for compressed_index"
  RETURN
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" is inclusive of the given word (hence the "-1") and is in 64-bit 
! (8-byte) words but "POS" is in bytes (hence the "*8") and is offset by one
! byte so that "POS=1" is the start of the file (hence the "+1") 
READ(ff_id, POS=(start-1)*8+1, IOSTAT=status) compressed_index
IF (status /= 0) THEN
  message = "Failed to read compressed_index"
  RETURN
END IF

END FUNCTION f_shum_read_compressed_index

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_lookup(ff_id, lookup, message) RESULT(status)
IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(IN)     :: ff_id
INTEGER(KIND=int64), INTENT(INOUT),                                            &
                     ALLOCATABLE    :: lookup(:, :)
CHARACTER(LEN=*),    INTENT(OUT)    :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: dim1
INTEGER(KIND=int64) :: dim2
INTEGER(KIND=int64) :: i

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64

! Retrieve stored fixed length header
ff => unique_id_to_ff(ff_id)

! Get the start position and dimensions from the fixed_length_header
start = ff % fixed_length_header(f_shum_flh_lookup_start)
dim1 = ff % fixed_length_header(f_shum_flh_lookup_dim1)
dim2 = ff % fixed_length_header(f_shum_flh_lookup_dim2)

! Check that these make sense
IF ((start == imdi) .OR. (dim1 <= 0_int64) .OR. (dim2 <= 0_int64)) THEN
  status = -1_int64
  message = "File does not specify any lookup table"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first, 
! unless it happens to be exactly the right size already
IF (ALLOCATED(lookup)                                                          &
    .AND. (SIZE(lookup, 1) /= dim1)                                            &
    .AND. (SIZE(lookup, 2) /= dim2)) THEN
  DEALLOCATE(lookup, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed lookup array"
    RETURN
  END IF
END IF
ALLOCATE(lookup(dim1, dim2), STAT=status)
IF (status /= 0) THEN
  message = "Unable to allocate memory for lookup array"
  RETURN
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" is inclusive of the given word (hence the "-1") and is in 64-bit 
! (8-byte) words but "POS" is in bytes (hence the "*8") and is offset by one
! byte so that "POS=1" is the start of the file (hence the "+1") 
READ(ff_id, POS=(start-1)*8+1, IOSTAT=status) lookup
IF (status /= 0) THEN
  message = "Failed to read lookup table"
  RETURN
END IF

! Check to see if the file appears to use indirect access; some older files
! don't use the positional elements and instead rely on the disk size and 
! field-order.  For simplicity, calculate the positional elements here and
! update the value of lbegin so that the returned lookup can always be used
! for direct access.
IF (ANY(lookup(lbegin,:) == 0_int64)) THEN
  lookup(lbegin, 1) = f_shum_flh_data_start
  DO i = 2, SIZE(lookup, 2)
    IF (lookup(lbnrec, i-1) > 0) THEN
      lookup(lbegin, i) = lookup(lbegin, i-1) + lookup(lbnrec, i-1) + 1
    END IF
  END DO
END IF

END FUNCTION f_shum_read_lookup

!------------------------------------------------------------------------------!

FUNCTION describe_allowed_kind_based_on_lbpack(lbpack) RESULT(description)

IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(IN)        :: lbpack
INTEGER(KIND=int64)                    :: lbpack_n1
INTEGER(KIND=int64), PARAMETER         :: prefix_len = 41
INTEGER(KIND=int64), PARAMETER         :: suffix_len = 40
CHARACTER(LEN=prefix_len)              :: prefix
CHARACTER(LEN=prefix_len + suffix_len) :: description

WRITE(prefix, "(A,I0,A)") "lookup item LBPACK=", lbpack, " indicates data"

lbpack_n1 = MOD(lbpack, 10_int64)

SELECT CASE (lbpack_n1)
  CASE (0_int64)
    ! Unpacked 64-bit data
    description = TRIM(prefix)//" will be 64-bit (Unpacked)"
  CASE (1_int64)
    ! WGDOS packed data
    description = TRIM(prefix)//" will be 32-bit INTEGER (WGDOS packed)"
  CASE (2_int64)
    ! Unpacked 32-bit data
    description = TRIM(prefix)//" will be 32-bit (Truncated)"
  CASE DEFAULT
    ! Unknown
    description = TRIM(prefix)//" is not supported by Shumlib"
END SELECT

END FUNCTION describe_allowed_kind_based_on_lbpack

!------------------------------------------------------------------------------!

FUNCTION describe_allowed_type_based_on_lbuser1(lbuser1) RESULT(description)

IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(IN)        :: lbuser1
INTEGER(KIND=int64), PARAMETER         :: prefix_len = 38
INTEGER(KIND=int64), PARAMETER         :: suffix_len = 31
CHARACTER(LEN=prefix_len)              :: prefix
CHARACTER(LEN=prefix_len + suffix_len) :: description

WRITE(prefix, "(A,I0,A)") "lookup item LBUSER1=", lbuser1, " indicates data"

SELECT CASE (lbuser1)
  CASE (1_int64)
    ! REAL data
    description = TRIM(prefix)//" will be REAL"
  CASE (2_int64)
    ! Integer data
    description = TRIM(prefix)//" will be INTEGER"
  CASE (3_int64)
    ! Logical data (treated as integer)
    description = TRIM(prefix)//" will be LOGICAL (as INTEGER)"
  CASE DEFAULT
    ! Unknown
    description = TRIM(prefix)//" is not supported by Shumlib"
END SELECT

END FUNCTION describe_allowed_type_based_on_lbuser1

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_field_data_real64(                                        &
                 ff_id, index, field_data, message, ignore_dtype) RESULT(status)
IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(IN)    :: ff_id
INTEGER(KIND=int64), INTENT(IN)    :: index
REAL(KIND=real64),   INTENT(INOUT),                                            &
                     ALLOCATABLE   :: field_data(:)
CHARACTER(LEN=*),    INTENT(OUT)   :: message
LOGICAL, OPTIONAL,   INTENT(IN)    :: ignore_dtype

LOGICAL                        :: ignore_dtype_local
INTEGER(KIND=int64)            :: status
INTEGER(KIND=int64)            :: start
INTEGER(KIND=int64)            :: len
INTEGER(KIND=int64)            :: pack_type
INTEGER(KIND=int64)            :: data_type
INTEGER(KIND=int64)            :: lookup(f_shum_lookup_dim1_len)

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64

! Retrieve stored lookup
ff => unique_id_to_ff(ff_id)

lookup = ff % lookup(:, index)

! Get the type of packing, and the data dimensions from the lookup
pack_type = lookup(lbpack)
start = lookup(lbegin)
len = lookup(lblrec)

! Pickup flag for ignoring data-type if present
ignore_dtype_local = .FALSE.
IF (PRESENT(ignore_dtype)) ignore_dtype_local = ignore_dtype

IF (ignore_dtype_local) THEN
  ! If the data type is being ignored, make any required adjustments to ensure
  ! the size of data being read in is correct; in this case the return array 
  ! is 64-bit, so any 32-bit types only require half the number of 64-bit words
  IF (MOD(pack_type, 10_int64) == 2) THEN
    len = (len + 1)/2
  END IF
ELSE
  ! If the data type isn't being ignored, first check the packing type will
  ! supply the data required to return this data (64-bit)
  IF (MOD(pack_type, 10_int64) /= 0) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between packing code in lookup and kind of field_data array "//&
      "(called routine with 64-bit REAL array, but "                         //&
      TRIM(describe_allowed_kind_based_on_lbpack(pack_type))//")"
    RETURN
  END IF
  ! And then check that the data type itself is correct
  data_type = lookup(lbuser1)
  IF (data_type /= 1) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between data-type code in lookup and type of field_data "    //&
      "array (called routine with REAL array, but "                          //&
      TRIM(describe_allowed_type_based_on_lbuser1(data_type))//")"
    RETURN
  END IF
END IF

! Check that the addressing info makes sense
IF ((start <= 0) .OR. (len <= 0_int64)) THEN
  status = -1_int64
  message = "Lookup does not contain addressing information"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first, 
! unless it happens to be exactly the right size already
IF (ALLOCATED(field_data) .AND. SIZE(field_data) /= len) THEN
  DEALLOCATE(field_data, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed field_data array"
    RETURN
  END IF
END IF

IF (.NOT. ALLOCATED(field_data)) THEN
  ALLOCATE(field_data(len), STAT=status)
  IF (status /= 0) THEN
    message = "Unable to allocate memory for field_data array"
    RETURN
  END IF
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" (LBEGIN here) is in 64-bit (8-byte) words but "POS" is in bytes 
! (hence the "*8") and is offset by one byte so that "POS=1" is the start of 
! the file (hence the "+1") 
READ(ff_id, POS=(start)*8+1, IOSTAT=status) field_data
IF (status /= 0) THEN
  message = "Failed to read field_data"
  RETURN
END IF

END FUNCTION f_shum_read_field_data_real64

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_field_data_int64(                                         &
                 ff_id, index, field_data, message, ignore_dtype) RESULT(status)
IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(IN)     :: ff_id
INTEGER(KIND=int64), INTENT(IN)     :: index
INTEGER(KIND=int64), INTENT(INOUT),                                            &
                     ALLOCATABLE    :: field_data(:)
CHARACTER(LEN=*),    INTENT(OUT)    :: message
LOGICAL, OPTIONAL,   INTENT(IN)     :: ignore_dtype

LOGICAL                          :: ignore_dtype_local
INTEGER(KIND=int64)              :: status
INTEGER(KIND=int64)              :: start
INTEGER(KIND=int64)              :: len
INTEGER(KIND=int64)              :: pack_type
INTEGER(KIND=int64)              :: data_type
INTEGER(KIND=int64)              :: lookup(f_shum_lookup_dim1_len)

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64

! Retrieve stored lookup
ff => unique_id_to_ff(ff_id)

lookup = ff % lookup(:, index)

! Get the type of packing, and the data dimensions from the lookup
pack_type = lookup(lbpack)
start = lookup(lbegin)
len = lookup(lblrec)

! Pickup flag for ignoring data-type if present
ignore_dtype_local = .FALSE.
IF (PRESENT(ignore_dtype)) ignore_dtype_local = ignore_dtype

IF (ignore_dtype_local) THEN
  ! If the data type is being ignored, make any required adjustments to ensure
  ! the size of data being read in is correct; in this case the return array 
  ! is 64-bit, so any 32-bit types only require half the number of 64-bit words
  IF (MOD(pack_type, 10_int64) == 2) THEN
    len = (len + 1)/2
  END IF
ELSE
  ! If the data type isn't being ignored, first check the packing type will
  ! supply the data required to return this data (64-bit)
  IF (MOD(pack_type, 10_int64) /= 0) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between packing code in lookup and kind of field_data array "//&
      "(called routine with 64-bit INTEGER array, but "                      //&
      TRIM(describe_allowed_kind_based_on_lbpack(pack_type))//")"
    RETURN
  END IF
  ! And then check that the data type itself is correct
  data_type = lookup(lbuser1)
  IF ((data_type /= 2) .AND. (data_type /= 3)) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between data-type code in lookup and type of field_data "    //&
      "array (called routine with INTEGER array, but "                       //&
      TRIM(describe_allowed_type_based_on_lbuser1(data_type))//")"
    RETURN
  END IF
END IF

! Check that the addressing info makes sense
IF ((start <= 0) .OR. (len <= 0_int64)) THEN
  status = -1_int64
  message = "Lookup does not contain addressing information"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first, 
! unless it happens to be exactly the right size already
IF (ALLOCATED(field_data) .AND. SIZE(field_data) /= len) THEN
  DEALLOCATE(field_data, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed field_data array"
    RETURN
  END IF
END IF

IF (.NOT. ALLOCATED(field_data)) THEN
  ALLOCATE(field_data(len), STAT=status)
  IF (status /= 0) THEN
    message = "Unable to allocate memory for field_data array"
    RETURN
  END IF
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" (LBEGIN here) is in 64-bit (8-byte) words but "POS" is in bytes 
! (hence the "*8") and is offset by one byte so that "POS=1" is the start of 
! the file (hence the "+1") 
READ(ff_id, POS=(start)*8+1, IOSTAT=status) field_data
IF (status /= 0) THEN
  message = "Failed to read field_data"
  RETURN
END IF

END FUNCTION f_shum_read_field_data_int64

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_field_data_real32(                                        &
                 ff_id, index, field_data, message, ignore_dtype) RESULT(status)
IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(IN)     :: ff_id
INTEGER(KIND=int64), INTENT(IN)     :: index
REAL(KIND=real32),   INTENT(INOUT),                                            &
                     ALLOCATABLE    :: field_data(:)
CHARACTER(LEN=*),    INTENT(OUT)    :: message
LOGICAL, OPTIONAL,   INTENT(IN)     :: ignore_dtype

LOGICAL                        :: ignore_dtype_local
INTEGER(KIND=int64)            :: status
INTEGER(KIND=int64)            :: start
INTEGER(KIND=int64)            :: len
INTEGER(KIND=int64)            :: pack_type
INTEGER(KIND=int64)            :: data_type
INTEGER(KIND=int64)            :: lookup(f_shum_lookup_dim1_len)

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64

! Retrieve stored lookup
ff => unique_id_to_ff(ff_id)

lookup = ff % lookup(:, index)

! Get the type of packing, and the data dimensions from the lookup
! Note: The data length reported for 32-bit truncated data is actually in 
!       32-bit words, so should be correct
pack_type = lookup(lbpack)
start = lookup(lbegin)
len = lookup(lblrec)
! The length reported for WGDOS packed fields is actually in 64-bit words,
! so to simplify the logic later, convert it to 32-bit words here
IF ((MOD(pack_type, 10_int64) == 1)) THEN
  len = len*2
END IF

! Pickup flag for ignoring data-type if present
ignore_dtype_local = .FALSE.
IF (PRESENT(ignore_dtype)) ignore_dtype_local = ignore_dtype

IF (ignore_dtype_local) THEN
  ! If the data type is being ignored, make any required adjustments to ensure
  ! the size of data being read in is correct; in this case the return array 
  ! is 32-bit, so any 64-bit types require double the number of 32-bit words
  IF (MOD(pack_type, 10_int64) /= 2) THEN
    len = 2*len
  END IF
ELSE  
  ! If the data type isn't being ignored, first check the packing type will
  ! supply the data required to return this data (64-bit)
  IF (MOD(pack_type, 10_int64) /= 2) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between packing code in lookup and kind of field_data array "//&
      "(called routine with 32-bit REAL array, but "                         //&
      TRIM(describe_allowed_kind_based_on_lbpack(pack_type))//")"
    RETURN
  END IF
  ! And then check that the data type itself is correct
  data_type = lookup(lbuser1)
  IF (data_type /= 1) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between data-type code in lookup and type of field_data "    //&
      "array (called routine with REAL array, but "                          //&
      TRIM(describe_allowed_type_based_on_lbuser1(data_type))//")"
    RETURN
  END IF
END IF

! Check that the addressing info makes sense
IF ((start <= 0) .OR. (len <= 0_int64)) THEN
  status = -1_int64
  message = "Lookup does not contain addressing information"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first, 
! unless it happens to be exactly the right size already
IF (ALLOCATED(field_data) .AND. SIZE(field_data) /= len) THEN
  DEALLOCATE(field_data, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed field_data array"
    RETURN
  END IF
END IF

IF (.NOT. ALLOCATED(field_data)) THEN
  ALLOCATE(field_data(len), STAT=status)
  IF (status /= 0) THEN
    message = "Unable to allocate memory for field_data array"
    RETURN
  END IF
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" (LBEGIN here) is in 64-bit (8-byte) words but "POS" is in bytes 
! (hence the "*8") and is offset by one byte so that "POS=1" is the start of 
! the file (hence the "+1") 
READ(ff_id, POS=(start)*8+1, IOSTAT=status) field_data
IF (status /= 0) THEN
  message = "Failed to read field_data"
  RETURN
END IF

END FUNCTION f_shum_read_field_data_real32

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_field_data_int32(                                         &
                 ff_id, index, field_data, message, ignore_dtype) RESULT(status)
IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(IN)     :: ff_id
INTEGER(KIND=int64), INTENT(IN)     :: index
INTEGER(KIND=int32), INTENT(INOUT),                                            & 
                     ALLOCATABLE    :: field_data(:)
CHARACTER(LEN=*),    INTENT(OUT)    :: message
LOGICAL, OPTIONAL,   INTENT(IN)     :: ignore_dtype

LOGICAL                          :: ignore_dtype_local
INTEGER(KIND=int64)              :: status
INTEGER(KIND=int64)              :: start
INTEGER(KIND=int64)              :: len
INTEGER(KIND=int64)              :: pack_type
INTEGER(KIND=int64)              :: data_type
INTEGER(KIND=int64)              :: lookup(f_shum_lookup_dim1_len)

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64

! Retrieve stored lookup
ff => unique_id_to_ff(ff_id)

lookup = ff % lookup(:, index)

! Get the type of packing, and the data dimensions from the lookup
pack_type = lookup(lbpack)
start = lookup(lbegin)
len = lookup(lblrec)
! The length reported for WGDOS packed fields is actually in 64-bit words,
! so to simplify the logic later, convert it to 32-bit words here
IF ((MOD(pack_type, 10_int64) == 1)) THEN
  len = len*2
END IF

! Pickup flag for ignoring data-type if present
ignore_dtype_local = .FALSE.
IF (PRESENT(ignore_dtype)) ignore_dtype_local = ignore_dtype

IF (ignore_dtype_local) THEN
  ! If the data type is being ignored, make any required adjustments to ensure
  ! the size of data being read in is correct; in this case the return array 
  ! is 32-bit, so any 64-bit types require double the number of 32-bit words
  IF ((MOD(pack_type, 10_int64) /= 2) .AND.                                    & 
      (MOD(pack_type, 10_int64) /= 1)) THEN
    len = len*2
  END IF
ELSE
  ! If the data type isn't being ignored, first check the packing type will
  ! supply the data required to return this data (64-bit)
  IF ((MOD(pack_type, 10_int64) /= 2) .AND.                                    &
      (MOD(pack_type, 10_int64) /= 1)) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between packing code in lookup and kind of field_data array "//&
      "(called routine with 32-bit INTEGER array, but "                      //&
      TRIM(describe_allowed_kind_based_on_lbpack(pack_type))//")"
    RETURN
  END IF
  ! And then check that the data type itself is correct
  data_type = lookup(lbuser1)
  IF ((MOD(pack_type, 10_int64) == 2) .AND.                                    &
      (data_type /= 2) .AND. (data_type /=3)) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between data-type code in lookup and type of field_data "    //&
      "array (called routine with INTEGER array, but "                       //&
      TRIM(describe_allowed_type_based_on_lbuser1(data_type))//")"
    RETURN
  END IF
END IF

! Check that the addressing info makes sense
IF ((start <= 0) .OR. (len <= 0_int64)) THEN
  status = -1_int64
  message = "Lookup does not contain addressing information"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first, 
! unless it happens to be exactly the right size already
IF (ALLOCATED(field_data) .AND. SIZE(field_data) /= len) THEN
  DEALLOCATE(field_data, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed field_data array"
    RETURN
  END IF
END IF

IF (.NOT. ALLOCATED(field_data)) THEN
  ALLOCATE(field_data(len), STAT=status)
  IF (status /= 0) THEN
    message = "Unable to allocate memory for field_data array"
    RETURN
  END IF
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" (LBEGIN here) is in 64-bit (8-byte) words but "POS" is in bytes 
! (hence the "*8") and is offset by one byte so that "POS=1" is the start of 
! the file (hence the "+1") 
READ(ff_id, POS=(start)*8+1, IOSTAT=status) field_data
IF (status /= 0) THEN
  message = "Failed to read field_data"
  RETURN
END IF

END FUNCTION f_shum_read_field_data_int32

!------------------------------------------------------------------------------!

END MODULE f_shum_fieldsfile_mod
