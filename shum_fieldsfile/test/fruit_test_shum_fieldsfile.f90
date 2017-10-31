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
MODULE fruit_test_shum_fieldsfile_mod

USE fruit
USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE

IMPLICIT NONE 

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

CONTAINS

SUBROUTINE fruit_test_shum_fieldsfile

USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
USE f_shum_fieldsfile_version_mod, ONLY: get_shum_fieldsfile_version

IMPLICIT NONE 

INTEGER(KIND=int64) :: version

! Note: we don't have a test case for the version checking because we don't
! want the testing to include further hardcoded version numbers to test
! against.  Since the version module is simple and hardcoded anyway it's 
! sufficient to make sure it is callable; but let's print the version for info.
version = get_shum_fieldsfile_version()

WRITE(OUTPUT_UNIT, "()")
WRITE(OUTPUT_UNIT, "(A,I0)")                                                   &
    "Testing shum_fieldsfile at Shumlib version: ", version

CALL run_test_case(test_ffapi, "test api")

END SUBROUTINE fruit_test_shum_fieldsfile

!------------------------------------------------------------------------------!

SUBROUTINE test_ffapi

USE f_shum_fieldsfile_mod, ONLY:                                               &
  f_shum_open_file,                                                            &
  f_shum_create_file,                                                          &
  f_shum_close_file,                                                           &
  f_shum_read_fixed_length_header,                                             &
  f_shum_read_integer_constants,                                               &
  f_shum_read_real_constants,                                                  &
  f_shum_read_level_dependent_constants,                                       &
  f_shum_read_row_dependent_constants,                                         &
  f_shum_read_column_dependent_constants,                                      &
  f_shum_read_additional_parameters,                                           &
  f_shum_read_extra_constants,                                                 &
  f_shum_read_temp_histfile,                                                   &
  f_shum_read_compressed_index,                                                &
  f_shum_read_lookup,                                                          &
  f_shum_read_field_data

USE f_shum_lookup_indices_mod, ONLY:                                           &
  lbpack, lbuser1

IMPLICIT NONE 

INTEGER(KIND=int32) :: filename_len
CHARACTER(LEN=1000) :: filename
INTEGER(KIND=int64) :: ff_id
INTEGER(KIND=int64) :: status
INTEGER(KIND=int32) :: status32

INTEGER(KIND=int64)              :: fixed_length_header(256)
INTEGER(KIND=int64), ALLOCATABLE :: integer_constants(:)
REAL(KIND=real64),   ALLOCATABLE :: real_constants(:)
REAL(KIND=real64),   ALLOCATABLE :: level_dependent_constants(:, :)
REAL(KIND=real64),   ALLOCATABLE :: row_dependent_constants(:, :)
REAL(KIND=real64),   ALLOCATABLE :: column_dependent_constants(:, :)
REAL(KIND=real64),   ALLOCATABLE :: additional_parameters(:, :)
REAL(KIND=real64),   ALLOCATABLE :: extra_constants(:)
REAL(KIND=real64),   ALLOCATABLE :: temp_histfile(:)
REAL(KIND=real64),   ALLOCATABLE :: compressed_index(:)
INTEGER(KIND=int64), ALLOCATABLE :: lookup(:, :)
REAL(KIND=real64),   ALLOCATABLE :: field_data_r64(:)
INTEGER(KIND=int64), ALLOCATABLE :: field_data_i64(:)
REAL(KIND=real32),   ALLOCATABLE :: field_data_r32(:)
INTEGER(KIND=int32), ALLOCATABLE :: field_data_i32(:)

CHARACTER(LEN=500) :: message = ""

INTEGER(KIND=int64) :: i

CALL GET_COMMAND_ARGUMENT(1, filename, filename_len, status32)
status = INT(status32, KIND=int64)
IF (status /= 0) THEN
  PRINT*, "No file provided for test of FF API, skipping"
  RETURN
END IF

status = f_shum_open_file(filename, ff_id, message)
IF (status /= 0) THEN
  PRINT*, TRIM(message)
  RETURN
END IF

!------------------------------------------------------------------------------!
status = f_shum_read_fixed_length_header(                                      &
  ff_id, fixed_length_header, message)

PRINT*, "==================="
PRINT*, "FIXED_LENGTH_HEADER"
PRINT*, "==================="
IF (status == 0) THEN
  PRINT*, (fixed_length_header(i), i = 1,SIZE(fixed_length_header))
ELSE
  PRINT*, TRIM(message)
  RETURN
END IF

!------------------------------------------------------------------------------!
status = f_shum_read_integer_constants(                                        &
  ff_id, integer_constants, message)

PRINT*, ""
PRINT*, "================="
PRINT*, "INTEGER_CONSTANTS"
PRINT*, "================="
IF (status == 0) THEN
  PRINT*, (integer_constants(i), i = 1,SIZE(integer_constants))
ELSE
  PRINT*, TRIM(message)
  RETURN
END IF

!------------------------------------------------------------------------------!
status = f_shum_read_real_constants(                                           &
  ff_id, real_constants, message)

PRINT*, ""
PRINT*, "=============="
PRINT*, "REAL_CONSTANTS"
PRINT*, "=============="
IF (status == 0) THEN
  PRINT*, (real_constants(i), i= 1,SIZE(real_constants))
ELSE
  PRINT*, TRIM(message)
  RETURN
END IF

!------------------------------------------------------------------------------!
status = f_shum_read_level_dependent_constants(                                &
  ff_id, level_dependent_constants, message)

PRINT*, ""
PRINT*, "========================="
PRINT*, "LEVEL_DEPENDENT_CONSTANTS"
PRINT*, "========================="
IF (status == 0) THEN
  PRINT*, (level_dependent_constants(:,i), i = 1,SIZE(level_dependent_constants,2))
ELSE
  PRINT*, TRIM(message)
  RETURN
END IF

!------------------------------------------------------------------------------!
status = f_shum_read_row_dependent_constants(                                  &
  ff_id, row_dependent_constants, message)

PRINT*, ""
PRINT*, "======================="
PRINT*, "ROW_DEPENDENT_CONSTANTS"
PRINT*, "======================="
IF (status == 0) THEN
  PRINT*, (row_dependent_constants(:,i), i = 1,SIZE(row_dependent_constants,2))
ELSE
  PRINT*, TRIM(message)
END IF

!------------------------------------------------------------------------------!
status = f_shum_read_column_dependent_constants(                               &
  ff_id, column_dependent_constants, message)

PRINT*, ""
PRINT*, "=========================="
PRINT*, "COLUMN_DEPENDENT_CONSTANTS"
PRINT*, "=========================="
IF (status == 0) THEN
  PRINT*, (column_dependent_constants(:,i), i = 1,SIZE(column_dependent_constants,2))
ELSE
  PRINT*, TRIM(message)
END IF

!------------------------------------------------------------------------------!
status = f_shum_read_additional_parameters(                                    &
  ff_id, additional_parameters, message)

PRINT*, ""
PRINT*, "====================="
PRINT*, "ADDITIONAL PARAMETERS"
PRINT*, "====================="
IF (status == 0) THEN
  PRINT*, (additional_parameters(:,i), i = 1,SIZE(additional_parameters,2))
ELSE
  PRINT*, TRIM(message)
END IF

!------------------------------------------------------------------------------!
status = f_shum_read_extra_constants(                                          &
  ff_id, extra_constants, message)

PRINT*, ""
PRINT*, "==============="
PRINT*, "EXTRA CONSTANTS"
PRINT*, "==============="
IF (status == 0) THEN
  PRINT*, extra_constants(:)
ELSE
  PRINT*, TRIM(message)
END IF

!------------------------------------------------------------------------------!
status = f_shum_read_temp_histfile(                                            &
  ff_id, temp_histfile, message)

PRINT*, ""
PRINT*, "============="
PRINT*, "TEMP HISTFILE"
PRINT*, "============="
IF (status == 0) THEN
  PRINT*, temp_histfile(:)
ELSE
  PRINT*, TRIM(message)
END IF

!------------------------------------------------------------------------------!
DO i=1,3
  IF (ALLOCATED(compressed_index)) DEALLOCATE(compressed_index)

  status = f_shum_read_compressed_index(                                       &
    ff_id, compressed_index, INT(i, KIND=int64), message)

  PRINT*, ""
  PRINT*, "================"
  PRINT*, "COMPRESSED INDEX", i
  PRINT*, "================"
  IF (status == 0) THEN
    PRINT*, compressed_index(:)
  ELSE
    PRINT*, TRIM(message)
  END IF
END DO

!------------------------------------------------------------------------------!
status = f_shum_read_lookup(                                                   &
  ff_id, lookup, message)

PRINT*, ""
PRINT*, "======"
PRINT*, "LOOKUP"
PRINT*, "======"
IF (status == 0) THEN
  DO i=1,SIZE(lookup, 2)
    PRINT*, ""
    PRINT*, "LOOKUP:", i
    PRINT*, "-------"
    PRINT*, lookup(1:45,i), TRANSFER(lookup(46:64,i), 0.0_real64, 64-45)
  END DO
ELSE
  PRINT*, TRIM(message)
END IF


! This block is massively wasteful - reading each field in the file 5 times,
! but for now we just want to ensure coverage of the API...
!------------------------------------------------------------------------------!
DO i=1, SIZE(lookup, 2)

  IF (lookup(1, i) == -99) EXIT

  ! Read the field 4 times, forcing the data type using the "ignore" flag
  !-----------------------------------------------------------------------
  status = f_shum_read_field_data(ff_id, i, field_data_r64, message,          &
                                 ignore_dtype=.TRUE.)
  PRINT*, "Read field as REAL64 (ignored dtype)", MAXVAL(field_data_r64)
  status = f_shum_read_field_data(ff_id, i, field_data_i64, message,          &
                                 ignore_dtype=.TRUE.)
  PRINT*, "Read field as INT64 (ignored dtype)", MAXVAL(field_data_i64)
  status = f_shum_read_field_data(ff_id, i, field_data_r32, message,          &
                                 ignore_dtype=.TRUE.)
  PRINT*, "Read field as REAL32 (ignored dtype)", MAXVAL(field_data_r32)
  status = f_shum_read_field_data(ff_id, i, field_data_i32, message,          &
                                 ignore_dtype=.TRUE.)
  PRINT*, "Read field as INT32 (ignored dtype)", MAXVAL(field_data_i32)

  ! And now use actual logic to determine the correct return array type
  ! for each field and read it in without the "ignore" flag
  !-----------------------------------------------------------------------
  SELECT CASE (MOD(lookup(lbpack, i), 10_int64))
    CASE (0_int64)
      ! Unpacked 64-bit
      SELECT CASE (lookup(lbuser1, i))
        CASE (1_int64)
          ! Real
          status = f_shum_read_field_data(ff_id, i, field_data_r64, message)            
          PRINT*, "Read field as REAL64 Unpacked", MAXVAL(field_data_r64)
        CASE (2_int64, 3_int64)
          ! Integer or Logical
          status = f_shum_read_field_data(ff_id, i, field_data_i64, message)
          PRINT*, "Read field as INTEGER64 Unpacked", MAXVAL(field_data_i64)
        END SELECT
    CASE (1_int64)
      ! WGDOS - always 32-bit integer
      status = f_shum_read_field_data(ff_id, i, field_data_i32, message)
      PRINT*, "Read field as INTEGER32 WGDOS Packed", MAXVAL(field_data_i32)
    CASE (2_int64)
      ! 32-bit Truncated
      SELECT CASE (lookup(lbuser1, i))
        CASE (1_int64)
          ! Real
          status = f_shum_read_field_data(ff_id, i, field_data_r32, message)
          PRINT*, "Read field as REAL32 Truncated", MAXVAL(field_data_r32)
        CASE (2_int64, 3_int64)
          ! Integer or Logical
          status = f_shum_read_field_data(ff_id, i, field_data_i32, message)
          PRINT*, "Read field as INTEGER32 Truncated", MAXVAL(field_data_i32)
        END SELECT
    CASE DEFAULT
      PRINT*, "Unrecognised value of LBPACK, cannot read"
    END SELECT

END DO

status = f_shum_close_file(ff_id, message)
IF (status /= 0) THEN
  PRINT*, TRIM(message)
  RETURN
END IF

END SUBROUTINE test_ffapi

!------------------------------------------------------------------------------!

END MODULE fruit_test_shum_fieldsfile_mod
