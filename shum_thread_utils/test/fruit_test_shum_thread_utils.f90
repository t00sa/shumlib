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
MODULE fruit_test_shum_thread_utils_mod

USE fruit
USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT64_T, C_INT32_T, C_FLOAT,          &
                                       C_DOUBLE, C_BOOL
!$ USE omp_lib

IMPLICIT NONE
PRIVATE

PUBLIC :: fruit_test_shum_thread_utils

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

INTERFACE

!-------------!

  SUBROUTINE c_test_returns_valid_lock(test_ret)                               &
             BIND(c, name="c_test_returns_valid_lock")

    IMPORT :: C_BOOL

    IMPLICIT NONE

    LOGICAL(KIND=C_BOOL), INTENT(OUT) :: test_ret

  END SUBROUTINE c_test_returns_valid_lock

!-------------!

  SUBROUTINE c_test_invalid_lock_release(test_ret)                             &
             BIND(c, name="c_test_invalid_lock_release")

    IMPORT :: C_BOOL

    IMPLICIT NONE

    LOGICAL(KIND=C_BOOL), INTENT(OUT) :: test_ret

  END SUBROUTINE c_test_invalid_lock_release

!-------------!

  SUBROUTINE c_test_create_and_release_lock(test_ret)                          &
             BIND(c, name="c_test_create_and_release_lock")

    IMPORT :: C_BOOL

    IMPLICIT NONE

    LOGICAL(KIND=C_BOOL), INTENT(OUT) :: test_ret

  END SUBROUTINE c_test_create_and_release_lock

!-------------!

  SUBROUTINE c_test_create_many_locks(test_ret)                                &
             BIND(c, name="c_test_create_many_locks")

    IMPORT :: C_BOOL

    IMPLICIT NONE

    LOGICAL(KIND=C_BOOL), INTENT(OUT) :: test_ret

  END SUBROUTINE c_test_create_many_locks

!-------------!

  SUBROUTINE c_test_backfill_locks(test_ret)                                   &
             BIND(c, name="c_test_backfill_locks")

    IMPORT :: C_BOOL

    IMPLICIT NONE

    LOGICAL(KIND=C_BOOL), INTENT(OUT) :: test_ret

  END SUBROUTINE c_test_backfill_locks

!-------------!

  SUBROUTINE c_test_sweep_release_locks(test_ret)                              &
             BIND(c, name="c_test_sweep_release_locks")

    IMPORT :: C_BOOL

    IMPLICIT NONE

    LOGICAL(KIND=C_BOOL), INTENT(OUT) :: test_ret

  END SUBROUTINE c_test_sweep_release_locks

!-------------!

  SUBROUTINE c_test_inpar(test_ret,par)                                        &
             BIND(c, name="c_test_inpar")

    IMPORT :: C_BOOL, C_INT64_T

    IMPLICIT NONE

    LOGICAL(KIND=C_BOOL), INTENT(OUT) :: test_ret
    INTEGER(KIND=C_INT64_T) :: par

  END SUBROUTINE c_test_inpar

!-------------!

  SUBROUTINE c_test_threadid(test_ret,tid)                                     &
             BIND(c, name="c_test_threadid")

    IMPORT :: C_BOOL, C_INT64_T

    IMPLICIT NONE

    LOGICAL(KIND=C_BOOL), INTENT(OUT) :: test_ret
    INTEGER(KIND=C_INT64_T) :: tid

  END SUBROUTINE c_test_threadid

!-------------!

  SUBROUTINE c_test_threadflush(test_ret,shared1)                              &
             BIND(c, name="c_test_threadflush")

    IMPORT :: C_BOOL, C_INT64_T

    IMPLICIT NONE

    LOGICAL(KIND=C_BOOL), INTENT(OUT) :: test_ret
    INTEGER(KIND=C_INT64_T) :: shared1

  END SUBROUTINE c_test_threadflush

!-------------!

END INTERFACE

!------------------------------------------------------------------------------!

CONTAINS

!------------------------------------------------------------------------------!

SUBROUTINE fruit_test_shum_thread_utils

USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
USE f_shum_thread_utils_version_mod, ONLY: get_shum_thread_utils_version

IMPLICIT NONE

INTEGER(KIND=int64) :: version

! Note: we don't have a test case for the version checking because we don't
! want the testing to include further hardcoded version numbers to test
! against.  Since the version module is simple and hardcoded anyway it's
! sufficient to make sure it is callable; but let's print the version for info.
version = get_shum_thread_utils_version()

WRITE(OUTPUT_UNIT, "()")
WRITE(OUTPUT_UNIT, "(A,I0)")                                                   &
    "Testing shum_thread_utils at Shumlib version: ", version

CALL run_test_case(test_returns_valid_lock, "returns_valid_lock")
CALL run_test_case(test_invalid_lock_release, "invlaid_lock_release")
CALL run_test_case(test_create_and_release_lock, "create_and_release_lock")
CALL run_test_case(test_create_many_locks, "create_many_locks")
CALL run_test_case(test_backfill_locks, "test_backfill_locks")
CALL run_test_case(test_inpar, "test_inpar")
CALL run_test_case(test_threadid, "test_threadid")
CALL run_test_case(test_flush, "test_flush")

END SUBROUTINE fruit_test_shum_thread_utils

!------------------------------------------------------------------------------!

SUBROUTINE test_returns_valid_lock

IMPLICIT NONE

LOGICAL(KIND=C_BOOL) :: test_ret
LOGICAL :: check

CALL set_case_name("test_returns_valid_lock")
CALL c_test_returns_valid_lock(test_ret)
check = test_ret
CALL assert_true(check, "Returned value is not a valid lock")

END SUBROUTINE test_returns_valid_lock

!------------------------------------------------------------------------------!

SUBROUTINE test_invalid_lock_release

IMPLICIT NONE

LOGICAL(KIND=C_BOOL) :: test_ret
LOGICAL :: check

CALL set_case_name("test_invalid_lock_release")
CALL c_test_invalid_lock_release(test_ret)
check = test_ret
CALL assert_true(check, "Did not handle/detect invalid lock release request")

END SUBROUTINE test_invalid_lock_release

!------------------------------------------------------------------------------!

SUBROUTINE test_create_and_release_lock

IMPLICIT NONE

LOGICAL(KIND=C_BOOL) :: test_ret
LOGICAL :: check

CALL set_case_name("test_create_and_release_lock")
CALL c_test_create_and_release_lock(test_ret)
check = test_ret
CALL assert_true(check, "Did not correctly create, then release a lock")

END SUBROUTINE test_create_and_release_lock

!------------------------------------------------------------------------------!

SUBROUTINE test_create_many_locks

IMPLICIT NONE

LOGICAL(KIND=C_BOOL) :: test_ret
LOGICAL :: check

CALL set_case_name("test_create_many_locksk")
CALL c_test_create_many_locks(test_ret)
check = test_ret
CALL assert_true(check, "Did not successfully create a high number of locks")

END SUBROUTINE test_create_many_locks

!------------------------------------------------------------------------------!

SUBROUTINE test_backfill_locks

IMPLICIT NONE

LOGICAL(KIND=C_BOOL) :: test_ret
LOGICAL :: check

CALL set_case_name("test_backfill_locks")
CALL c_test_backfill_locks(test_ret)
check = test_ret
CALL assert_true(check, "Did not backfill lock array")

END SUBROUTINE test_backfill_locks

!------------------------------------------------------------------------------!

SUBROUTINE test_sweep_release_locks

IMPLICIT NONE

LOGICAL(KIND=C_BOOL) :: test_ret
LOGICAL :: check

CALL set_case_name("test_sweep_release_locks")
CALL c_test_sweep_release_locks(test_ret)
check = test_ret
CALL assert_true(check, "Did not successfully release all locks in a sweep")

END SUBROUTINE test_sweep_release_locks

!------------------------------------------------------------------------------!

SUBROUTINE test_inpar

IMPLICIT NONE

LOGICAL(KIND=C_BOOL) :: test_ret
LOGICAL :: check
INTEGER(KIND=C_INT64_T) :: par

par = -1

CALL set_case_name("test_inpar")

CALL c_test_inpar(test_ret,par)
check = test_ret
CALL assert_true(check, "Did succesfully call c_shum_inPar()")
CALL assert_true(par==0, "c_shum_inPar() detected a false parallel region")

!$ CALL omp_set_num_threads(3)
!$OMP PARALLEL
!$ CALL c_test_inpar(test_ret,par)
!$OMP END PARALLEL
!$ check = test_ret
!$ CALL assert_true(check, "Did succesfully call c_shum_inPar()")
!$ CALL assert_true(par==1, "c_shum_inPar() did not detect a parallel region")

END SUBROUTINE test_inpar

!------------------------------------------------------------------------------!

SUBROUTINE test_threadid

IMPLICIT NONE

LOGICAL(KIND=C_BOOL) :: test_ret
LOGICAL :: check
INTEGER(KIND=C_INT64_T) :: tid

!$ INTEGER :: i

tid = -1

CALL set_case_name("test_threadid")

CALL c_test_threadid(test_ret,tid)
check = test_ret
CALL assert_true(check, "Thread IDs not calculated correctly")
CALL assert_true(tid==0, "Thread ID not zero outside parallel region")

!$ CALL omp_set_num_threads(3)
!$OMP PARALLEL DO SCHEDULE(static, 1) DEFAULT(NONE) PRIVATE(tid, i)            &
!$OMP REDUCTION(.AND.:test_ret)
!$ DO i=0,2
!$ CALL c_test_threadid(test_ret,tid)
!$ IF (tid/=i) test_ret = .FALSE.
!$ END DO
!$OMP END PARALLEL DO
!$ check = test_ret
!$ CALL assert_true(check, "Thread IDs not calculated correctly" //            &
!$                         " in parallel region")

END SUBROUTINE test_threadid

!------------------------------------------------------------------------------!

SUBROUTINE test_flush

IMPLICIT NONE

LOGICAL(KIND=C_BOOL) :: test_ret
LOGICAL :: check

INTEGER(KIND=C_INT64_T) :: shared1

CALL set_case_name("test_threadflush")

! These are dummy tests for now.

! TODO: work out how to correctly test a flush, and implement it here

shared1=0
CALL c_test_threadflush(test_ret,shared1)
check = test_ret
CALL assert_true(check, "Dummy flush test fails!")

END SUBROUTINE test_flush

!------------------------------------------------------------------------------!

END MODULE fruit_test_shum_thread_utils_mod
