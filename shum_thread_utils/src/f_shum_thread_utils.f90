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
! This module contains the wrappers around OpenMP functionality.
!
MODULE f_shum_thread_utils_mod

!$ USE OMP_LIB
USE, INTRINSIC :: ISO_C_BINDING

IMPLICIT NONE
PRIVATE

INTEGER, PARAMETER                 :: noOwner=-1

TYPE threadLock
  LOGICAL                          :: inUse = .FALSE.
!$ INTEGER(KIND=omp_lock_kind)     :: lock
  INTEGER(KIND=c_int64_t)          :: owner = noOwner
  INTEGER                          :: lockqueue = 0
END TYPE threadLock

TYPE(threadLock), SAVE, ALLOCATABLE :: locks(:)

INTEGER, PARAMETER                  :: chunk = 10

INTEGER, PARAMETER                  :: successCode       =  0
INTEGER, PARAMETER                  :: failCode          =  1
INTEGER, PARAMETER                  :: alreadyLockedCode = -1

CONTAINS

!------------------------------------------------------------------------------!
! Lock Functions                                                               !
!------------------------------------------------------------------------------!

! newLock() creates a new lock and returns its lock id number

FUNCTION newLock()                                                             &
  BIND(c,NAME="f_shum_newLock")                                                &
  RESULT(r)

IMPLICIT NONE

INTEGER, PARAMETER :: no_lock = -1

TYPE(threadLock), ALLOCATABLE :: temp(:)
INTEGER(KIND=c_int64_t)       :: r
INTEGER                       :: n, old_locks_size

!$OMP CRITICAL (SHUM_THREAD_UTILS_LOCKSTATE)

IF ( .NOT. ALLOCATED(locks) ) ALLOCATE(locks(chunk))

r = no_lock

old_locks_size = SIZE(locks)

! look for an unused lock
DO n=1,old_locks_size
  IF (locks(n)%inUse) CYCLE
  r = n
  EXIT
END DO

! check if we have actually found an unused lock, and if we haven't...
IF (r==no_lock) THEN

  ! ...increase storage, because there isn't enough space for a new locked

  ! Our new lock will be the first of the extra chunk
  r = old_locks_size + 1

  ! allocate and copy to a temporary array
  ALLOCATE (temp (old_locks_size + chunk ))
  DO n=1,old_locks_size
    temp(n)=locks(n)
  END DO

  ! move the allocation of the locks to the original array
  DEALLOCATE (locks)
  CALL MOVE_ALLOC(temp, locks)

END IF

! initialise the new lock
!$ CALL omp_init_lock( locks(r)%lock )
locks(r)%inUse = .TRUE.

!$OMP END CRITICAL(SHUM_THREAD_UTILS_LOCKSTATE)

END FUNCTION newLock

!------------------------------------------------------------------------------!

! releaseLock(l) will destroy the lock "l" if: -
!   * it exists (i.e locks is allocated and lock(l) is in use),
!   * there are no pending lockings
!   * and is owned by noone (i.e. is unlocked)
!
! If these conditions are met it returns successCode, else it returns failCode


FUNCTION releaseLock(l)                                                        &
  BIND(c,NAME="f_shum_releaseLock")                                            &
  RESULT(r)

IMPLICIT NONE

INTEGER(KIND=c_int64_t), INTENT(IN) :: l
INTEGER(KIND=c_int64_t)             :: r

r = failCode

IF ( ALLOCATED(locks) ) THEN

  IF (l>0 .AND. l<=SIZE(locks)) THEN

!$OMP CRITICAL(SHUM_THREAD_UTILS_LOCKSTATE)
    IF ( locks(l)%inUse                                                        &
         .AND. locks(l)%owner==noOwner                                         &
         .AND. locks(l)%lockqueue==0 ) THEN

      locks(l)%inUse=.FALSE.
!$    CALL omp_destroy_lock( locks(l)%lock)

      r = successCode

    END IF
!$OMP END CRITICAL(SHUM_THREAD_UTILS_LOCKSTATE)

  END IF

END IF

END FUNCTION releaseLock

!------------------------------------------------------------------------------!

! Lock(l) will lock the lock "l" if: -
!   * it exists (i.e locks is allocated and lock(l) is in use),
!   * and is not already owned by the current thread
!
! If these conditions are met it returns successCode, else it returns failCode
!
! If the lock is currently owned by another thread (i.e. already locked),
! execution will be blocked until that thread reliquishes it, and this thread
! has subsequntly acquired ownership, re-locking it.

FUNCTION Lock(l)                                                               &
  BIND(c,NAME="f_shum_Lock")                                                   &
  RESULT(r)

IMPLICIT NONE

INTEGER(KIND=c_int64_t), INTENT(IN) :: l
INTEGER(KIND=c_int64_t)             :: tid
INTEGER(KIND=c_int64_t)             :: r

r = failCode

IF ( ALLOCATED(locks) ) THEN

  IF (l<=SIZE(locks)) THEN

    tid = threadID()

!$OMP CRITICAL(SHUM_THREAD_UTILS_LOCKSTATE)
    locks(l)%lockqueue = locks(l)%lockqueue + 1
!$OMP END CRITICAL(SHUM_THREAD_UTILS_LOCKSTATE)

    ! locks(l)%inUse cannot become .FALSE. here as locks(l)%lockqueue is >0
    ! locks(l)%owner cannot change to the value tid if it isn't already
    ! We therefore do not need to worry about strict data consistency here
    IF ( locks(l)%inUse .AND. locks(l)%owner/=tid ) THEN

!$    CALL omp_set_lock(locks(l)%lock)
!$OMP CRITICAL(SHUM_THREAD_UTILS_LOCKSTATE)
      locks(l)%owner=tid
!$OMP END CRITICAL(SHUM_THREAD_UTILS_LOCKSTATE)

      r = successCode

    END IF

!$OMP CRITICAL(SHUM_THREAD_UTILS_LOCKSTATE)
    locks(l)%lockqueue = locks(l)%lockqueue - 1
!$OMP END CRITICAL(SHUM_THREAD_UTILS_LOCKSTATE)

  END IF

END IF

END FUNCTION Lock

!------------------------------------------------------------------------------!

! TestLock(l) will attempt to lock the lock "l" if: -
!   * it exists (i.e locks is allocated and lock(l) is in use),
!   * and is not owned by the current thread
!
! If these conditions are met and: -
!   * the lock was previously unlocked - successCode is returned
!   * the lock is already locked - alreadyLockedCode is returned
! If these conditions are not met it returns failCode
!
! Unlike Lock(), TestLock(l) will not block execution if the lock is currently
! owned (locked) by another thread. Instead alreadyLockedCode is returned, and
! this thread abandons trying to obtain ownership of the lock.

FUNCTION TestLock(l)                                                           &
  BIND(c,NAME="f_shum_TestLock")                                               &
  RESULT(r)

IMPLICIT NONE

INTEGER(KIND=c_int64_t), INTENT(IN) :: l
INTEGER(KIND=c_int64_t)             :: tid
INTEGER(KIND=c_int64_t)             :: r

LOGICAL :: test_set

r = failCode

IF ( ALLOCATED(locks) ) THEN

  IF (l<=SIZE(locks)) THEN

    tid = threadID()

!$OMP CRITICAL(SHUM_THREAD_UTILS_LOCKSTATE)
    locks(l)%lockqueue = locks(l)%lockqueue + 1
!$OMP END CRITICAL(SHUM_THREAD_UTILS_LOCKSTATE)

    ! locks(l)%inUse cannot become .FALSE. here as locks(l)%lockqueue is >0
    ! locks(l)%owner cannot change to the value tid if it isn't already
    ! We therefore do not need to worry about strict data consistency here
    IF ( locks(l)%inUse .AND. locks(l)%owner/=tid ) THEN

      test_set = .TRUE.

      ! omp_test_lock() returns .TRUE. if the lock was previously unlocked, else
      ! it returns .FALSE.
!$    test_set = omp_test_lock(locks(l)%lock)

      IF (test_set) THEN

!$OMP CRITICAL(SHUM_THREAD_UTILS_LOCKSTATE)
        locks(l)%owner=tid
!$OMP END CRITICAL(SHUM_THREAD_UTILS_LOCKSTATE)
        r = successCode

      ELSE

        r = alreadyLockedCode

      END IF

    END IF

!$OMP CRITICAL(SHUM_THREAD_UTILS_LOCKSTATE)
    locks(l)%lockqueue = locks(l)%lockqueue - 1
!$OMP END CRITICAL(SHUM_THREAD_UTILS_LOCKSTATE)

  END IF

END IF

END FUNCTION TestLock

!------------------------------------------------------------------------------!

! Unlock(l) will unlock the lock "l" if: -
!   * it exists (i.e locks is allocated and lock(l) is in use),
!   * and is owned by the current thread (i.e. locked)
!
! If these conditions are met it returns successCode, else it returns failCode

FUNCTION Unlock(l)                                                             &
  BIND(c,NAME="f_shum_Unlock")                                                 &
  RESULT(r)

IMPLICIT NONE

INTEGER(KIND=c_int64_t), INTENT(IN) :: l
INTEGER(KIND=c_int64_t)             :: r

r = failCode

IF ( ALLOCATED(locks) ) THEN

  IF (l<=SIZE(locks)) THEN

!$OMP FLUSH
    IF (locks(l)%inUse .AND. locks(l)%owner==threadID() ) THEN

!$OMP CRITICAL(SHUM_THREAD_UTILS_LOCKSTATE)
!$    CALL omp_unset_lock(locks(l)%lock)
      locks(l)%owner=noowner
!$OMP END CRITICAL(SHUM_THREAD_UTILS_LOCKSTATE)

      r = successCode

    END IF

  END IF

END IF

END FUNCTION Unlock

!------------------------------------------------------------------------------!
! Other Functions                                                              !
!------------------------------------------------------------------------------!

! threadFlush() flushes the OpenMP environment and always returns successCode

FUNCTION threadFlush()                                                         &
  BIND(c,NAME="f_shum_threadFlush")                                            &
  RESULT(r)

IMPLICIT NONE

INTEGER(KIND=c_int64_t) :: r

!$OMP FLUSH
r = successCode

END FUNCTION threadFlush

!------------------------------------------------------------------------------!

! threadID() returns the OpenMP thread number

FUNCTION threadID()                                                            &
  BIND(c,NAME="f_shum_threadID")                                               &
  RESULT(r)

IMPLICIT NONE

INTEGER(KIND=c_int64_t) :: r

r = 0
!$ r = omp_get_thread_num()

END FUNCTION threadID

!------------------------------------------------------------------------------!

! inPar() returns 1 in an OpenMP parallel region, and 0 otherwise

FUNCTION inPar()                                                               &
  BIND(c,NAME="f_shum_inPar")                                                  &
  RESULT(r)

IMPLICIT NONE

INTEGER(KIND=c_int64_t) :: r

r = 0
!$ IF(omp_in_parallel()) r = 1

END FUNCTION inPar

!------------------------------------------------------------------------------!

END MODULE f_shum_thread_utils_mod

