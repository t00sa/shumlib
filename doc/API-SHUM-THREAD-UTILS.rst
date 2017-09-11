API Reference: shum_thread_utils
---------------------------------

Fortran Functions/Subroutines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

``get_shum_thread_utils_version``
''''''''''''''''''''''''''''''''''

All Shumlib libraries expose a module and function named in this format; it
allows access to the Shumlib version number used when compiling the library.

    **Available via module**
        ``f_shum_thread_utils_version_mod``

    **Syntax**
        ``version = get_shum_thread_utils_version()``

    **Returns**
        ``version (INTEGER(KIND=C_INT64_T))``
            Shumlib version number, in format ``YYYYMMX`` (the 4-digit year
            followed by 2-digit month and an additional digit signifying the
            release count within that specified month).

C Functions
%%%%%%%%%%%

``get_shum_thread_utils_version``
''''''''''''''''''''''''''''''''''

All Shumlib libraries expose a function named in this format; it allows access
to the Shumlib version number used when compiling the library.

    **Required header/s**
        ``c_shum_thread_utils_version.h``

    **Syntax**
        ``version = get_shum_thread_utils_version()``

    **Returns**
        ``version (int)``
            Shumlib version number, in format ``YYYYMMX`` (the 4-digit year
            followed by 2-digit month and an additional digit signifying the
            release count within that specified month).

``f_shum_newLock``
''''''''''''''''''''''''''''

This creates a new lock and returns its lock id number.

    **Required header/s**
        ``c_shum_thread_utils.h``

    **Syntax**
        ``lock = f_shum_newLock()``

    **Arguments**
        None

    **Return Value**
        ``(int64_t)``
            Lock id number of the newly created lock.

``f_shum_releaseLock``
''''''''''''''''''''''''''''

This will destroy the lock "l" if:

  - it exists,
  - there are no pending lockings
  - and it is currently unlocked

If these conditions are met it returns successCode, else it returns failCode

    **Required header/s**
        ``c_shum_thread_utils.h``

    **Syntax**
        ``result = f_shum_releaseLock(l)``

    **Arguments**
        ``l (int64_t)``
            Lock id number of the lock to destroy.

    **Return Value**
        ``(int64_t)``
            Code for the success of the operation. Is equal to successCode on
            successfull destruction of the lock, else it is equal to failCode.

``f_shum_Lock``
''''''''''''''''''''''''''''

This will lock the lock "l" if:

 - it exists,
 - and is not already locked by the calling thread

If these conditions are met it returns successCode, else it returns failCode

If the lock is currently locked by another thread, execution will be blocked
until that thread reliquishes it, at which point the calling thread will
re-lock it and continue execution.

    **Required header/s**
        ``c_shum_thread_utils.h``

    **Syntax**
        ``result = f_shum_Lock(l)``

    **Arguments**
        ``l (int64_t)``
            Lock id number of the lock to aquire.

    **Return Value**
        ``(int64_t)``
            Code for the success of the operation. Is equal to successCode on
            successfull aquisition of the lock, else it is equal to failCode.

``f_shum_TestLock``
''''''''''''''''''''''''''''

This will attempt to lock the lock "l" if:

 - it exists,
 - and is not already locked by the current thread

If these conditions are met and:

 - the lock was previously unlocked - successCode is returned
 - the lock is already locked - alreadyLockedCode is returned

If these conditions are not met it returns failCode

Unlike Lock(), TestLock(l) will not block execution if the lock is currently
locked by another thread. Instead alreadyLockedCode is returned, and
this thread abandons trying to obtain ownership of the lock.

    **Required header/s**
        ``c_shum_thread_utils.h``

    **Syntax**
        ``result = f_shum_TestLock(l)``

    **Arguments**
        ``l (int64_t)``
            Lock id number of the lock to aquire.

    **Return Value**
        ``(int64_t)``
            Code for the success of the operation. Is equal to successCode on
            successfull aquisition of the lock. Is equal to alreadyLockedCode
            if the lock is already locked. Else it is equal to failCode.

``f_shum_Unlock``
''''''''''''''''''''''''''''

This will unlock the lock "l" if:

 - it exists,
 - and is locked by the current thread

If these conditions are met it returns successCode, else it returns failCode

    **Required header/s**
        ``c_shum_thread_utils.h``

    **Syntax**
        ``result = f_shum_Unlock(l)``

    **Arguments**
        ``l (int64_t)``
            Lock id number of the lock to release.

    **Return Value**
        ``(int64_t)``
            Code for the success of the operation. Is equal to successCode on
            successfull release of the lock, else it is equal to failCode.

``f_shum_threadFlush``
''''''''''''''''''''''''''''

This flushes the OpenMP environment and always returns successCode

    **Required header/s**
        ``c_shum_thread_utils.h``

    **Syntax**
        ``result = f_shum_threadFlush()``

    **Arguments**
        None

    **Return Value**
        ``(int64_t)``
            Is always equal to successCode

``f_shum_threadID``
''''''''''''''''''''''''''''

This returns the OpenMP thread number

    **Required header/s**
        ``c_shum_thread_utils.h``

    **Syntax**
        ``tid = f_shum_threadID()``

    **Arguments**
        None

    **Return Value**
        ``(int64_t)``
            the OpenMP thread number, as defined by the OpenMP
            specification.

``f_shum_inPar``
''''''''''''''''''''''''''''

This returns ``1`` when called within an OpenMP parallel region,
and ``0`` otherwise

    **Required header/s**
        ``c_shum_thread_utils.h``

    **Syntax**
        ``result = f_shum_inPar()``

    **Arguments**
        None

    **Return Value**
        ``(int64_t)``
            ``1`` when called within an OpenMP parallel region,
            and ``0`` otherwise
