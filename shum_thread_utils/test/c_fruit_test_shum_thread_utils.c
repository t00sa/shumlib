/* *********************************COPYRIGHT**********************************/
/* (C) Crown copyright Met Office. All rights reserved.                       */
/* For further details please refer to the file LICENCE.txt                   */
/* which you should have received as part of this distribution.               */
/* *********************************COPYRIGHT**********************************/
/*                                                                            */
/* This file is part of the UM Shared Library project.                        */
/*                                                                            */
/* The UM Shared Library is free software: you can redistribute it            */
/* and/or modify it under the terms of the Modified BSD License, as           */
/* published by the Open Source Initiative.                                   */
/*                                                                            */
/* The UM Shared Library is distributed in the hope that it will be           */
/* useful, but WITHOUT ANY WARRANTY; without even the implied warranty        */
/* of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           */
/* Modified BSD License for more details.                                     */
/*                                                                            */
/* You should have received a copy of the Modified BSD License                */
/* along with the UM Shared Library.                                          */
/* If not, see <http://opensource.org/licenses/BSD-3-Clause>.                 */
/******************************************************************************/

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <inttypes.h>
#include "c_shum_thread_utils.h"

/******************************************************************************/
/* Prototypes                                                                 */
/******************************************************************************/

void c_test_returns_valid_lock      (bool *);
void c_test_invalid_lock_release    (bool *);
void c_test_create_and_release_lock (bool *);
void c_test_create_many_locks       (bool *);
void c_test_backfill_locks          (bool *);
void c_test_sweep_release_locks     (bool *);
void c_test_inpar                   (bool *, int64_t *);
void c_test_threadid                (bool *, int64_t *);
void c_test_threadflush             (bool *, volatile int64_t *);

/******************************************************************************/
/* tests                                                                      */
/******************************************************************************/

void c_test_returns_valid_lock(bool *test_ret)
{
  int64_t lock = -1;
  *test_ret = false;

  lock = f_shum_newLock();

  *test_ret = ( (lock>0)
                && (lock<INT64_MAX)
              );

}

/******************************************************************************/

void c_test_invalid_lock_release(bool *test_ret)
{
  int64_t lock = -1;
  int64_t res = 0;
  *test_ret = false;

  res = f_shum_releaseLock(&lock);

  *test_ret = (res!=0);

}

/******************************************************************************/

void c_test_create_and_release_lock(bool *test_ret)
{
  int64_t lock = -1;
  int64_t res1 = 0;
  int64_t res2 = 0;

  *test_ret = false;

  lock = f_shum_newLock();

  res1 = f_shum_releaseLock(&lock);
  res2 = f_shum_releaseLock(&lock);

  *test_ret = (res1==0 && res2!=0);

}

/******************************************************************************/

void c_test_create_many_locks(bool *test_ret)
{
  int64_t lock = -1;
  int i;

  *test_ret = true;

  for (i=0;i<1000;i++)
  {
    lock = f_shum_newLock();

    *test_ret = ( *test_ret
                  && (lock>0)
                  && (lock<INT64_MAX)
                );

    if (!*test_ret) break;
  }
}

/******************************************************************************/

void c_test_backfill_locks(bool *test_ret)
{
  int64_t lock[4] = {-1,-1,-1,-1};
  int64_t prev_lock = -1;
  int64_t res=0;
  int i;

  *test_ret = true;

  lock[0] = f_shum_newLock();

  for (i=1;i<4;i++)
  {
    lock[i] = f_shum_newLock();

    *test_ret = ( *test_ret
                  && (lock[i]>lock[i-1])
                );

    if (!*test_ret) break;
  }

  prev_lock = lock[3];

  res = f_shum_releaseLock(&lock[3]);

  *test_ret = *test_ret && (res==0);

  lock[3] = f_shum_newLock();

  *test_ret = *test_ret && (lock[3]==prev_lock);
}

/******************************************************************************/

void c_test_sweep_release_locks(bool *test_ret)
{
  int64_t res=0;
  int64_t i;
  int64_t max;

  *test_ret = true;

  max = f_shum_newLock();

  if (max<2500)
  {
    max=2500;
  }

  for (i=0;i<max;i++)
  {
    res = f_shum_releaseLock(&i);

    *test_ret = ( *test_ret
                  && (res==1||res==0)
                );

    if (!*test_ret) break;
  }

}

/******************************************************************************/

void c_test_inpar(bool *test_ret, int64_t *par_res)
{
  *par_res = f_shum_inPar();
  *test_ret = (*par_res==1 || *par_res==0);
}

/******************************************************************************/

void c_test_threadid(bool *test_ret, int64_t *tid)
{
  *tid = f_shum_threadID();
  *test_ret = (*tid>=0);
}

/******************************************************************************/

void c_test_threadflush(bool *test_ret, volatile int64_t *shared1)
{
  /* this is a dummy test for now */

  *test_ret = (*shared1==0);
}
