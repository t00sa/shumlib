Setup/Installation
------------------

This part of the Shumlib documentation details the build process from the
perspective of a user: how to build Shumlib so that you can start using it in
your applications.  For a greater level of detail and information on
*developing* Shumlib itself see the developer information (develop.rst).

Prerequisites
*************

 - Either "make" or any tool supporting standard make syntax (e.g. "gmake").

 - A Fortran compiler capable of (at least) the 2003 standard.

 - A C compiler capable of (at least) the c99 standard.

Configuration files
*******************

Building Shumlib first requires a suitable make configuration file; to define
the tools, compilers and flags for your platform/architecture. Sites where the
UM is routinely built have their own sets of these configurations in the
toplevel ``make`` directory, and these files are named (by convention):

.. parsed-literal::

    <site_identifier>-<platform>-<fortran_compiler>-<c_compiler>.mk

So for example the file ``meto-x86-ifort-gcc.mk`` is a configuration for a
standard Linux system at the UK Met Office using the Intel Fortran compiler and
GNU C compiler. If you are building Shumlib at a UM partner site you may find a
configuration for your site already exists, otherwise you can create a new file
or try using an existing file that appears to be a close match for your own
platform/architecture.

Either way the existing configuration files are liberally commented to indicate 
what each value represents - making any of them a good starting point to base
your own file on. You should assume that all of the available values must be 
defined (but some may be left blank).

Running Make
************

Once you have a suitable configuration file, you can build the libraries by
passing the file to make, e.g.

.. parsed-literal::

    make -f <configuration> [library_name(s)]

Where ``<configuration>`` gives the full path to the configuration file you wish
to build and a ``library_name`` refers to any of the library sub-directories
starting with ``shum_*`` e.g. ``shum_wgdos_packing``.  Note that these trailing
arguments are optional - if you omit them the default behaviour will be to
compile all available libraries. Otherwise only named libraries *and any
libraries on which they depend* will be compiled (so for instance if the
argument ``shum_wgdos_pack`` is passed this will also compile
``shum_string_conv``, since the packing library makes use of it).

So for example, to compile the WGDOS packing library and its dependent
libraries using the ``meto-x86-ifort-gcc`` config you would run the command:

.. parsed-literal::

    make -f make/meto-x86-ifort-gcc.mk shum_wgdos_packing


Build Location
%%%%%%%%%%%%%%

If the build is successful the output will appear (by default) in the folder
``build/<platform>`` where ``<platform>`` is the value of the ``PLATFORM`` variable
set at the end of the configuration file (by convention this is the same as the
configuration filename without the ``.mk`` extension).

You can control the build destination using the following environment variables:

 - ``LIBDIR_ROOT``: this sets the base directory for the install (i.e. not
   including the ``<platform>`` part above, which will be appended as a
   subdirectory); the default for this is ``$PWD/build``.

 - ``LIBDIR_OUT``: this sets the *entire* directory for the install (i.e. the
   value of ``<platform>`` above will be disregarded and your exact name will
   be used); the default for this is ``$LIBDIR_ROOT/$PLATFORM``.

FRUIT Testing
%%%%%%%%%%%%%

Shumlib also includes a series of unit-tests, to confirm that the runtime
behaviour of the libraries is as expected. To build and run these tests you must
first build at least one library (see above) and then re-run the make command
with the single argument ``test``; e.g:

.. parsed-literal::

  make -f <configuration> test

This will compile the FRUIT tests (FRUIT is an external unit testing framework
distributed with Shumlib) and then execute them and display the results. You
will notice that the test output appears twice - this is because each library is
also built twice; once as a dynamic library and once as a static library, and
these are tested separately.

Note that any additional environment variables passed to the initial build must
also be passed to the above command (e.g. if you specified an alternative
``LIBDIR_OUT`` you should specify it again for the testing).

Cleanup
%%%%%%%

Like most make-based systems you may pass commands to either remove *all* build
output, or only intermediate build output (e.g. object files).  Note that since
the paths depend on the configuration files these must still be specified, and
in the case of cleaning the build output, only the specified configuration
output will be cleaned.  The make targets are invoked as follows:

 - ``make -f <configuration> clean``: to completely remove *all* build output
   including the produced libraries and test executables.

 - ``make -f <configuration> clean-temp``: to *only* remove intermediate files but
   leave the build output itself in place.

OpenMP Control
%%%%%%%%%%%%%%

By default Shumlib will try to build libraries with OpenMP support enabled
(assuming the compiler and configuration file support the flags
correctly). This will require the same OpenMP flags to be provided to the linker
when building an application that links against a Shumlib library. For
convenience the inclusion of the OpenMP flags can be toggled using the
environment variable ``SHUM_OPENMP``.  If set to "false" the resulting build will
not include the OpenMP flags specified in the configuration file.

In the case where you require *both* an OpenMP and no-OpenMP build you can use
this option in conjunction with the build location options (see above) to
produce multiple output directories.  For instance suppose we are building
multiple libraries for and wish to install to a non-default location:

.. parsed-literal:: 

    export LIBDIR_OUT=/home/wilfred/shumlib/openmp
    export SHUM_OPENMP=true
    make -f <configuration>
    make -f <configuration> clean-temp

    export LIBDIR_OUT=/home/wilfred/shumlib/no-openmp
    export SHUM_OPENMP=false
    make -f <configuration>
    make -f <configuration> clean-temp

This would produce the libraries twice, allowing including applications to link
to the appropriate version depending on their own OpenMP status.
