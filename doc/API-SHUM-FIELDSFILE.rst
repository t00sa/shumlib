API Reference: shum_fieldsfile
------------------------------

Fortran Functions/Subroutines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

``get_shum_fieldsfile_version``
''''''''''''''''''''''''''''''''''

All Shumlib libraries expose a module and function named in this format; it 
allows access to the Shumlib version number used when compiling the library.

    **Available via module**
        ``f_shum_fieldsfile_version_mod``

    **Syntax**
        ``version = get_shum_fieldsfile_version()``

    **Returns**
        ``version (INTEGER)``
            Shumlib version number, in format ``YYYYMMX`` (the 4-digit year
            followed by 2-digit month and an additional digit signifying the
            release count within that specified month).

``f_shum_open_file``
''''''''''''''''''''

This function is used to open an existing FieldsFile (or variant thereof) - see
the routine ``f_shum_create_file`` for creating a new file.  

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_open_file(filename, ff_id, message)``

    **Inputs**
        ``filename (CHARACTER)``
            The path to the filename (which must already exist).

    **Outputs**
        ``ff_id (INTEGER)``
            A value which acts as an identifier for the file, the value 
            itself is arbitrary but must be passed to all other operations
            which need to reference the file opened here.

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

``f_shum_create_file``
''''''''''''''''''''''

This function is used to create a new FieldsFile (or variant thereof) - see
the routine ``f_shum_open_file`` for opening an existing file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_create_file(filename, n_lookups, ff_id, message)``

    **Inputs**
        ``filename (CHARACTER)``
            The path to the filename (which will be created).
        ``n_lookups (INTEGER)``
            The space reserved for the lookup table in this file will be set
            to be large enough to hold this number of lookup entries.  This
            cannot be changed once set for the lifetime of the file.
        ``overwrite (optional LOGICAL)``
            Determines whether the presence of an existing file with the given
            name should be treated as an error or not. If not provided the
            default behaviour is to allow overwriting an existing file.

    **Outputs**
        ``ff_id (INTEGER)``
            A value which acts as an identifier for the file, the value 
            itself is arbitrary but must be passed to all other operations
            which need to reference the file opened here.

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

``f_shum_close_file``
'''''''''''''''''''''

This function closes access to a previously opened file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_close_file(ff_id, message)``

    **Inputs**
        ``ff_id (INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

``f_shum_read_fixed_length_header``
'''''''''''''''''''''''''''''''''''

This function reads in and returns the fixed length header from the file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_fixed_length_header(ff_id, fixed_length_header, message)``

    **Inputs**
        ``ff_id (INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Outputs**
        ``fixed_length_header (64-bit INTEGER)``
            The fixed length header (always a 1D array with exactly
            256 elements).

    **Input & Output**
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

``f_shum_read_integer_constants``
'''''''''''''''''''''''''''''''''

This function reads in and returns the integer constants from the file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_integer_constants(ff_id, integer_constants, message)``

    **Inputs**
        ``ff_id (INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Input & Output**
        ``integer_constants (64-bit INTEGER)``
            The integer constants, a 1D ``ALLOCATABLE`` array which will become
            ``ALLOCATED`` to the correct size following the call (if it was 
            already ``ALLOCATED`` it will first be ``DEALLOCATED``).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.

``f_shum_read_real_constants``
''''''''''''''''''''''''''''''

This function reads in and returns the real constants from the file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_real_constants(ff_id, real_constants, message)``

    **Inputs**
        ``ff_id (INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Input & Output**
        ``real_constants (64-bit REAL)``
            The real constants, a 1D ``ALLOCATABLE`` array which will become
            ``ALLOCATED`` to the correct size following the call (if it was 
            already ``ALLOCATED`` it will first be ``DEALLOCATED``).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.

``f_shum_read_level_dependent_constants``
'''''''''''''''''''''''''''''''''''''''''

This function reads in and returns the level dependent constants from the file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_level_dependent_constants(ff_id, level_dependent_constants, message)``

    **Inputs**
        ``ff_id (INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Input & Output**
        ``level_dependent_constants (64-bit REAL)``
            The level dependent constants, a 2D ``ALLOCATABLE`` array which 
            will become ``ALLOCATED`` to the correct size following the call 
            (if it was already ``ALLOCATED`` it will first be ``DEALLOCATED``).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.

``f_shum_read_row_dependent_constants``
'''''''''''''''''''''''''''''''''''''''

This function reads in and returns the row dependent constants from the file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_row_dependent_constants(ff_id, row_dependent_constants, message)``

    **Inputs**
        ``ff_id (INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Input & Output**
        ``row_dependent_constants (64-bit REAL)``
            The row dependent constants, a 2D ``ALLOCATABLE`` array which 
            will become ``ALLOCATED`` to the correct size following the call 
            (if it was already ``ALLOCATED`` it will first be ``DEALLOCATED``).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.

``f_shum_read_column_dependent_constants``
''''''''''''''''''''''''''''''''''''''''''

This function reads in and returns the column dependent constants from the file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_column_dependent_constants(ff_id, column_dependent_constants, message)``

    **Inputs**
        ``ff_id (INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Input & Output**
        ``column_dependent_constants (64-bit REAL)``
            The column dependent constants, a 2D ``ALLOCATABLE`` array which 
            will become ``ALLOCATED`` to the correct size following the call 
            (if it was already ``ALLOCATED`` it will first be ``DEALLOCATED``).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.

``f_shum_read_additional_parameters``
'''''''''''''''''''''''''''''''''''''

This function reads in and returns the additional parameters from the file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_additional_parameters(ff_id, additional_parameters, message)``

    **Inputs**
        ``ff_id (INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Input & Output**
        ``column_dependent_constants (64-bit REAL)``
            The additional parameters, a 2D ``ALLOCATABLE`` array which 
            will become ``ALLOCATED`` to the correct size following the call 
            (if it was already ``ALLOCATED`` it will first be ``DEALLOCATED``).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.

``f_shum_read_extra_constants``
'''''''''''''''''''''''''''''''

This function reads in and returns the extra constants from the file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_extra_constants(ff_id, extra_constants, message)``

    **Inputs**
        ``ff_id (INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Input & Output**
        ``extra_constants (64-bit REAL)``
            The extra constants, a 1D ``ALLOCATABLE`` array which 
            will become ``ALLOCATED`` to the correct size following the call 
            (if it was already ``ALLOCATED`` it will first be ``DEALLOCATED``).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.

``f_shum_read_temp_histfile``
'''''''''''''''''''''''''''''

This function reads in and returns the temporary historyfile from the file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_temp_histfile(ff_id, temp_histfile, message)``

    **Inputs**
        ``ff_id (INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Input & Output**
        ``temp_histfile (64-bit REAL)``
            The temp_histfile, a 1D ``ALLOCATABLE`` array which 
            will become ``ALLOCATED`` to the correct size following the call 
            (if it was already ``ALLOCATED`` it will first be ``DEALLOCATED``).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.

``f_shum_read_compressed_index``
''''''''''''''''''''''''''''''''

This function reads in and returns the compressed indices from the file.

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_compressed_index(ff_id, compressed_index, index, message)``

    **Inputs**
        ``ff_id (INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.
        ``index (INTEGER)``
            Indicates which of the 3 compressed index headers should be returned
            (can take a value of 1, 2 or 3).

    **Input & Output**
        ``compressed_index (64-bit REAL)``
            The compressed index (one of 3 depending on the value of ``index``), 
            a 1D ``ALLOCATABLE`` array which will become ``ALLOCATED`` to the 
            correct size following the call (if it was already ``ALLOCATED`` 
            it will first be ``DEALLOCATED``).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.

``f_shum_read_lookup``
''''''''''''''''''''''

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_lookup(ff_id, lookup, message)``

    **Inputs**
        ``ff_id (INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.

    **Input & Output**
        ``lookup (64-bit REAL)``
            The lookup table, a 2D ``ALLOCATABLE`` array which will become 
            ``ALLOCATED`` to the correct size following the call (if it was 
            already ``ALLOCATED`` it will first be ``DEALLOCATED``).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.

``f_shum_read_field_data``
''''''''''''''''''''''''''

    **Available via module**
        ``f_shum_fieldsfile_mod``

    **Syntax**
        ``status = f_shum_read_field_data(ff_id, index, field_data, message, ignore_dtype)``

    **Inputs**
        ``ff_id (INTEGER)``
            The identifier for the file; this must be a value returned by an
            earlier call to ``f_shum_open_file`` or ``f_shum_create_file``.
        ``index (INTEGER)``
            Indicates the index into the lookup table containing the field data
            that should be returned.

    **Input & Output**
        ``field_data``
            The field data, a 1D ``ALLOCATABLE`` array which will become 
            ``ALLOCATED`` to the correct size following the call (if it was
            already ``ALLOCATED`` it will first be ``DEALLOCATED``). The type
            of ``field_data`` may be either ``INTEGER`` or ``REAL`` and can be
            either 32-bit or 64-bit.  Which combination of these is correct
            depends on the data and packing types of the field, which you can 
            determine by examining the lookup table yourself.  Passing a 
            ``field_data`` array that does not match the type and precision 
            indicated by the lookup will result in an error (*unless* the
            optional ``ignore_dtype`` flag is passed (see below).
        ``message (CHARACTER(LEN=*))``
            Error message buffer.
        ``ignore_dtype (optional, LOGICAL)``
            If provided and set to true (default if not provided is false) the
            type and precision of the ``field_data`` variable will be used 
            regardless of what the lookup specifies. This means that for example
            a field which should be 64-bit ``REAL`` data can be read into a 
            32-bit ``INTEGER`` array (as raw bytes; to retrieve the true 
            ``REAL`` values later each pair of values would need to be combined
            and then changed into a ``REAL`` representation using ``TRANSFER``)
            
    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything above ``0`` means an 
            error has occurred, and a value of ``-1`` means this component was
            not present in the file.  In both unsuccessful cases the ``message`` 
            argument will contain further information.


C Functions
%%%%%%%%%%%

``get_shum_fieldsfile_version``
''''''''''''''''''''''''''''''''''

All Shumlib libraries expose a function named in this format; it allows access
to the Shumlib version number used when compiling the library.

    **Required header/s**
        None - always defined provided any other part of library is included.

    **Syntax**
        ``version = get_shum_fieldsfile_version()``

    **Returns**
        ``version (int)``
            Shumlib version number, in format ``YYYYMMX`` (the 4-digit year
            followed by 2-digit month and an additional digit signifying the
            release count within that specified month).
