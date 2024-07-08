(*  Main module that starts the compilation pipeline. 

    This is a command line application that takes arguments from the standard input and outputs an entropy compiled
    binary. This module only uses the compiler API to start a compilation pipeline. Execution options for 'entc' compiler are defined below:

    --------------------------------- Use ---------------------------------

    entc <file1> [<file2>] ... (FLAGS)

    -------------------------------- Flags --------------------------------

    -o [NAME]   --output            Output binary name with path. If this flag is not used, an default 'out' binary
                                    will be created in the same folder where the command was executed.
    -v          --version           Prints the current compiler version and exit.
    -d          --debug             Prints all debug messages while compiling.
    -h          --help              Prints this help text and exit.
    -W          --warn-as-errors    Interprets all warnings as errors and exits when such appear.
   *)
