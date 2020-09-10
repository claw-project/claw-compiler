package clawfc;

import net.sourceforge.argparse4j.*;
import net.sourceforge.argparse4j.inf.*;
import net.sourceforge.argparse4j.helper.*;

public class Driver
{
    public static void main(String[] args) throws Exception
    {
        try
        {
            ArgumentParser parser = ArgumentParsers.newFor("clawfc").build().description("The CLAW Compiler is a " +
                    "source-to-source translator working on the XcodeML intermediate representation");
            ArgumentGroup cOpts = parser.addArgumentGroup("Compiler options");
            parser.parseArgs(args);
        }
        catch(HelpScreenException hse)
        {
            System.exit(0);
        }
        /*parser = argparse.ArgumentParser(description='CLAW')
        c_opts = parser.add_argument_group('Compiler options')
        c_opts.add_argument('-o', '--output-file', type=str,
            help='Output file for the transformed FORTRAN code. If not given, code is printed to stdout.')
        c_opts.add_argument('-I', '--include-dir', type=str, nargs='*',
            help='Add the directory dir to the search path for .mod and .xmod files.')
        c_opts.add_argument('-J', '--output-mod-dir', type=str, help='Output directory for ccompiled .mod and .xmod files.')
        c_opts.add_argument('-O', '--output-dir', type=str, help='Output directory for transformed files.')
        c_opts.add_argument('-t', '--target', type=str, help='Type of accelerator directive language for code generation')
        c_opts.add_argument('--list-targets', '--target-list', action='store_true',
            help='List the available type of accelerator directive language supported')
        c_opts.add_argument('-d', '--directive', type=str,
            help='Specify the type of accelerator directive language for code generation')
        c_opts.add_argument('--list-directives', '--directive-list', action='store_true',
            help='List the available type of accelerator directive language supported')
        c_opts.add_argument('--config', type=str, help='specify a different configuration for the translator')
        c_opts.add_argument('--show-config', action='store_true',
            help='List the current configuration information. If used with --config, list the information from \
                  the specific configuration')
        c_opts.add_argument('-m', '--model-config', type=str, help='Specific model configuration for SCA')
        c_opts.add_argument('-c', '--keep-comment', action='store_true', help='Keep comments in the transformed file')
        c_opts.add_argument('-v', '--verbose', action='store_true', help='Print processing status')
        c_opts.add_argument('--version', action='store_true', help='Print version')
        c_opts.add_argument('--show-env', action='store_true', help='Show environment variables')
        c_opts.add_argument('--no-dep', action='store_true', help="Don't generate .mod or .xmod file for dependencies")
        c_opts.add_argument('--no-module-cache', action='store_true', help='Deactivate module cache in the front-end')
        c_opts.add_argument('-f', '--force', action='store_true', help='Force the translation of files without directives')
        c_opts.add_argument('--force-pure', action='store_true',
                            help='Force compiler to exit when transformation applied to PURE subroutine/function')
        c_opts.add_argument('--add-paren', action='store_true',
                            help='Add parenthesis to binary operation in generated code')
        c_opts.add_argument('-r', '--report', action='store_true', help='Generate the transformation report')
        c_opts.add_argument('--debug', action='store_true', help='Display transformation debug information')
        c_opts.add_argument('--debug-omni', action='store_true',
                            help='Save intermediate files in __omni_tmp__ and display driver information')
        c_opts.add_argument('--debug-ffront', action='store_true', help='Drive OMNI Fortran front-end in debug mode')
        c_opts.add_argument('--stop-pp', action='store_true', help='Save intermediate files and stop after preprocess')
        c_opts.add_argument('--stop-dependencies', action='store_true',
                            help='Save intermediate files and stop after dependencies resolution')
        c_opts.add_argument('--stop-frontend', action='store_true',
                            help='Save intermediate files and stop after frontend')
        c_opts.add_argument('--stop-translator', action='store_true',
                            help='Save intermediate files and stop after translator')
        c_opts.add_argument('-x', type=str, nargs='*', help='Override a configuration key:value pair from the command '
                            'line. Higher priority over base configuration and user configuration')
        dc_opts = parser.add_argument_group('Decompiler options')
        dc_opts.add_argument('-w', type=int, help='Set the number of columns for the output FORTRAN file (default: 80)')
        dc_opts.add_argument('-l', action='store_true', help='Add preprocessor line directives in the output FORTRAN file')
        p_opts = parser.add_argument_group('Process options')
        p_opts.add_argument('--Wp', type=str, nargs='*', help='Add preprocessor option')
        p_opts.add_argument('--Wf', type=str, nargs='*', help='Add frontend option')
        p_opts.add_argument('--Wx', type=str, nargs='*', help='Add Xcode translator option')
        p_args = parser.parse_args()*/
        System.exit(0);
    }
};