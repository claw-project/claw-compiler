package clawfc;

import net.sourceforge.argparse4j.*;
import net.sourceforge.argparse4j.inf.*;
import net.sourceforge.argparse4j.helper.*;
import net.sourceforge.argparse4j.impl.Arguments;

public class Driver
{
    public static void main(String[] args) throws Exception
    {
        try
        {
            ArgumentParser parser = ArgumentParsers.newFor("clawfc").build().description("The CLAW Compiler is a " +
                    "source-to-source translator working on the XcodeML intermediate representation");
            ArgumentGroup cOpts = parser.addArgumentGroup("Compiler options");
            cOpts.addArgument("-o", "--output-file").help(
            		"Output file for the transformed FORTRAN code. If not given, code is printed to stdout.");
            cOpts.addArgument("-I", "--include-dir").nargs("*").help(
            		"Add the directory dir to the search path for .mod and .xmod files.");
            cOpts.addArgument("-J", "--output-mod-dir").help("Output directory for compiled .mod and .xmod files.");
            cOpts.addArgument("-t", "--target").help("Type of accelerator directive language for code generation");
            cOpts.addArgument("--list-targets", "--target-list").action(Arguments.storeTrue()).help(
            		"List the available type of accelerator directive language supported");
            cOpts.addArgument("-d", "--directive").help(
            		"Specify the type of accelerator directive language for code generation");
            cOpts.addArgument("--list-directives", "--directive-list").action(Arguments.storeTrue()).help(
            		"List the available type of accelerator directive language supported");
            cOpts.addArgument("--config").help("Specify a different configuration for the translator");
            cOpts.addArgument("--show-config").action(Arguments.storeTrue()).help("List the current configuration information."
            		+ " If used with --config, list the information from the specific configuration");
            cOpts.addArgument("-m", "--model-config").help("Specific model configuration for SCA");
            cOpts.addArgument("-c", "--keep-comment").action(Arguments.storeTrue()).help("Keep comments in the transformed file");
            cOpts.addArgument("-v", "--verbose").action(Arguments.storeTrue()).help("Print processing status");
            cOpts.addArgument("--version").action(Arguments.storeTrue()).help("Print version");
            cOpts.addArgument("--show-env").action(Arguments.storeTrue()).help("Show environment variables");
            cOpts.addArgument("--no-dep").action(Arguments.storeTrue()).help("Don't generate .mod or .xmod file for dependencies");
            cOpts.addArgument("--no-module-cache").action(Arguments.storeTrue()).help("Deactivate module cache in the front-end");
            cOpts.addArgument("-f", "--force").action(Arguments.storeTrue()).help(
            		"Force the translation of files without directives");
            cOpts.addArgument("--force-pure").action(Arguments.storeTrue()).help(
            		"Force compiler to exit when transformation applied to PURE subroutine/function");
            cOpts.addArgument("--add-paren").action(Arguments.storeTrue()).help(
            		"Add parenthesis to binary operation in generated code");
            cOpts.addArgument("-r", "--report").action(Arguments.storeTrue()).help("Generate the transformation report");
            cOpts.addArgument("--debug").action(Arguments.storeTrue()).help("Display transformation debug information");
            cOpts.addArgument("--debug-omni").action(Arguments.storeTrue()).help(
            		"Save intermediate files in __omni_tmp__ and display driver information");
            cOpts.addArgument("--debug-ffront").action(Arguments.storeTrue()).help("Drive OMNI Fortran front-end in debug mode");
            cOpts.addArgument("--stop-pp").action(Arguments.storeTrue()).help("Save intermediate files and stop after preprocess");
            cOpts.addArgument("--stop-dependencies").action(Arguments.storeTrue()).help(
            		"Save intermediate files and stop after dependencies resolution");
            cOpts.addArgument("--stop-frontend").action(Arguments.storeTrue()).help(
            		"Save intermediate files and stop after frontend");
            cOpts.addArgument("--stop-translator").action(Arguments.storeTrue()).help(
            		"Save intermediate files and stop after translator");
            cOpts.addArgument("-x").nargs("*").help("Override a configuration key:value pair from the command line. Higher "
            		+ "priority over base configuration and user configuration");
            ArgumentGroup dcOpts = parser.addArgumentGroup("Decompiler options");
            dcOpts.addArgument("-w").type(Integer.class).help("Set the number of columns for the output FORTRAN file (default: 80)");
            dcOpts.addArgument("-l").action(Arguments.storeTrue()).help(
            		"Add preprocessor line directives in the output FORTRAN file");
            ArgumentGroup pOpts = parser.addArgumentGroup("Process options");
            pOpts.addArgument("--Wp").nargs("*").help("Add preprocessor option");
            pOpts.addArgument("--Wf").nargs("*").help("Add frontend option");
            pOpts.addArgument("--Wx").nargs("*").help("Add Xcode translator option");
            parser.parseArgs(args);
        }
        catch(HelpScreenException hse)
        {
            System.exit(0);
        }
        System.exit(0);
    }
};