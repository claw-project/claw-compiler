package clawfc;

import java.lang.RuntimeException;
import java.lang.ProcessBuilder;

import java.util.logging.Logger;
import java.util.Scanner;

import java.nio.file.Files;

import java.io.InputStream;

import net.sourceforge.argparse4j.ArgumentParsers;
import net.sourceforge.argparse4j.helper.HelpScreenException;
import net.sourceforge.argparse4j.impl.Arguments;
import net.sourceforge.argparse4j.inf.ArgumentGroup;
import net.sourceforge.argparse4j.inf.ArgumentParser;
import net.sourceforge.argparse4j.inf.Namespace;
import net.sourceforge.argparse4j.inf.ArgumentParserException;

public class Driver
{
    private final static Logger LOGGER = Logger.getLogger(Driver.class.getName());

    public static void main(String[] args) throws Exception
    {
        Driver driver = new Driver();
        driver.verifyInstall();
        Namespace parsedArgs = parseCmdlineArguments(args);
        driver.execute(parsedArgs);
        System.exit(0);
    }

    final Configuration cfg;

    Driver() throws Exception
    {
        cfg = new Configuration();
    }

    static String getCmdOutput(String[] args) throws Exception
    {
        ProcessBuilder pb = new ProcessBuilder(args);
        pb.redirectErrorStream(true);
        Process p = pb.start();
        final int retCode = p.waitFor();
        if (retCode != 0)
        {
            throw new RuntimeException(
                    String.format("Cmd \"%s\" failed with return code ", String.join(" ", args), retCode));
        }
        String result = null;
        try (InputStream istrm = p.getInputStream(); Scanner s = new Scanner(istrm).useDelimiter("\\A"))
        {
            result = s.hasNext() ? s.next() : null;
        }
        return result;
    }

    void verifyInstall()
    {
        if (!Files.isDirectory(cfg.installRoot()))
        {
            LOGGER.severe(String.format("CLAW install directory \"%s\" does not exist or is not a directory",
                    cfg.installRoot()));
            System.exit(1);
        }
        if (!Files.isDirectory(cfg.omniInstallRoot()))
        {
            LOGGER.severe(
                    String.format("OMNI XCodeML Tools install directory \"%s\" does not exist or is not a directory",
                            cfg.omniInstallRoot()));
            System.exit(1);
        }
        if (!Files.isExecutable(cfg.omniFrontEnd()))
        {
            LOGGER.severe(
                    String.format("OMNI XCodeML Tools Fortran Frontend \"%s\" does not exist or is not a directory",
                            cfg.omniFrontEnd()));
            System.exit(1);
        }
        {
            String omniVersionTag = null;
            try
            {
                omniVersionTag = getCmdOutput(new String[] { cfg.omniFrontEnd().toString(), "--version-tag" });
            } catch (Exception e)
            {
                LOGGER.severe("Failed to get OMNI XCodeML Tools version: " + e.getMessage());
                System.exit(1);
            }
            if (!cfg.omniVersionTag().equals(omniVersionTag))
            {
                LOGGER.severe(String.format("OMNI XCodeML Tools version mismatch\n\texpected: \"%s\"\n\tgot: \"%s\"",
                        cfg.omniVersionTag(), omniVersionTag));
                System.exit(1);
            }
        }
    }

    void execute(Namespace parsedArgs)
    {
        if (parsedArgs.getBoolean("show_env"))
        {
            System.out.print(cfg.toString());
        }
        else if(parsedArgs.getBoolean("version"))
        {
            printVersion();            
        }
    }
    
    void printVersion()
    {
        String vStr = String.format("%s %s \"%s\" %s ", cfg.name(), cfg.version(), cfg.commit(), cfg.omniVersion());
        System.out.print(vStr);        
    }

    static Namespace parseCmdlineArguments(String[] args)
    {
        ArgumentParser parser = ArgumentParsers.newFor("clawfc").build().description("The CLAW Compiler is a "
                + "source-to-source translator working on the XcodeML intermediate representation");
        try
        {
            ArgumentGroup cOpts = parser.addArgumentGroup("Compiler options");
            cOpts.addArgument("-o", "--output-file")
                    .help("Output file for the transformed FORTRAN code. If not given, code is printed to stdout.");
            cOpts.addArgument("-I", "--include-dir").nargs("*")
                    .help("Add the directory dir to the search path for .mod and .xmod files.");
            cOpts.addArgument("-J", "--output-mod-dir").help("Output directory for compiled .mod and .xmod files.");
            cOpts.addArgument("-t", "--target").help("Type of accelerator directive language for code generation");
            cOpts.addArgument("--list-targets", "--target-list").action(Arguments.storeTrue())
                    .help("List the available type of accelerator directive language supported");
            cOpts.addArgument("-d", "--directive")
                    .help("Specify the type of accelerator directive language for code generation");
            cOpts.addArgument("--list-directives", "--directive-list").action(Arguments.storeTrue())
                    .help("List the available type of accelerator directive language supported");
            cOpts.addArgument("--config").help("Specify a different configuration for the translator");
            cOpts.addArgument("--show-config").action(Arguments.storeTrue())
                    .help("List the current configuration information."
                            + " If used with --config, list the information from the specific configuration");
            cOpts.addArgument("-m", "--model-config").help("Specific model configuration for SCA");
            cOpts.addArgument("-c", "--keep-comment").action(Arguments.storeTrue())
                    .help("Keep comments in the transformed file");
            cOpts.addArgument("-v", "--verbose").action(Arguments.storeTrue()).help("Print processing status");
            cOpts.addArgument("--version").action(Arguments.storeTrue()).help("Print version");
            cOpts.addArgument("--show-env").action(Arguments.storeTrue()).help("Show environment configuration");
            cOpts.addArgument("--no-dep").action(Arguments.storeTrue())
                    .help("Don't generate .mod or .xmod file for dependencies");
            cOpts.addArgument("--no-module-cache").action(Arguments.storeTrue())
                    .help("Deactivate module cache in the front-end");
            cOpts.addArgument("-f", "--force").action(Arguments.storeTrue())
                    .help("Force the translation of files without directives");
            cOpts.addArgument("--force-pure").action(Arguments.storeTrue())
                    .help("Force compiler to exit when transformation applied to PURE subroutine/function");
            cOpts.addArgument("--add-paren").action(Arguments.storeTrue())
                    .help("Add parenthesis to binary operation in generated code");
            cOpts.addArgument("-r", "--report").action(Arguments.storeTrue())
                    .help("Generate the transformation report");
            cOpts.addArgument("--debug").action(Arguments.storeTrue()).help("Display transformation debug information");
            cOpts.addArgument("--debug-omni").action(Arguments.storeTrue())
                    .help("Save intermediate files in __omni_tmp__ and display driver information");
            cOpts.addArgument("--debug-ffront").action(Arguments.storeTrue())
                    .help("Drive OMNI Fortran front-end in debug mode");
            cOpts.addArgument("--stop-pp").action(Arguments.storeTrue())
                    .help("Save intermediate files and stop after preprocess");
            cOpts.addArgument("--stop-dependencies").action(Arguments.storeTrue())
                    .help("Save intermediate files and stop after dependencies resolution");
            cOpts.addArgument("--stop-frontend").action(Arguments.storeTrue())
                    .help("Save intermediate files and stop after frontend");
            cOpts.addArgument("--stop-translator").action(Arguments.storeTrue())
                    .help("Save intermediate files and stop after translator");
            cOpts.addArgument("-x").nargs("*")
                    .help("Override a configuration key:value pair from the command line. Higher "
                            + "priority over base configuration and user configuration");
            ArgumentGroup dcOpts = parser.addArgumentGroup("Decompiler options");
            dcOpts.addArgument("-w").type(Integer.class)
                    .help("Set the number of columns for the output FORTRAN file (default: 80)");
            dcOpts.addArgument("-l").action(Arguments.storeTrue())
                    .help("Add preprocessor line directives in the output FORTRAN file");
            ArgumentGroup pOpts = parser.addArgumentGroup("Process options");
            pOpts.addArgument("--Wp").nargs("*").help("Add preprocessor option");
            pOpts.addArgument("--Wf").nargs("*").help("Add frontend option");
            pOpts.addArgument("--Wx").nargs("*").help("Add Xcode translator option");
            return parser.parseArgs(args);
        } catch (HelpScreenException hse)
        {
            System.exit(0);
        } catch (ArgumentParserException ape)
        {
            parser.handleError(ape);
            System.exit(1);
        }
        throw new RuntimeException("Unreachable code");
    }
};