package clawfc;

import java.lang.RuntimeException;
import java.lang.ProcessBuilder;

import java.util.logging.Logger;
import java.util.Scanner;
import java.util.ArrayList;
import java.util.Arrays;
import java.nio.file.Files;

import java.io.InputStream;

import net.sourceforge.argparse4j.ArgumentParsers;
import net.sourceforge.argparse4j.helper.HelpScreenException;
import net.sourceforge.argparse4j.impl.Arguments;
import net.sourceforge.argparse4j.inf.ArgumentGroup;
import net.sourceforge.argparse4j.inf.MutuallyExclusiveGroup;;
import net.sourceforge.argparse4j.inf.ArgumentParser;
import net.sourceforge.argparse4j.inf.Namespace;
import net.sourceforge.argparse4j.inf.ArgumentParserException;

import claw.ClawX2T;

public class Driver
{
    private final static Logger LOGGER = Logger.getLogger(Driver.class.getName());

    /*
     * class Options { boolean onlyPreprocess(); boolean enableCPreprocess();
     * boolean verbose(); boolean enableDebug(); boolean enableDebugOmni(); boolean
     * resolveDependencies(); boolean stopAfterPreprocess(); boolean
     * stopAfterFrontend(); boolean stopAfterTranslator(); boolean
     * stopAfterDependenciesResolution(); boolean forceTranslation(); boolean
     * listHWTargets(); boolean listDirectiveLanguages(); boolean show_config();
     * boolean user_target(); boolean user_config(); boolean model_config(); boolean
     * user_directive(); boolean line_directive(); boolean decompiler_max_column();
     * boolean dump_cx2t_args() boolean force_pure(); boolean report(); boolean
     * pipe_workflow(); boolean keep_comment(); boolean add_paren(); boolean
     * omni_ffront_debug(); boolean omni_ffront_no_module_cache(); };
     */

    /*
     * class Options { boolean only_pp = false; boolean enable_cpp = true; boolean
     * verbose = false; boolean enable_debug = false; boolean enable_debug_omni =
     * false; boolean resolve_dependencies = true; boolean stop_pp = false; boolean
     * stop_frontend = false; boolean stop_translator = false; boolean
     * stop_dependencies = false; boolean force_translation = false; boolean
     * list_target = false; boolean list_directive = false; boolean show_config =
     * false; boolean user_target = false; boolean user_config = false; boolean
     * model_config = false; boolean user_directive = false; boolean line_directive
     * = false; boolean decompiler_max_column = false; boolean dump_cx2t_args =
     * false boolean force_pure = false; boolean report = false; boolean
     * pipe_workflow = true; boolean keep_comment = false; boolean add_paren =
     * false; boolean omni_ffront_debug = false; boolean omni_ffront_no_module_cache
     * = false; };
     */

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

    void execute(Namespace parsedArgs) throws Exception
    {
        if (parsedArgs.getBoolean("show_env"))
        {
            System.out.print(cfg.toString());
        } else if (parsedArgs.getBoolean("version"))
        {
            printVersion();
        } else if(parsedArgs.getBoolean("list_targets"))
        {
            ClawX2T.main(new String[] {"--target-list"});
        } else if(parsedArgs.getBoolean("list_directives"))
        {
            ClawX2T.main(new String[] {"--directive-list"});
        } else if(parsedArgs.getBoolean("show_config"))
        {
            ArrayList<String> args = new ArrayList<String>(Arrays.asList("--show-config", "--config-path=" + cfg.configDir()));
            String cfg = parsedArgs.getString("config");
            if(cfg != null)
            { args.add("--config=" + cfg); }
            ClawX2T.main(args.stream().toArray(String[]::new));
        }
        else
        {}
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
            parser.addArgument("fortran-file").nargs("*").help("Input file");
            MutuallyExclusiveGroup qOpts = parser.addMutuallyExclusiveGroup("Query options");
            qOpts.addArgument("--list-targets", "--target-list").action(Arguments.storeTrue())
                    .help("List available types of accelerator hardware");
            qOpts.addArgument("--list-directives", "--directive-list").action(Arguments.storeTrue())
                    .help("List supported accelerator directive languages");
            qOpts.addArgument("--show-config").action(Arguments.storeTrue())
                    .help("List the current configuration information. If used with --config, list the information"
                            + " from the specific configuration");
            qOpts.addArgument("--version").action(Arguments.storeTrue()).help("Print version");
            qOpts.addArgument("--show-env").action(Arguments.storeTrue()).help("Show environment configuration");
            ArgumentGroup cOpts = parser.addArgumentGroup("Compiler options");
            cOpts.addArgument("-o", "--output-file")
                    .help("Output file for the transformed FORTRAN code. If not given, code is printed to stdout.");
            cOpts.addArgument("-I", "--include-dir").nargs("*")
                    .help("Add the directory dir to the search path for .mod and .xmod files.");
            cOpts.addArgument("-J", "--output-mod-dir").help("Output directory for compiled .mod and .xmod files.");
            cOpts.addArgument("-t", "--target").help("Type of target accelerator hardware");
            cOpts.addArgument("-d", "--directive")
                    .help("Specify accelerator directive language to be used for code generation");
            cOpts.addArgument("--config").help("Specify a different configuration for the translator");
            cOpts.addArgument("-m", "--model-config").help("Specific model configuration for SCA");
            cOpts.addArgument("-c", "--keep-comment").action(Arguments.storeTrue())
                    .help("Keep comments in the transformed file");
            cOpts.addArgument("-v", "--verbose").action(Arguments.storeTrue()).help("Print processing status");
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