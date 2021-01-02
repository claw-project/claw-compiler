/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package claw.wani;

import static java.lang.String.format;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.sourceforge.argparse4j.ArgumentParsers;
import net.sourceforge.argparse4j.helper.HelpScreenException;
import net.sourceforge.argparse4j.impl.Arguments;
import net.sourceforge.argparse4j.inf.ArgumentGroup;
import net.sourceforge.argparse4j.inf.ArgumentParser;
import net.sourceforge.argparse4j.inf.ArgumentParserException;
import net.sourceforge.argparse4j.inf.MutuallyExclusiveGroup;
import net.sourceforge.argparse4j.inf.Namespace;

public class CLIOptions extends ConfigurationOptions
{
    public static final Path STARTUP_DIR = Paths.get(System.getProperty("user.dir"));

    final boolean _printOpts;
    final boolean _printVersion;
    final boolean _printTargets;
    final boolean _printDirectives;
    final boolean _printCfg;
    final Path _transReportFile;
    final List<Path> _modIncDirs;
    final Path _inputFile;
    final Path _outputTxastFile;
    final Path _outputSrcFile;

    public boolean printOptions()
    {
        return _printOpts;
    }

    public boolean printVersion()
    {
        return _printVersion;
    }

    public boolean printTargets()
    {
        return _printTargets;
    }

    public boolean printDirectives()
    {
        return _printDirectives;
    }

    public boolean printCfg()
    {
        return _printCfg;
    }

    public Path translationReportFilePath()
    {
        return _transReportFile;
    }

    public List<Path> moduleIncludeDirs()
    {
        return _modIncDirs;
    }

    public Path inputFilePath()
    {
        return _inputFile;
    }

    public Path outputTranslatedXastFilePath()
    {
        return _outputTxastFile;
    }

    public Path outputFortranFilePath()
    {
        return _outputSrcFile;
    }

    CLIOptions(Namespace parsedArgs)
    {
        super(parsedArgs.getString("target"), getOptionalPath(parsedArgs, "config"),
                getOptionalPath(parsedArgs, "config_path"), parsedArgs.getString("directive"),
                getOptionalPath(parsedArgs, "model_config"), getStringList(parsedArgs, "override_cfg_key"),
                parsedArgs.getBoolean("debug"), parsedArgs.getInt("max_fortran_line_length"),
                parsedArgs.getBoolean("suppress_pp_line_directives"), parsedArgs.getBoolean("force_pure"),
                parsedArgs.getBoolean("add_paren"));
        _printVersion = parsedArgs.getBoolean("version");
        _printTargets = parsedArgs.getBoolean("target_list");
        _printDirectives = parsedArgs.getBoolean("directive_list");
        _printCfg = parsedArgs.getBoolean("show_config");
        _printOpts = parsedArgs.getBoolean("print_opts");
        _transReportFile = getOptionalPath(parsedArgs, "report");
        _modIncDirs = getPathList(parsedArgs, "mod_include_dir");
        _inputFile = getOptionalPath(parsedArgs, "input_xast_file");
        _outputTxastFile = getOptionalPath(parsedArgs, "out_xast_file");
        _outputSrcFile = getOptionalPath(parsedArgs, "out_ftn_file");
    }

    public static CLIOptions parseArguments(String[] args) throws Exception
    {
        ArgumentParser parser = ArgumentParsers.newFor("claw-cx2t").build().description("The CLAW Compiler is a "
                + "source-to-source translator working on the XcodeML intermediate representation");
        Namespace parsedArgs = null;
        try
        {
            parser.addArgument("input-xast-file").nargs("?").help("Input XCodeML file");
            MutuallyExclusiveGroup qOpts = parser.addMutuallyExclusiveGroup("Query options");
            qOpts.addArgument("--print-opts").action(Arguments.storeTrue()).help("Print processed cmdline options");
            qOpts.addArgument("--version").action(Arguments.storeTrue()).help("Print version");
            qOpts.addArgument("-tl", "--target-list").action(Arguments.storeTrue())
                    .help("List all targets supported by code transformation");
            qOpts.addArgument("-dl", "--directive-list").action(Arguments.storeTrue())
                    .help("List all directive languages supported by code generation");
            qOpts.addArgument("-sc", "--show-config").action(Arguments.storeTrue())
                    .help("Display the current configuration");
            ArgumentGroup cOpts = parser.addArgumentGroup("Translator options");
            cOpts.addArgument("-l", "--suppress-pp-line-directives").action(Arguments.storeTrue())
                    .help("Suppress line directive in decompiled code");
            cOpts.addArgument("-cp", "--config-path").help("Path to configuration directory");
            cOpts.addArgument("-c", "--config").help("Path to translator configuration file");
            cOpts.addArgument("-t", "--target").help("Code transformation target platform");
            cOpts.addArgument("-dir", "--directive").help("Target directive language to be used in code generation");
            cOpts.addArgument("-d", "--debug").action(Arguments.storeTrue()).help("Enable output debug message");
            cOpts.addArgument("-f", "--out-ftn-file").help("Output file for decompiled FORTRAN source");
            cOpts.addArgument("-o", "--out-xast-file").help("Output file for transformed XcodeML");
            cOpts.addArgument("-r", "--report").help("Output file for the transformation report");
            cOpts.addArgument("-M", "--mod-include-dir").nargs("*").action(Arguments.append())
                    .help("Search directory for .xmod files");
            cOpts.addArgument("-w", "--max-fortran-line-length").type(Integer.class)
                    .help("Number of character per line in decompiled code");
            cOpts.addArgument("-fp", "--force-pure").action(Arguments.storeTrue())
                    .help("Exit the translator if a PURE subroutine/function has to be transformed");
            cOpts.addArgument("-m", "--model-config").help("Model configuration file for SCA transformation");
            cOpts.addArgument("-x", "--override-cfg-key").nargs("*").action(Arguments.append())
                    .help("Override configuration option. Has higher priority than base and user configuration");
            cOpts.addArgument("-ap", "--add-paren").action(Arguments.storeTrue())
                    .help("Force backend to add parentheses around binary mathematical operations");
            parsedArgs = parser.parseArgs(args);
        } catch (HelpScreenException hse)
        {
            return null;
        } catch (ArgumentParserException ape)
        {
            parser.handleError(ape);
            throw ape;
        }
        CLIOptions opts = new CLIOptions(parsedArgs);
        return opts;
    }

    @Override
    public String toString()
    {
        StringBuilder res = new StringBuilder();
        res.append(format("Print options: %s\n", printOptions()));
        res.append(format("Print install version: %s\n", printVersion()));
        res.append(format("Print supported targets: %s\n", printTargets()));
        res.append(format("Print supported accelerator directive languages: %s\n", printDirectives()));
        res.append(format("Print configuration: %s\n", printCfg()));
        res.append(format("Input file: %s\n", inputFilePath()));
        res.append(format("Output FORTRAN file: %s\n", outputFortranFilePath()));
        res.append(format("Output XCodeML AST file: %s\n", outputTranslatedXastFilePath()));
        res.append(format("Output translation report file: %s\n", translationReportFilePath()));
        res.append("Module include directories: \n" + toString(moduleIncludeDirs()));
        res.append(super.toString());
        return res.toString();
    }

    static Path getOptionalPath(Namespace parsedArgs, String name)
    {
        String str = parsedArgs.getString(name);
        Path path = null;
        if (str != null)
        {
            path = toAbsPath(str);
        }
        return path;
    }

    List<Path> getPathList(Namespace parsedArgs, String name)
    {
        List<Path> res = new ArrayList<Path>();
        List<List<String>> strs = parsedArgs.<List<String>>getList(name);
        if (strs != null)
        {
            for (List<String> lstStr : strs)
            {
                for (String s : lstStr)
                {
                    res.add(toAbsPath(s));
                }
            }
        }
        return Collections.unmodifiableList(res);
    }

    static List<String> getStringList(Namespace parsedArgs, String name)
    {
        List<String> res = new ArrayList<String>();
        List<List<String>> strs = parsedArgs.<List<String>>getList(name);
        if (strs != null)
        {
            for (List<String> lstStr : strs)
            {
                for (String s : lstStr)
                {
                    res.add(s);
                }
            }
        }
        return Collections.unmodifiableList(res);
    }

    static Path toAbsPath(String pathStr)
    {
        Path path = Paths.get(pathStr);
        if (!path.isAbsolute())
        {
            path = STARTUP_DIR.resolve(path);
        }
        return path;
    }
}
