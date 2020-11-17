/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import net.sourceforge.argparse4j.ArgumentParsers;
import net.sourceforge.argparse4j.helper.HelpScreenException;
import net.sourceforge.argparse4j.impl.Arguments;
import net.sourceforge.argparse4j.inf.ArgumentGroup;
import net.sourceforge.argparse4j.inf.ArgumentParser;
import net.sourceforge.argparse4j.inf.ArgumentParserException;
import net.sourceforge.argparse4j.inf.MutuallyExclusiveGroup;
import net.sourceforge.argparse4j.inf.Namespace;

public class Options
{
    public static final List<String> FC_COMPILER_TYPES = Collections
            .unmodifiableList(Arrays.asList("Cray", "Intel", "GNU", "PGI", "NAG"));
    final boolean _printInstallCfg;
    final boolean _printVersion;
    final boolean _printTargets;
    final boolean _printDirectives;
    final boolean _printCfg;
    final boolean _printOpts;
    final List<Path> _inputFiles;
    final Path _outputFile;
    final Path _outputDir;
    final Path _outputModDir;
    final String _userTarget;
    final List<Path> _incDirs;
    final List<String> _addMacros;
    final String _accDirLanguage;
    final String _configFile;
    final String _modelConfigFile;
    final List<String> _cfgKeyOverrides;
    final boolean _disableMP;
    final boolean _onlyPreprocess;
    final boolean _verbose;
    final boolean _keepComments;
    final boolean _forceTranslation;
    final boolean _resolveDependencies;
    final boolean _showDebugOutput;
    final boolean _keepIntFiles;
    final Path _intDir;
    final boolean _debugOmniFFront;
    final boolean _disableFFrontModuleCache;
    final boolean _skipPP;
    final boolean _stopAfterPP;
    final boolean _stopAfterFFront;
    final boolean _stopAfterDepRes;
    final boolean _stopAfterTrans;
    final List<String> _ppOpts;
    final List<String> _ffrontOpts;
    final List<String> _transOpts;
    final Integer _maxFLineLength;
    final boolean _addPPLineDirectives;
    final boolean _dumpCX2TArgs;
    final boolean _exitOnPureFunction;
    final boolean _addParenToBinOpts;
    final boolean _genTransReport;
    final String _fCompilerType;
    final String _fCompilerCmd;

    public boolean printInstallCfg()
    {
        return _printInstallCfg;
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

    public boolean printOptions()
    {
        return _printOpts;
    }

    public boolean disableMultiprocessing()
    {
        return _disableMP;
    }

    public List<Path> inputFiles()
    {
        return _inputFiles;
    }

    public Path outputFile()
    {
        return _outputFile;
    }

    public Path outputDir()
    {
        return _outputDir;
    }

    public Path outputModulesDir()
    {
        return _outputModDir;
    }

    public String userTarget()
    {
        return _userTarget;
    }

    public List<Path> includeDirs()
    {
        return _incDirs;
    }

    public List<String> predefinedMacros()
    {
        return _addMacros;
    }

    public String acceleratorDirectiveLanguage()
    {
        return _accDirLanguage;
    }

    public boolean onlyPreprocess()
    {
        return _onlyPreprocess;
    }

    public boolean verbose()
    {
        return _verbose;
    }

    public boolean keepComments()
    {
        return _keepComments;
    }

    public boolean forceTranslation()
    {
        return _forceTranslation;
    }

    public boolean resolveDependencies()
    {
        return _resolveDependencies;
    }

    public boolean showDebugOutput()
    {
        return _showDebugOutput;
    }

    public boolean keepIntermediateFiles()
    {
        return _keepIntFiles;
    }

    public Path intermediateFilesDir()
    {
        return _intDir;
    }

    public boolean debugOmniFFront()
    {
        return _debugOmniFFront;
    }

    public boolean disableOmniFFrontModuleCache()
    {
        return _disableFFrontModuleCache;
    }

    public boolean skipPreprocessing()
    {
        return _skipPP;
    }

    public boolean stopAfterPreprocessing()
    {
        return _stopAfterPP;
    }

    public boolean stopAfterOmniFFront()
    {
        return _stopAfterFFront;
    }

    public boolean stopAfterDepResolution()
    {
        return _stopAfterDepRes;
    }

    public boolean stopAfterTranslation()
    {
        return _stopAfterTrans;
    }

    public List<String> preprocessorOptions()
    {
        return _ppOpts;
    }

    public List<String> OmniFFrontOptions()
    {
        return _ffrontOpts;
    }

    public List<String> translatorOptions()
    {
        return _transOpts;
    }

    public String configFile()
    {
        return _configFile;
    }

    public String modelConfigFile()
    {
        return _modelConfigFile;
    }

    public List<String> cfgKeysOverrides()
    {
        return _cfgKeyOverrides;
    }

    public Integer maxFortranLineLength()
    {
        return _maxFLineLength;
    }

    public boolean addPreprocLineDirectives()
    {
        return _addPPLineDirectives;
    }

    public boolean dumpCX2TArgs()
    {
        return _dumpCX2TArgs;
    }

    public boolean exitOnPureFunction()
    {
        return _exitOnPureFunction;
    }

    public boolean addParenToBinaryOpts()
    {
        return _addParenToBinOpts;
    }

    public boolean genTransReport()
    {
        return _genTransReport;
    }

    public String fortranCompilerType()
    {
        return _fCompilerType;
    }

    public String fortranCompilerCmd()
    {
        return _fCompilerCmd;
    }

    public static Options parseCmdlineArguments(String[] args) throws Exception
    {
        ArgumentParser parser = ArgumentParsers.newFor("clawfc").build().description("The CLAW Compiler is a "
                + "source-to-source translator working on the XcodeML intermediate representation");
        Namespace parsedArgs = null;
        try
        {
            parser.addArgument("fortran-file").nargs("*").action(Arguments.append()).help("Input file");
            MutuallyExclusiveGroup qOpts = parser.addMutuallyExclusiveGroup("Query options");
            qOpts.addArgument("--list-targets", "--target-list").action(Arguments.storeTrue())
                    .help("List available types of accelerator hardware");
            qOpts.addArgument("--list-directives", "--directive-list").action(Arguments.storeTrue())
                    .help("List supported accelerator directive languages");
            qOpts.addArgument("--show-config").action(Arguments.storeTrue())
                    .help("List the current configuration information. If used with --config, list the information"
                            + " from the specific configuration");
            qOpts.addArgument("--version").action(Arguments.storeTrue()).help("Print version");
            qOpts.addArgument("--print-install-cfg", "--show-env").action(Arguments.storeTrue())
                    .help("Print install configuration");
            qOpts.addArgument("--print-opts").action(Arguments.storeTrue()).help("Print processed cmdline options");
            MutuallyExclusiveGroup outOpts = parser.addMutuallyExclusiveGroup("Compiler output options");
            outOpts.addArgument("-o", "--output-file")
                    .help("Output file for the transformed FORTRAN code. If not given, code is printed to stdout.");
            outOpts.addArgument("-O", "--output-dir").help("Output directory for transformed FORTRAN files");
            ArgumentGroup cOpts = parser.addArgumentGroup("Compiler options");
            cOpts.addArgument("-I", "--include-dir").nargs("*").action(Arguments.append())
                    .help("Add the directory dir to the search path for .mod and .xmod files.");
            cOpts.addArgument("-D", "--add-macro").nargs("*").action(Arguments.append()).help("Predefine macro");
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
            cOpts.addArgument("--int-dir")
                    .help("Path to intermediate files directory (all existing contents will be removed)");
            cOpts.addArgument("--keep-int-files").action(Arguments.storeTrue()).help("Keep intermediate files");
            cOpts.addArgument("--debug-ffront").action(Arguments.storeTrue())
                    .help("Drive OMNI Fortran front-end in debug mode");
            cOpts.addArgument("--stop-pp").action(Arguments.storeTrue())
                    .help("Save intermediate files and stop after preprocess");
            cOpts.addArgument("--skip-pp").action(Arguments.storeTrue())
                    .help("Do not apply preprocessing to input and include files");
            cOpts.addArgument("--stop-dependencies").action(Arguments.storeTrue())
                    .help("Save intermediate files and stop after dependencies resolution");
            cOpts.addArgument("--stop-frontend").action(Arguments.storeTrue())
                    .help("Save intermediate files and stop after frontend");
            cOpts.addArgument("--stop-translator").action(Arguments.storeTrue())
                    .help("Save intermediate files and stop after translator");
            cOpts.addArgument("-x", "--override-cfg-key").nargs("*").action(Arguments.append())
                    .help("Override a configuration key:value pair from the command line. Higher "
                            + "priority over base configuration and user configuration");
            cOpts.addArgument("--dump-cx2t-args").action(Arguments.storeTrue()).help("Print arguments passed to CX2T");
            cOpts.addArgument("--disable-mp").action(Arguments.storeTrue()).help("Disable multiprocessing");
            ArgumentGroup dcOpts = parser.addArgumentGroup("Decompiler options");
            dcOpts.addArgument("--max-fortran-line-length", "-w").type(Integer.class)
                    .help("Set the number of columns for the output FORTRAN file (default: 80)");
            dcOpts.addArgument("-l", "--add-pp-line-directives").action(Arguments.storeTrue())
                    .help("Add preprocessor line directives in the output FORTRAN file");
            cOpts.addArgument("-E", "--only-preprocess").action(Arguments.storeTrue())
                    .help("Only apply preprocessor to input files");
            ArgumentGroup pOpts = parser.addArgumentGroup("Process options");
            pOpts.addArgument("--add-pp-opts", "--Wp").nargs("*").action(Arguments.append())
                    .help("Add preprocessor option");
            pOpts.addArgument("--add-ffront-opts", "--Wf").nargs("*").action(Arguments.append())
                    .help("Add frontend option");
            pOpts.addArgument("--add-trans-opts", "--Wx").nargs("*").action(Arguments.append())
                    .help("Add Xcode translator option");
            ArgumentGroup fcOpts = parser.addArgumentGroup("Fortran compiler options");
            fcOpts.addArgument("--fc-type").choices(FC_COMPILER_TYPES).help("Fortran compiler type");
            fcOpts.addArgument("--fc-cmd").help("Fortran compiler cmd");
            parsedArgs = parser.parseArgs(args);
        } catch (HelpScreenException hse)
        {
            return null;
        } catch (ArgumentParserException ape)
        {
            parser.handleError(ape);
            throw ape;
        }
        Options opts = new Options(parsedArgs);
        return opts;
    }

    Options(Namespace parsedArgs)
    {
        _printInstallCfg = parsedArgs.getBoolean("print_install_cfg");
        _printVersion = parsedArgs.getBoolean("version");
        _printTargets = parsedArgs.getBoolean("list_targets");
        _printDirectives = parsedArgs.getBoolean("list_directives");
        _printCfg = parsedArgs.getBoolean("show_config");
        _printOpts = parsedArgs.getBoolean("print_opts");
        _inputFiles = getPathList(parsedArgs, "fortran_file");
        _outputFile = getOptionalPath(parsedArgs, "output_file");
        _outputDir = getOptionalPath(parsedArgs, "output_dir");
        _outputModDir = getOptionalPath(parsedArgs, "output_mod_dir");
        _userTarget = parsedArgs.getString("target");
        _incDirs = getPathList(parsedArgs, "include_dir");
        _addMacros = getStringList(parsedArgs, "add_macro");
        _accDirLanguage = parsedArgs.getString("directive");
        _configFile = parsedArgs.getString("config");
        _modelConfigFile = parsedArgs.getString("model_config");
        _onlyPreprocess = parsedArgs.getBoolean("only_preprocess");
        _verbose = parsedArgs.getBoolean("verbose");
        _keepComments = parsedArgs.getBoolean("keep_comment");
        _forceTranslation = parsedArgs.getBoolean("force");
        _resolveDependencies = !parsedArgs.getBoolean("no_dep");
        _showDebugOutput = parsedArgs.getBoolean("debug");
        _keepIntFiles = parsedArgs.getBoolean("keep_int_files");
        _intDir = getOptionalPath(parsedArgs, "int_dir");
        _debugOmniFFront = parsedArgs.getBoolean("debug_ffront");
        _disableFFrontModuleCache = parsedArgs.getBoolean("no_module_cache");
        _skipPP = parsedArgs.getBoolean("skip_pp");
        _stopAfterPP = parsedArgs.getBoolean("stop_pp");
        _stopAfterFFront = parsedArgs.getBoolean("stop_frontend");
        _stopAfterDepRes = parsedArgs.getBoolean("stop_dependencies");
        _stopAfterTrans = parsedArgs.getBoolean("stop_translator");
        _ppOpts = getStringList(parsedArgs, "add_pp_opts");
        _ffrontOpts = getStringList(parsedArgs, "add_ffront_opts");
        _transOpts = getStringList(parsedArgs, "add_trans_opts");
        _cfgKeyOverrides = getStringList(parsedArgs, "override_cfg_key");
        _maxFLineLength = parsedArgs.getInt("max_fortran_line_length");
        _addPPLineDirectives = parsedArgs.getBoolean("add_pp_line_directives");
        _dumpCX2TArgs = parsedArgs.getBoolean("dump_cx2t_args");
        _exitOnPureFunction = parsedArgs.getBoolean("force_pure");
        _addParenToBinOpts = parsedArgs.getBoolean("add_paren");
        _genTransReport = parsedArgs.getBoolean("report");
        _fCompilerType = parsedArgs.getString("fc_compiler_type");
        _fCompilerCmd = parsedArgs.getString("fc_compiler_cmd");
        _disableMP = parsedArgs.getBoolean("disable_mp");
    }

    @Override
    public String toString()
    {
        String res = "";
        res += "Input files: \n\t"
                + String.join("\n\t", inputFiles().stream().map((path) -> path.toString()).collect(Collectors.toList()))
                + "\n";
        res += "Include directories: \n\t" + String.join("\n\t",
                includeDirs().stream().map((path) -> path.toString()).collect(Collectors.toList())) + "\n";
        res += "Predefined macros: \n\t" + String.join("\n\t", predefinedMacros()) + "\n";
        res += String.format("Output file: \"%s\"\n", outputFile());
        res += String.format("Output directory: \"%s\"\n", outputDir());
        res += String.format("Output xmod directory: \"%s\"\n", outputModulesDir());
        res += "User target: " + userTarget() + "\n";
        res += "Accelerator directive language: " + acceleratorDirectiveLanguage() + "\n";
        res += "Config file: " + configFile() + "\n";
        res += String.format("Config overrides: \n\t%s\n", String.join("\n\t", cfgKeysOverrides()));
        res += "Model Config file: " + modelConfigFile() + "\n";
        res += String.format("Only preprocess: %s\n", onlyPreprocess());
        res += String.format("Verbose output: %s\n", verbose());
        res += String.format("Keep comments: %s\n", keepComments());
        res += String.format("Force translation: %s\n", forceTranslation());
        res += String.format("Resolve dependencies: %s\n", resolveDependencies());
        res += String.format("Show debug output: %s\n", showDebugOutput());
        res += String.format("Keep intermediate files: %s\n", keepIntermediateFiles());
        res += String.format("Intermediate files directory: %s\n", intermediateFilesDir().toString());
        res += String.format("OMNI Fortran Front-End debugging enabled: %s\n", debugOmniFFront());
        res += String.format("Disable OMNI Fortran Front-End module cache: %s\n", disableOmniFFrontModuleCache());
        res += String.format("Skip preprocessing: %s\n", skipPreprocessing());
        res += String.format("Stop after preprocessing: %s\n", stopAfterPreprocessing());
        res += String.format("Stop after OMNI Fortran Front-End: %s\n", stopAfterOmniFFront());
        res += String.format("Stop after dependencies resolution: %s\n", stopAfterDepResolution());
        res += String.format("Stop after translator: %s\n", stopAfterTranslation());
        res += String.format("Add preprocessor options: \n\t%s\n", String.join("\n\t", preprocessorOptions()));
        res += String.format("Add OMNI Fortran Front-End options: \n\t%s\n", String.join("\n\t", OmniFFrontOptions()));
        res += String.format("Add translation options: \n\t%s\n", String.join("\n\t", translatorOptions()));
        res += String.format("Max Fortran line length: %s\n", maxFortranLineLength());
        res += String.format("Add preprocessor line directives: %s\n", addPreprocLineDirectives());
        res += String.format("Dump CX2T args: %s\n", dumpCX2TArgs());
        res += String.format("Exit on pure function: %s\n", exitOnPureFunction());
        res += String.format("Add parenthesis to binary opts: %s\n", addParenToBinaryOpts());
        res += String.format("Generate transformation report: %s\n", genTransReport());
        res += String.format("Max Fortran line length: %s\n", maxFortranLineLength());
        res += String.format("Print install configuration: %s\n", printInstallCfg());
        res += String.format("Print configuration: %s\n", printCfg());
        res += String.format("Print install version: %s\n", printVersion());
        res += String.format("Print supported targets: %s\n", printTargets());
        res += String.format("Print supported accelerator directive languages: %s\n", printDirectives());
        res += String.format("Print options: %s\n", printOptions());
        res += String.format("Fortran compiler type: %s\n", fortranCompilerType());
        res += String.format("Fortran compiler cmd: %s\n", fortranCompilerCmd());
        res += String.format("Disable multiprocessing: %s\n", disableMultiprocessing());
        return res;
    }

    Path getOptionalPath(Namespace parsedArgs, String name)
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

    List<String> getStringList(Namespace parsedArgs, String name)
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
            path = Utils.STARTUP_DIR.resolve(path);
        }
        return path;
    }
}
