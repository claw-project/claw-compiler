/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc;

import static clawfc.Utils.sprintf;

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
    final boolean _printCLAWFiles;
    final boolean _genBuildInfoFiles;
    final boolean _genModFiles;
    final List<Path> _inputFiles;
    final Path _outputFile;
    final Path _outputDir;
    final Path _ppOutDir;
    final Path _xmodOutDir;
    final String _userTarget;
    final List<Path> _ppIncDirs;
    final List<Path> _srcIncDirs;
    final List<Path> _binfoIncDirs;
    final Path _binfoOutDir;
    final List<Path> _modIncDirs;
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
    final boolean _stopAfterDepScan;
    final boolean _stopAfterFFront;
    final boolean _stopAfterDepRes;
    final boolean _stopAfterXmodGen;
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

    public boolean printCLAWFiles()
    {
        return _printCLAWFiles;
    }

    public boolean generateBuildInfoFiles()
    {
        return _genBuildInfoFiles;
    }

    public boolean generateModFiles()
    {
        return _genModFiles;
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

    public Path xmodOutputDir()
    {
        return _xmodOutDir;
    }

    public Path preprocessedSourcesOutputDir()
    {
        return _ppOutDir;
    }

    public String userTarget()
    {
        return _userTarget;
    }

    public List<Path> preprocessorIncludeDirs()
    {
        return _ppIncDirs;
    }

    public List<Path> sourceIncludeDirs()
    {
        return _srcIncDirs;
    }

    public List<Path> moduleIncludeDirs()
    {
        return _modIncDirs;
    }

    public List<Path> buildInfoIncludeDirs()
    {
        return _binfoIncDirs;
    }

    public Path buildInfoOutputDir()
    {
        return _binfoOutDir;
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

    public boolean stopAfterDepScan()
    {
        return _stopAfterDepScan;
    }

    public boolean stopAfterOmniFFront()
    {
        return _stopAfterFFront;
    }

    public boolean stopAfterDepResolution()
    {
        return _stopAfterDepRes;
    }

    public boolean stopAfterXmodGeneration()
    {
        return _stopAfterXmodGen;
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
            qOpts.addArgument("--print-claw-files").action(Arguments.storeTrue())
                    .help("Print input files which use CLAW directives");
            MutuallyExclusiveGroup outOpts = parser.addMutuallyExclusiveGroup("Compiler output options");
            outOpts.addArgument("-o", "--output-file")
                    .help("Output file for the transformed FORTRAN code. If not given, code is printed to stdout.");
            outOpts.addArgument("-O", "--output-dir").help("Output directory for transformed FORTRAN files");
            ArgumentGroup cOpts = parser.addArgumentGroup("Compiler options");
            cOpts.addArgument("-I", "--pp-include-dir").nargs("*").action(Arguments.append()).help(
                    "Add the directory to the search path for include files reference in preprocessor directives");
            outOpts.addArgument("-PO", "--pp-output-dir")
                    .help("Output directory for preprocessed FORTRAN source files");
            cOpts.addArgument("-D", "--add-macro").nargs("*").action(Arguments.append()).help("Predefine macro");
            cOpts.addArgument("-SI", "--src-include-dir").nargs("*").action(Arguments.append())
                    .help("Add the directory to the search path for the source of referenced Fortran modules");
            cOpts.addArgument("-M", "-MI", "--mod-include-dir").nargs("*").action(Arguments.append())
                    .help("Input directory for .xmod files.");
            cOpts.addArgument("-J", "-MO", "--mod-output-dir").help("Output directory for .xmod files.");
            cOpts.addArgument("-BI", "--buildinfo-include-dir").nargs("*").action(Arguments.append())
                    .help("Include directory for BuildInfo files");
            cOpts.addArgument("-BO", "--buildinfo-output-dir").help("Output directory for BuildInfo files");
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
                    .help("Force the translation of modules without directives");
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
            MutuallyExclusiveGroup sOpts = parser.addMutuallyExclusiveGroup("Compiler debug options");
            sOpts.addArgument("--stop-depscan").action(Arguments.storeTrue())
                    .help("Save intermediate files and stop after dependencies scan");
            sOpts.addArgument("--stop-dependencies").action(Arguments.storeTrue())
                    .help("Save intermediate files and stop after dependencies resolution");
            sOpts.addArgument("--stop-xmod-gen").action(Arguments.storeTrue())
                    .help("Save intermediate files and stop after Xmod generation");
            sOpts.addArgument("--stop-frontend").action(Arguments.storeTrue())
                    .help("Save intermediate files and stop after frontend");
            sOpts.addArgument("--stop-translator").action(Arguments.storeTrue())
                    .help("Save intermediate files and stop after translator");
            cOpts.addArgument("--gen-buildinfo-files").action(Arguments.storeTrue())
                    .help("Generate build information files for input and then stop");
            cOpts.addArgument("--gen-mod-files").action(Arguments.storeTrue())
                    .help("Generate xmod files for input and then stop");
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
        _printCLAWFiles = parsedArgs.getBoolean("print_claw_files");
        _inputFiles = getPathList(parsedArgs, "fortran_file");
        _outputFile = getOptionalPath(parsedArgs, "output_file");
        _outputDir = getOptionalPath(parsedArgs, "output_dir");
        _ppOutDir = getOptionalPath(parsedArgs, "pp_output_dir");
        _xmodOutDir = getOptionalPath(parsedArgs, "mod_output_dir");
        _userTarget = parsedArgs.getString("target");
        _ppIncDirs = getPathList(parsedArgs, "pp_include_dir");
        _srcIncDirs = getPathList(parsedArgs, "src_include_dir");
        _modIncDirs = getPathList(parsedArgs, "mod_include_dir");
        _binfoIncDirs = getPathList(parsedArgs, "buildinfo_include_dir");
        _binfoOutDir = getOptionalPath(parsedArgs, "buildinfo_output_dir");
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
        _stopAfterDepScan = parsedArgs.getBoolean("stop_depscan");
        _stopAfterFFront = parsedArgs.getBoolean("stop_frontend");
        _stopAfterDepRes = parsedArgs.getBoolean("stop_dependencies");
        _stopAfterXmodGen = parsedArgs.getBoolean("stop_xmod_gen");
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
        _fCompilerType = parsedArgs.getString("fc_type");
        _fCompilerCmd = parsedArgs.getString("fc_cmd");
        _disableMP = parsedArgs.getBoolean("disable_mp");
        _genBuildInfoFiles = parsedArgs.getBoolean("gen_buildinfo_files");
        _genModFiles = parsedArgs.getBoolean("gen_mod_files");
    }

    String toString(List<Path> paths)
    {
        final String res = String.join("\n\t",
                paths.stream().map((path) -> path.toString()).collect(Collectors.toList()));
        return res != "" ? "\t" + res + "\n" : res;
    }

    @Override
    public String toString()
    {
        StringBuilder res = new StringBuilder();
        res.append("Input files: \n" + toString(inputFiles()));
        res.append("Preprocessor include directories: \n" + toString(preprocessorIncludeDirs()));
        res.append("Source include directories: \n" + toString(sourceIncludeDirs()));
        res.append("Buildinfo include directories: \n" + toString(buildInfoIncludeDirs()));
        res.append("Module include directories: \n" + toString(moduleIncludeDirs()));
        res.append("Predefined macros: \n\t" + String.join("\n\t", predefinedMacros()) + "\n");
        res.append(sprintf("Output file: \"%s\"\n", outputFile()));
        res.append(sprintf("Output directory: \"%s\"\n", outputDir()));
        res.append(sprintf("Output xmod directory: \"%s\"\n", xmodOutputDir()));
        res.append(sprintf("Output buildinfo directory: \"%s\"\n", buildInfoOutputDir()));
        res.append(sprintf("Preprocessed sources output directory: \"%s\"\n", preprocessedSourcesOutputDir()));
        res.append("User target: " + userTarget() + "\n");
        res.append("Accelerator directive language: " + acceleratorDirectiveLanguage() + "\n");
        res.append("Config file: " + configFile() + "\n");
        res.append(sprintf("Config overrides: \n\t%s\n", String.join("\n\t", cfgKeysOverrides())));
        res.append("Model Config file: " + modelConfigFile() + "\n");
        res.append(sprintf("Only preprocess: %s\n", onlyPreprocess()));
        res.append(sprintf("Verbose output: %s\n", verbose()));
        res.append(sprintf("Keep comments: %s\n", keepComments()));
        res.append(sprintf("Force translation: %s\n", forceTranslation()));
        res.append(sprintf("Resolve dependencies: %s\n", resolveDependencies()));
        res.append(sprintf("Show debug output: %s\n", showDebugOutput()));
        res.append(sprintf("Keep intermediate files: %s\n", keepIntermediateFiles()));
        res.append(sprintf("Intermediate files directory: %s\n", intermediateFilesDir()));
        res.append(sprintf("OMNI Fortran Front-End debugging enabled: %s\n", debugOmniFFront()));
        res.append(sprintf("Disable OMNI Fortran Front-End module cache: %s\n", disableOmniFFrontModuleCache()));
        res.append(sprintf("Skip preprocessing: %s\n", skipPreprocessing()));
        res.append(sprintf("Stop after preprocessing: %s\n", stopAfterPreprocessing()));
        res.append(sprintf("Stop after dependencies scan: %s\n", stopAfterDepScan()));
        res.append(sprintf("Stop after OMNI Fortran Front-End: %s\n", stopAfterOmniFFront()));
        res.append(sprintf("Stop after dependencies resolution: %s\n", stopAfterDepResolution()));
        res.append(sprintf("Stop after Xmod generation: %s\n", stopAfterXmodGeneration()));
        res.append(sprintf("Stop after translator: %s\n", stopAfterTranslation()));
        res.append(sprintf("Add preprocessor options: \n\t%s\n", String.join("\n\t", preprocessorOptions())));
        res.append(sprintf("Add OMNI Fortran Front-End options: \n\t%s\n", String.join("\n\t", OmniFFrontOptions())));
        res.append(sprintf("Add translation options: \n\t%s\n", String.join("\n\t", translatorOptions())));
        res.append(sprintf("Max Fortran line length: %s\n", maxFortranLineLength()));
        res.append(sprintf("Add preprocessor line directives: %s\n", addPreprocLineDirectives()));
        res.append(sprintf("Dump CX2T args: %s\n", dumpCX2TArgs()));
        res.append(sprintf("Exit on pure function: %s\n", exitOnPureFunction()));
        res.append(sprintf("Add parenthesis to binary opts: %s\n", addParenToBinaryOpts()));
        res.append(sprintf("Generate transformation report: %s\n", genTransReport()));
        res.append(sprintf("Max Fortran line length: %s\n", maxFortranLineLength()));
        res.append(sprintf("Print install configuration: %s\n", printInstallCfg()));
        res.append(sprintf("Print configuration: %s\n", printCfg()));
        res.append(sprintf("Print install version: %s\n", printVersion()));
        res.append(sprintf("Print supported targets: %s\n", printTargets()));
        res.append(sprintf("Print supported accelerator directive languages: %s\n", printDirectives()));
        res.append(sprintf("Print options: %s\n", printOptions()));
        res.append(sprintf("Print input CLAW files: %s\n", printCLAWFiles()));
        res.append(sprintf("Fortran compiler type: %s\n", fortranCompilerType()));
        res.append(sprintf("Fortran compiler cmd: %s\n", fortranCompilerCmd()));
        res.append(sprintf("Disable multiprocessing: %s\n", disableMultiprocessing()));
        res.append(sprintf("Generate dependencies info files: %s\n", generateBuildInfoFiles()));
        res.append(sprintf("Generate xmod files: %s\n", generateModFiles()));
        return res.toString();
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
