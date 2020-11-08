package clawfc;

import java.lang.RuntimeException;

import java.util.logging.Logger;
import java.util.stream.Collectors;
import java.util.*;
import java.nio.file.*;

import java.io.InputStream;

import net.sourceforge.argparse4j.ArgumentParsers;
import net.sourceforge.argparse4j.helper.HelpScreenException;
import net.sourceforge.argparse4j.impl.Arguments;
import net.sourceforge.argparse4j.inf.ArgumentGroup;
import net.sourceforge.argparse4j.inf.MutuallyExclusiveGroup;
import net.sourceforge.argparse4j.inf.ArgumentParser;
import net.sourceforge.argparse4j.inf.Namespace;
import net.sourceforge.argparse4j.inf.ArgumentParserException;

public class Options
{
    boolean _printHelp;
    boolean _printInstallCfg;
    boolean _printVersion;
    boolean _printTargets;
    boolean _printDirectives;
    boolean _printCfg;
    boolean _printOpts;
    List<Path> _inputFiles;
    Path _outputFile;
    Path _outputDir;
    Path _outputModDir;
    String _userTarget;
    List<Path> _incDirs;
    String _accDirLanguage;
    String _configFile;
    String _modelConfigFile;
    List<String> _cfgKeyOverrides;
    boolean _onlyPreprocess;
    boolean _verbose;
    boolean _keepComments;
    boolean _forceTranslation;
    boolean _resolveDependencies;
    boolean _showDebugOutput;
    boolean _debugOmni;
    boolean _debugOmniFFront;
    boolean _disableFFrontModuleCache;
    boolean _stopAfterPP;
    boolean _stopAfterFFront;
    boolean _stopAfterDepRes;
    boolean _stopAfterTrans;
    List<String> _ppOpts;
    List<String> _ffrontOpts;
    List<String> _transOpts;
    Integer _maxFLineLength;
    boolean _addPPLineDirectives;
    boolean _dumpCX2TArgs;
    boolean _exitOnPureFunction;
    boolean _addParenToBinOpts;
    boolean _genTransReport; 

    public boolean printHelp() { return _printHelp; }
    public boolean printInstallCfg() { return _printInstallCfg; }
    public boolean printVersion() { return _printVersion; }
    public boolean printTargets() { return _printTargets; }
    public boolean printDirectives() { return _printDirectives; }
    public boolean printCfg() { return _printCfg; }
    public boolean printOptions() { return _printOpts; }
    public List<Path> inputFiles() { return _inputFiles; }
    public Path outputFile() { return _outputFile; }
    public Path outputDir() { return _outputDir; }
    public Path outputModulesDir() { return _outputModDir; }
    public String userTarget() { return _userTarget; }
    public List<Path> includeDirs() { return _incDirs; }
    public String acceleratorDirectiveLanguage() { return _accDirLanguage; }
    public boolean onlyPreprocess() { return _onlyPreprocess; }
    public boolean verbose() { return _verbose; }
    public boolean keepComments() { return _keepComments; }
    public boolean forceTranslation() { return _forceTranslation; }
    public boolean resolveDependencies() { return _resolveDependencies; }
    public boolean showDebugOutput() { return _showDebugOutput; }
    public boolean debugOmni() { return _debugOmni; }
    public boolean debugOmniFFront() { return _debugOmniFFront; }
    public boolean disableOmniFFrontModuleCache() { return _disableFFrontModuleCache; }
    public boolean stopAfterPreprocessing() { return _stopAfterPP; }
    public boolean stopAfterOmniFFront() { return _stopAfterFFront; }
    public boolean stopAfterDepResolution() { return _stopAfterDepRes; }
    public boolean stopAfterTranslation() { return _stopAfterTrans; }
    public List<String> preprocessorOptions() { return _ppOpts; }
    public List<String> OmniFFrontOptions() { return _ffrontOpts; }
    public List<String> translatorOptions() { return _transOpts; }
    public String configFile() { return _configFile; }
    public String modelConfigFile() { return _modelConfigFile; }
    public List<String> cfgKeysOverrides() { return _cfgKeyOverrides; }
    public Integer maxFortranLineLength() { return _maxFLineLength; }
    public boolean addPreprocLineDirectives() { return _addPPLineDirectives; }
    public boolean dumpCX2TArgs() { return _dumpCX2TArgs; }
    public boolean exitOnPureFunction() { return _exitOnPureFunction; }
    public boolean addParenToBinaryOpts() { return _addParenToBinOpts; }
    public boolean genTransReport() { return _genTransReport; }
    
    public static Options parseCmdlineArguments(String[] args) throws Exception
    {
        ArgumentParser parser = ArgumentParsers.newFor("clawfc").build().description("The CLAW Compiler is a "
                + "source-to-source translator working on the XcodeML intermediate representation");
        Namespace processedArgs = null;
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
            qOpts.addArgument("--print-install-cfg", "--show-env").action(Arguments.storeTrue()).help("Print install configuration");
            qOpts.addArgument("--print-opts").action(Arguments.storeTrue()).help("Print processed cmdline options");
            MutuallyExclusiveGroup outOpts = parser.addMutuallyExclusiveGroup("Compiler output options");
            outOpts.addArgument("-o", "--output-file")
                    .help("Output file for the transformed FORTRAN code. If not given, code is printed to stdout.");
            outOpts.addArgument("-O", "--output-dir").help("Output directory for transformed FORTRAN files");
            ArgumentGroup cOpts = parser.addArgumentGroup("Compiler options");
            cOpts.addArgument("-I", "--include-dir").nargs("*").action(Arguments.append())
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
            cOpts.addArgument("-x", "--override-cfg-key").nargs("*").action(Arguments.append())
                    .help("Override a configuration key:value pair from the command line. Higher "
                            + "priority over base configuration and user configuration");
            cOpts.addArgument("--dump-cx2t-args").action(Arguments.storeTrue())
                    .help("Print arguments passed to CX2T");
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
            processedArgs = parser.parseArgs(args);
        } catch (HelpScreenException hse)
        {
        } catch (ArgumentParserException ape)
        {
            parser.handleError(ape);
            throw ape;
        }
        Options opts = new Options(processedArgs);
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
        _accDirLanguage = parsedArgs.getString("directive");
        _configFile = parsedArgs.getString("config");
        _modelConfigFile = parsedArgs.getString("model_config");
        _onlyPreprocess = parsedArgs.getBoolean("only_preprocess");
        _verbose = parsedArgs.getBoolean("verbose");
        _keepComments = parsedArgs.getBoolean("keep_comment");
        _forceTranslation = parsedArgs.getBoolean("force");
        _resolveDependencies = !parsedArgs.getBoolean("no_dep");
        _showDebugOutput = parsedArgs.getBoolean("debug");
        _debugOmni = parsedArgs.getBoolean("debug_omni");
        _debugOmniFFront = parsedArgs.getBoolean("debug_ffront");
        _disableFFrontModuleCache = parsedArgs.getBoolean("no_module_cache");
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
    }
    
    @Override
    public String toString()
    {
        String res = "";
        res += "Input files: \n\t" + String.join("\n\t", inputFiles()
                .stream().map((path) -> path.toString()).collect( Collectors.toList())) + "\n";
        res += "Include directories: \n\t" + String.join("\n\t", includeDirs()
                .stream().map((path) -> path.toString()).collect( Collectors.toList())) + "\n";
        res += String.format("Output file: \"%s\"\n", outputFile());
        res += String.format("Output directory: \"%s\"\n", outputDir());
        res += String.format("Output xmod directory: \"%s\"\n", outputModulesDir());
        res += "User target: " + userTarget() + "\n";
        res += "Accelerator directive language: " + acceleratorDirectiveLanguage() + "\n";
        res += "Config file: " + configFile() + "\n";
        res += String.format("Config overrides: \n\t%s\n", 
                String.join("\n\t", cfgKeysOverrides()));
        res += "Model Config file: " + modelConfigFile() + "\n";
        res += String.format("Only preprocess: %s\n", onlyPreprocess());
        res += String.format("Verbose output: %s\n", verbose());
        res += String.format("Keep comments: %s\n", keepComments());
        res += String.format("Force translation: %s\n", forceTranslation());
        res += String.format("Resolve dependencies: %s\n", resolveDependencies());
        res += String.format("Show debug output: %s\n", showDebugOutput());
        res += String.format("OMNI debugging enabled: %s\n", debugOmni());
        res += String.format("OMNI Fortran Front-End debugging enabled: %s\n", debugOmniFFront());
        res += String.format("Disable OMNI Fortran Front-End module cache: %s\n", disableOmniFFrontModuleCache());
        res += String.format("Stop after preprocessing: %s\n", stopAfterPreprocessing());
        res += String.format("Stop after OMNI Fortran Front-End: %s\n", stopAfterOmniFFront());
        res += String.format("Stop after dependencies resolution: %s\n", stopAfterDepResolution());
        res += String.format("Stop after translator: %s\n", stopAfterTranslation());
        res += String.format("Add preprocessor options: \n\t%s\n", 
                String.join("\n\t", preprocessorOptions()));
        res += String.format("Add OMNI Fortran Front-End options: \n\t%s\n", 
                String.join("\n\t", OmniFFrontOptions()));
        res += String.format("Add translation options: \n\t%s\n", 
                String.join("\n\t", translatorOptions()));
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
        res += String.format("Print help: %s\n", printHelp());
        res += String.format("Print options: %s\n", printOptions());
        return res;
    }
    
    Path getOptionalPath(Namespace parsedArgs, String name)
    {
        String str = parsedArgs.getString(name);
        Path path = null;
        if(str != null)
        { 
            path = toAbsPath(str); 
        }
        return path;
    }
    
    List<Path> getPathList(Namespace parsedArgs, String name)
    {
        List<Path> res = new ArrayList<Path>();
        List<List<String>> strs = parsedArgs.<List<String>>getList(name);
        if(strs != null)
        {
            for(List<String> lstStr: strs)
            {
                for(String s: lstStr)
                { res.add(toAbsPath(s)); }
            }
        }
        return Collections.unmodifiableList(res);
    }
    
    List<String> getStringList(Namespace parsedArgs, String name)
    {
        List<String> res = new ArrayList<String>();
        List<List<String>> strs = parsedArgs.<List<String>>getList(name);
        if(strs != null)
        {
            for(List<String> lstStr: strs)
            {
                for(String s: lstStr)
                { res.add(s); }
            }
        }
        return Collections.unmodifiableList(res);
    }

    final static Path CWD = Paths.get(System.getProperty("user.dir"));
    
    static Path toAbsPath(String pathStr)
    {
        Path path = Paths.get(pathStr);
        if(!path.isAbsolute())
        {
            path = CWD.resolve(path);
        }
        return path;
    }
}
