/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc;

import static clawfc.Utils.ASCII_CARRIAGE_RETURN;
import static clawfc.Utils.ASCII_NEWLINE_VALUE;
import static clawfc.Utils.collectIntoString;
import static clawfc.Utils.copy;
import static clawfc.Utils.executeTasksUntilFirstError;
import static clawfc.Utils.fileExists;
import static clawfc.Utils.getOrCreateDir;
import static clawfc.Utils.max;
import static clawfc.Utils.removeExtension;
import static clawfc.Utils.saveToFile;
import static clawfc.Utils.sprintf;

import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.function.Function;
import java.util.stream.Collectors;

import claw.wani.ClawX2T;
import claw.wani.ConfigurationOptions;
import clawfc.ProgramUnitData.UnitDesignation;
import clawfc.Utils.ExecuteTasks;
import clawfc.depscan.FortranDepScanner;
import clawfc.depscan.FortranException;
import clawfc.depscan.FortranFileProgramUnitInfo;
import clawfc.depscan.FortranFileProgramUnitInfoDeserializer;
import clawfc.depscan.FortranFileProgramUnitInfoSerializer;
import clawfc.depscan.FortranProgramUnitInfo;
import clawfc.utils.AsciiArrayIOStream;
import clawfc.utils.FileInfo;
import clawfc.utils.FileInfoImpl;
import clawfc.utils.PathHashGenerator;
import clawfc.utils.UniquePathHashGenerator;

public class Driver
{
    public static void main(String[] args) throws Exception
    {
        run(args, null);
        System.exit(0);
    }

    public static void run(String[] args, Path workingDir) throws Exception
    {
        Utils.log.setLevel(java.util.logging.Level.INFO);
        Driver driver = new Driver();
        Driver.verifyInstall();
        final Options opts;
        if (workingDir != null)
        {
            opts = Options.parseCmdlineArguments(args, workingDir);
        } else
        {
            opts = Options.parseCmdlineArguments(args);
        }
        if (opts != null)
        {
            if (!opts.verbose())
            {
                Utils.log.setLevel(java.util.logging.Level.SEVERE);
            }
            verifyOptions(opts);
            driver.execute(opts);
        }
    }

    static Configuration _cfg;

    public static Configuration cfg()
    {
        return _cfg;
    }

    Driver() throws Exception
    {
        _cfg = new Configuration();
    }

    static void verifyInstall()
    {
        if (!Utils.dirExists(cfg().installRoot()))
        {
            throw new RuntimeException((sprintf("CLAW install directory \"%s\" does not exist or is not a directory",
                    cfg().installRoot())));
        }
        if (!Utils.dirExists(cfg().omniInstallRoot()))
        {
            throw new RuntimeException(
                    sprintf("OMNI XCodeML Tools install directory \"%s\" does not exist or is not a directory",
                            cfg().omniInstallRoot()));
        }
        if (!Files.isExecutable(cfg().omniFrontEnd()))
        {
            throw new RuntimeException(
                    sprintf("OMNI XCodeML Tools Fortran Frontend \"%s\" does not exist or is not a directory",
                            cfg().omniFrontEnd()));
        }
        {
            String omniVersionTag = null;
            try
            {
                omniVersionTag = Utils.getCmdOutput(new String[] { cfg().omniFrontEnd().toString(), "--version-tag" });
            } catch (Exception e)
            {
                throw new RuntimeException("Failed to get OMNI XCodeML Tools version: " + e.getMessage());
            }
            if (!cfg().omniVersionTag().equals(omniVersionTag))
            {
                throw new RuntimeException(
                        sprintf("OMNI XCodeML Tools version mismatch\n\texpected: \"%s\"\n\tgot: \"%s\"",
                                cfg().omniVersionTag(), omniVersionTag));
            }
        }
    }

    int getMaxNumMPJobs(Options opts)
    {
        if (opts.disableMultiprocessing())
        {
            return 1;
        } else if (opts.maxNumMultiprocJobs() != null)
        {
            return opts.maxNumMultiprocJobs();
        } else
        {
            return Runtime.getRuntime().availableProcessors();
        }
    }

    void execute(Options opts) throws Exception
    {
        if (opts.printInstallCfg())
        {
            print(cfg().toString());
        } else if (opts.printVersion())
        {
            printVersion();
        } else if (opts.printTargets())
        {
            ClawX2T.printTargets();
        } else if (opts.printDirectives())
        {
            ClawX2T.printDirectiveLanguages();
        } else if (opts.printCfg())
        {
            printCX2TCfg(opts);
        } else if (opts.printOptions())
        {
            print(opts.toString());
        } else
        {
            if (opts.inputFiles().isEmpty())
            {
                return;
            }
            Path tmpDir = null;
            try
            {
                final int maxNumMPJobs = getMaxNumMPJobs(opts);
                Map<Path, FortranFileProgramUnitInfoData> buildInfoBySrcPath;
                if (!opts.buildInfoIncludeDirs().isEmpty())
                {
                    info("Loading input build information files...");
                    buildInfoBySrcPath = loadBuildInfoFromFiles(opts.buildInfoIncludeDirs(), opts.inputFiles(),
                            opts.sourceIncludeDirs(), maxNumMPJobs);
                } else
                {
                    buildInfoBySrcPath = new HashMap<Path, FortranFileProgramUnitInfoData>();
                }
                info("Creating temp files directory...");
                tmpDir = createTempDir(opts);
                info(tmpDir.toString(), 1);
                info("Creating temp dirs for input files...");
                final Preprocessor pp = new Preprocessor(cfg(), opts, tmpDir);
                info("Preprocessing input files...");
                Map<Path, PreprocessedFortranSourceData> inputPPSrcFiles = preprocessFiles(opts.inputFiles(),
                        buildInfoBySrcPath, pp, opts.skipPreprocessing(), maxNumMPJobs);
                final UniquePathHashGenerator dirHashGen = new UniquePathHashGenerator();
                precomputeDirHashes(dirHashGen, opts.inputFiles(), opts.preprocessorIncludeDirs());
                final Map<Path, Path> ppSrcPathBySrcPath = savePreprocessedSources("input", opts.inputFiles(),
                        inputPPSrcFiles, opts.preprocessedSourcesOutputDir(), dirHashGen, opts.skipPreprocessing(),
                        maxNumMPJobs);
                if (!opts.stopAfterPreprocessing())
                {
                    info("Scanning input files for build information...");
                    scanFiles(opts.inputFiles(), buildInfoBySrcPath, inputPPSrcFiles, ppSrcPathBySrcPath, maxNumMPJobs);
                    if (opts.buildInfoOutputDir() != null)
                    {
                        info("Verifying output buildinfo dir...");
                        getOrCreateDir(opts.buildInfoOutputDir());
                        if (!opts.generateBuildInfoFiles())
                        {
                            saveBuildInfo(opts.inputFiles(), buildInfoBySrcPath, opts.buildInfoOutputDir(), dirHashGen,
                                    maxNumMPJobs);
                        } else
                        {
                            saveBuildInfo(opts.inputFiles(), buildInfoBySrcPath, opts.buildInfoOutputDir(), null,
                                    maxNumMPJobs);
                            return;
                        }
                    }
                }
                if (opts.printCLAWFiles())
                {
                    info("Filtering CLAW files...");
                    printCLAWFiles(opts.inputFiles(), buildInfoBySrcPath);
                    return;
                }
                final List<Path> includeFiles = createIncludeFilesList(opts.sourceIncludeDirs(), opts.inputFiles());
                info("Preprocessing include files...");
                Map<Path, PreprocessedFortranSourceData> incPPSrcFiles = preprocessFiles(includeFiles,
                        buildInfoBySrcPath, pp, opts.skipPreprocessing(), maxNumMPJobs);
                Map<Path, Path> ppIncSrcPathBySrcPath = savePreprocessedSources("include", includeFiles, incPPSrcFiles,
                        opts.preprocessedSourcesOutputDir(), dirHashGen, opts.skipPreprocessing(), maxNumMPJobs);
                if (opts.stopAfterPreprocessing())
                {
                    return;
                }
                info("Scanning include files for build information...");
                scanFiles(includeFiles, buildInfoBySrcPath, incPPSrcFiles, ppIncSrcPathBySrcPath, maxNumMPJobs);
                if (opts.buildInfoOutputDir() != null)
                {
                    saveBuildInfo(includeFiles, buildInfoBySrcPath, opts.buildInfoOutputDir(), dirHashGen,
                            maxNumMPJobs);
                }
                if (opts.stopAfterDepScan())
                {
                    return;
                }
                final Map<String, XmodData> inputXmods;
                info("Load input Xmod files...");
                inputXmods = loadXmodFiles(opts.moduleIncludeDirs(), true);
                info("Verifying build set...");
                final Map<String, ProgramUnitInfo> availModules = getAvailableModulesInfo(buildInfoBySrcPath,
                        inputPPSrcFiles, incPPSrcFiles, inputXmods);
                final boolean onlyCLAWTargets = !opts.forceTranslation();
                final Set<String> targetModuleNames = getTargetModuleNames(availModules, false, onlyCLAWTargets);
                final Map<String, ProgramUnitInfo> usedModules = Build.removeUnreferencedModules(availModules,
                        targetModuleNames);
                info("Verifying input xmod files...");
                setXmodData(usedModules, targetModuleNames, inputXmods);
                final FortranFrontEnd ffront = new FortranFrontEnd(cfg(), opts, tmpDir);
                if (opts.resolveDependencies())
                {
                    info("Generating xmods...");
                    generateXmods(ffront, usedModules, targetModuleNames, false, maxNumMPJobs, opts.showDebugOutput());
                    if (opts.xmodOutputDir() != null)
                    {
                        saveXmods(usedModules, targetModuleNames, false, opts.xmodOutputDir(), true);
                    }
                } else
                {
                    info("Generating xmods for targets...");
                    generateXmods(ffront, usedModules, targetModuleNames, true, maxNumMPJobs, opts.showDebugOutput());
                    if (opts.generateModFiles())
                    {
                        saveXmods(usedModules, targetModuleNames, true, opts.xmodOutputDir(), true);
                    }
                }
                if (opts.generateModFiles())
                {
                    return;
                }
                if (opts.stopAfterXmodGeneration())
                {
                    return;
                }
                info("Generating XCodeML-AST...");
                generateXast(ffront, usedModules, targetModuleNames, opts.showDebugOutput(), opts.skipPreprocessing(),
                        maxNumMPJobs);
                if (opts.xastOutputDir() != null)
                {
                    saveXast(usedModules, targetModuleNames, opts.xastOutputDir(), maxNumMPJobs);
                }
                if (opts.stopAfterXastGeneration())
                {
                    return;
                }
                info("Translating XCodeML-AST...");
                translate(usedModules, targetModuleNames, opts, ffront.getOutModDir(), maxNumMPJobs);
                if (opts.transXastOutputDir() != null)
                {
                    saveTransXast(usedModules, targetModuleNames, opts.transXastOutputDir(), maxNumMPJobs);
                }
                if (opts.transReportOutputDir() != null)
                {
                    saveTransReport(usedModules, targetModuleNames, opts.transReportOutputDir(), maxNumMPJobs);
                }
                if (opts.stopAfterTranslation())
                {
                    return;
                }
                if (opts.transSrcOutputDir() != null)
                {
                    saveTransSrc(usedModules, targetModuleNames, opts.transSrcOutputDir(), maxNumMPJobs);
                }
                if (opts.stopAfterDecompilation())
                {
                    return;
                }
                final Map<Path, AsciiArrayIOStream> outSrcBySrcPath = generateOutputSrc(opts.inputFiles(),
                        buildInfoBySrcPath, availModules, targetModuleNames, maxNumMPJobs);
                saveOutputSrc(outSrcBySrcPath, opts.outputFile(), opts.outputDir(), maxNumMPJobs);
            } finally
            {
                if (tmpDir != null && !opts.keepIntermediateFiles())
                {
                    Utils.removeDir(tmpDir);
                }
            }
        }
    }

    static int getNumEndingNewlines(AsciiArrayIOStream buf)
    {
        int numNewLines = 0;
        for (int idx = buf.getSize() - 1; idx >= 0; --idx)
        {
            final Byte chr = buf.getChr(idx);
            if (chr == ASCII_NEWLINE_VALUE)
            {
                ++numNewLines;
            } else if (chr != ASCII_CARRIAGE_RETURN)
            {
                break;
            }
        }
        return numNewLines;
    }

    static Map<Path, AsciiArrayIOStream> generateOutputSrc(final List<Path> inputFiles,
            final Map<Path, FortranFileProgramUnitInfoData> buildInfoBySrcPath,
            final Map<String, ProgramUnitInfo> usedModules, final Set<String> targetModuleNames, int maxNumMPJobs)
            throws Exception
    {
        final int n = inputFiles.size();
        List<Callable<Void>> tasks = new ArrayList<Callable<Void>>(n);
        Map<Path, AsciiArrayIOStream> outBySrcPath = new LinkedHashMap<Path, AsciiArrayIOStream>();
        for (final Path inputFilePath : inputFiles)
        {
            final AsciiArrayIOStream out = new AsciiArrayIOStream();
            outBySrcPath.put(inputFilePath, out);
            tasks.add(new Callable<Void>()
            {
                public Void call() throws Exception
                {
                    FortranFileProgramUnitInfo bInfo = buildInfoBySrcPath.get(inputFilePath).getInfo();
                    final List<String> modNames = bInfo.getModuleNames();
                    for (final String modName : modNames)
                    {
                        final ProgramUnitInfo modInfo = usedModules.get(modName);
                        final boolean isTargetMod = targetModuleNames.contains(modName);
                        final AsciiArrayIOStream modTransSrc = isTargetMod ? modInfo.getTransSrc()
                                : modInfo.getPreprocessedSrc(false);
                        copy(modTransSrc.getAsInputStreamUnsafe(), out);
                        int numNewLines = getNumEndingNewlines(out);
                        for (; numNewLines < 2; ++numNewLines)
                        {
                            out.write(ASCII_NEWLINE_VALUE);
                        }
                    }
                    int numNewLines = getNumEndingNewlines(out);
                    for (; numNewLines < 1; ++numNewLines)
                    {
                        out.write(ASCII_NEWLINE_VALUE);
                    }
                    return null;
                }
            });
        }
        executeTasksUntilFirstError(tasks, maxNumMPJobs);
        return Collections.unmodifiableMap(outBySrcPath);
    }

    static void saveOutputSrc(final Map<Path, AsciiArrayIOStream> outSrcBySrcPath, Path outFilePath, Path outDirPath,
            int maxNumMPJobs) throws Exception
    {
        if (outDirPath == null)
        {
            outDirPath = outFilePath.getParent();
        }
        getOrCreateDir(outDirPath);
        if (outFilePath != null)
        {
            Optional<Entry<Path, AsciiArrayIOStream>> optOutStrm = outSrcBySrcPath.entrySet().stream().findFirst();
            if (optOutStrm.isPresent())
            {
                AsciiArrayIOStream outSrc = optOutStrm.get().getValue();
                saveToFile(outSrc.getAsInputStreamUnsafe(), outFilePath);
            } else
            {
                throw new Exception("outSrcBySrcPath empty");
            }
        } else
        {
            saveOutputSrc(outSrcBySrcPath, outDirPath, maxNumMPJobs);
        }
    }

    static void saveOutputSrc(final Map<Path, AsciiArrayIOStream> outSrcBySrcPath, final Path outDirPath,
            int maxNumMPJobs) throws Exception
    {
        final int n = outSrcBySrcPath.size();
        List<Callable<Void>> tasks = new ArrayList<Callable<Void>>(n);
        for (final Map.Entry<Path, AsciiArrayIOStream> entry : outSrcBySrcPath.entrySet())
        {
            tasks.add(new Callable<Void>()
            {
                public Void call() throws Exception
                {
                    final Path inFilePath = entry.getKey();
                    Path outFilePath = outDirPath.resolve(inFilePath.getFileName());
                    final AsciiArrayIOStream outSrc = entry.getValue();
                    saveToFile(outSrc.getAsInputStreamUnsafe(), outFilePath);
                    return null;
                }
            });
        }
        executeTasksUntilFirstError(tasks, maxNumMPJobs);
    }

    static void precomputeDirHashes(UniquePathHashGenerator hashGen, List<Path> inputFiles, List<Path> includeDirs)
    {
        for (Path path : includeDirs)
        {
            hashGen.generate(path);
        }
        for (Path filePath : inputFiles)
        {
            hashGen.generate(filePath.getParent());
        }
    }

    static Collection<Path> getDepXmods(ProgramUnitInfo modInfo, Map<String, ProgramUnitInfo> usedModules)
    {
        List<Path> depXmods = new ArrayList<Path>();
        for (String depModName : modInfo.getUsedModules())
        {
            depXmods.add(usedModules.get(depModName).getXMod().getFilePath());
        }
        return depXmods;
    }

    static class GenerateXmods extends ExecuteTasks
    {
        final FortranFrontEnd ffront;
        final Map<String, ProgramUnitInfo> usedModules;
        final Set<String> targetModuleNames;
        final Set<Path> targetSrcFiles;
        final boolean onlyForTargetsOrSameFile;
        final boolean printDebugOutput;

        final BuildOrder buildOrder;
        public final Set<String> successful;
        public final Set<String> failed;

        public GenerateXmods(FortranFrontEnd ffront, Map<String, ProgramUnitInfo> usedModules,
                Set<String> targetModuleNames, final boolean onlyForTargetsOrSameFile, int maxNumMPJobs,
                boolean printDebugOutput)
        {
            super(maxNumMPJobs);
            this.ffront = ffront;
            this.usedModules = usedModules;
            this.targetModuleNames = targetModuleNames;
            this.onlyForTargetsOrSameFile = onlyForTargetsOrSameFile;
            this.printDebugOutput = printDebugOutput;
            buildOrder = Build.getParallelOrder(usedModules, targetModuleNames);
            successful = Collections.synchronizedSet(new LinkedHashSet<String>());
            failed = Collections.synchronizedSet(new LinkedHashSet<String>());
            targetSrcFiles = getTargetSrcFiles(usedModules, targetModuleNames);
        }

        static Set<Path> getTargetSrcFiles(Map<String, ProgramUnitInfo> usedModules, Set<String> targetModuleNames)
        {
            Set<Path> targetSrcFiles = new HashSet<Path>();
            for (String targetModName : targetModuleNames)
            {
                final Path srcPath = usedModules.get(targetModName).getSrcPath();
                targetSrcFiles.add(srcPath);
            }
            return Collections.unmodifiableSet(targetSrcFiles);
        }

        void submitWaitingTasks()
        {
            for (String modNameIt = buildOrder.next(); modNameIt != null; modNameIt = buildOrder.next())
            {
                final String modName = modNameIt;
                final ProgramUnitInfo modInfo = usedModules.get(modName);
                submitTask(new Callable<Void>()
                {
                    public Void call() throws Exception
                    {
                        if (failed.isEmpty())
                        {
                            if (generateXmod(modInfo))
                            {
                                buildOrder.onProcessed(modName);
                                successful.add(modName);
                                submitWaitingTasks();
                            } else
                            {
                                failed.add(modName);
                            }
                        }
                        return null;
                    }
                });
            }
        }

        public void run() throws Exception
        {
            submitWaitingTasks();
            join();
        }

        boolean generateXmod(ProgramUnitInfo modInfo) throws Exception
        {
            final String modName = modInfo.getName();
            if (!modInfo.isModule())
            {
                info(sprintf("Xmod generation: skipping %s, it is a %s", modName, Build.moduleNameWithLocation(modInfo),
                        modInfo.getType()));
                return true;
            }
            if (modInfo.getXMod() != null)
            {
                info(sprintf("Xmod generation: %s.xmod for dependency module %s is up to date", modName,
                        Build.moduleNameWithLocation(modInfo)));
                return true;
            }
            final boolean isTarget = targetModuleNames.contains(modName);
            final Path modSrcFilePath = modInfo.getSrcPath();
            final boolean isInTargetSrcFile = targetSrcFiles.contains(modSrcFilePath);
            if (onlyForTargetsOrSameFile && !isTarget && !isInTargetSrcFile)
            {
                error(sprintf("Xmod generation: Error! Up to date xmod file for dependency module %s is not available",
                        Build.moduleNameWithLocation(modInfo)));
                return false;
            }
            if (modInfo.getXMod() != null)
            {
                info(sprintf("Xmod generation: %s.xmod for dependency module %s is up to date", modName,
                        Build.moduleNameWithLocation(modInfo)));
                return true;
            }
            AsciiArrayIOStream xmodDataStrm = new AsciiArrayIOStream();
            try
            {
                AsciiArrayIOStream modPPSrc = modInfo.getPreprocessedSrc(true);
                Collection<Path> depXmods = getDepXmods(modInfo, usedModules);
                ffront.generateXmod(modInfo.getPPSrcPath(), modPPSrc, xmodDataStrm, depXmods);
            } catch (FortranFrontEnd.Failed failed)
            {
                final List<String> modIncStack = Build.getModuleIncludeStack(modName, usedModules, targetModuleNames);
                final String modIncStr = Build.getModuleIncludeStackString(modIncStack, usedModules);
                String errMsg = sprintf(
                        "Xmod generation: Error! Call to Omni frontend for %s failed\nInclude stack:\n%s\n",
                        Build.moduleNameWithLocation(modInfo), modIncStr);
                if (printDebugOutput)
                {
                    errMsg += "\n" + failed.getMessage();
                }
                error(errMsg);
                return false;
            }
            final Path xmodFilePath = XmodData.getOutputFilePath(ffront.getOutModDir(), modName);
            final FileInfo xmodFInfo = saveToFile(xmodDataStrm.getAsInputStreamUnsafe(), xmodFilePath);
            final XmodData xmodData = new XmodData(modName, xmodFInfo, xmodDataStrm);
            ((ProgramUnitData) modInfo).setXMod(xmodData);
            return true;
        }
    };

    static void generateXmods(FortranFrontEnd ffront, Map<String, ProgramUnitInfo> usedModules,
            Set<String> targetModuleNames, final boolean onlyForTargetsOrSameFile, int maxNumMPJobs,
            boolean printDebugOutput) throws Exception
    {
        GenerateXmods genXmodTasks = new GenerateXmods(ffront, usedModules, targetModuleNames, onlyForTargetsOrSameFile,
                maxNumMPJobs, printDebugOutput);
        genXmodTasks.run();
        final boolean res = genXmodTasks.failed.isEmpty() && (genXmodTasks.successful.size() == usedModules.size());
        if (!res)
        {
            String errMsg = sprintf("Xmod generation failed: only %s out of %s successful",
                    genXmodTasks.successful.size(), usedModules.size());
            throw new Exception(errMsg);
        }
    }

    static void saveXmods(Map<String, ProgramUnitInfo> usedModules, Set<String> targetModuleNames,
            final boolean onlyForTargets, final Path outDirPath, final boolean ignoreStdCompilerLibs) throws Exception
    {// It is best to save xmods in order so that file modifications dates will be in
     // the right sequence
        BuildOrder buildOrder = Build.getParallelOrder(usedModules, targetModuleNames);
        for (String modName = buildOrder.next(); modName != null; modName = buildOrder.next())
        {
            if (!onlyForTargets || targetModuleNames.contains(modName))
            {
                final ProgramUnitInfo modInfo = usedModules.get(modName);
                if (modInfo.isModule())
                {
                    final XmodData xmodData = modInfo.getXMod();
                    if (!ignoreStdCompilerLibs || !xmodData.isStdCompilerMod())
                    {
                        xmodData.save(outDirPath);
                    }
                }
            }
            buildOrder.onProcessed(modName);
        }
    }

    static void generateXast(FortranFrontEnd ffront, Map<String, ProgramUnitInfo> usedModules,
            Set<String> targetModuleNames, boolean printFfrontDebugOutput, final boolean skipPP, int maxNumMPJobs)
            throws Exception
    {
        final int n = targetModuleNames.size();
        List<Callable<Void>> tasks = new ArrayList<Callable<Void>>(n);
        final ThreadLocal<AddIgnoreDirectiveFilter> addIgnoreFilter = new ThreadLocal<AddIgnoreDirectiveFilter>();
        for (final String modName : targetModuleNames)
        {
            final ProgramUnitInfo modInfo = usedModules.get(modName);
            tasks.add(new Callable<Void>()
            {
                public Void call() throws Exception
                {
                    AddIgnoreDirectiveFilter localAddIgnoreFilter = addIgnoreFilter.get();
                    if (localAddIgnoreFilter == null)
                    {
                        localAddIgnoreFilter = new AddIgnoreDirectiveFilter();
                        addIgnoreFilter.set(localAddIgnoreFilter);
                    }
                    AsciiArrayIOStream xastDataStrm = new AsciiArrayIOStream();
                    Path ppSrcPath = modInfo.getPPSrcPath();
                    AsciiArrayIOStream modPPSrc = modInfo.getPreprocessedSrc(ppSrcPath != null);
                    final AsciiArrayIOStream modPPSrcWithIgnore;
                    if (skipPP)
                    {// Ignore is normaly performed at preprocessing stage
                        try
                        {
                            modPPSrcWithIgnore = new AsciiArrayIOStream();
                            localAddIgnoreFilter.run(modPPSrc.getAsInputStreamUnsafe(), modPPSrcWithIgnore);
                        } catch (Exception e)
                        {
                            String errMsg = sprintf("Exception thrown while applying ignore directive to module %s",
                                    Build.moduleNameWithLocation(modInfo));
                            throw new Exception(errMsg, e);
                        }
                    } else
                    {
                        modPPSrcWithIgnore = modPPSrc;
                    }
                    modPPSrc = null;
                    try
                    {
                        Collection<Path> depXmods = getDepXmods(modInfo, usedModules);
                        ffront.generateAST(modInfo.getPPSrcPath(), modPPSrcWithIgnore, xastDataStrm, depXmods);
                    } catch (FortranFrontEnd.Failed failed)
                    {
                        String errMsg = sprintf("Xmod generation: Error! Call to Omni frontend for module %s failed",
                                Build.moduleNameWithLocation(modInfo));
                        if (printFfrontDebugOutput)
                        {
                            errMsg += "\n" + failed.getMessage();
                        }
                        error(errMsg);
                        throw new Exception(errMsg, failed);
                    }
                    ((ProgramUnitData) modInfo).setXast(xastDataStrm);
                    return null;
                }
            });
        }
        executeTasksUntilFirstError(tasks, maxNumMPJobs);
    }

    static ConfigurationOptions toCX2TCfgOptions(final Options opts)
    {
        final ConfigurationOptions cfgOpts = new ConfigurationOptions(opts.targetPlatform(), opts.configFile(), null,
                opts.acceleratorDirectiveLanguage(), opts.modelConfigFile(), opts.cfgKeysOverrides(),
                opts.showDebugOutput(), opts.maxFortranLineLength(), !opts.addPreprocLineDirectives(),
                opts.exitOnPureFunction(), opts.addParenToBinaryOpts(), opts.transSetPaths());
        return cfgOpts;
    }

    static void printCX2TCfg(Options opts) throws Exception
    {
        final ConfigurationOptions cfgOpts = toCX2TCfgOptions(opts);
        claw.wani.x2t.configuration.Configuration cfg = ClawX2T.createCfg(cfgOpts);
        print(cfg.toString());
    }

    static void translate(final Map<String, ProgramUnitInfo> usedModules, final Set<String> targetModuleNames,
            final Options opts, final Path outModDir, final int maxNumMPJobs) throws Exception
    {
        final ConfigurationOptions cfgOpts = toCX2TCfgOptions(opts);
        final boolean saveTransXast = opts.transXastOutputDir() != null;
        final boolean genTransReport = opts.transReportOutputDir() != null;
        final boolean saveDecSrc = !opts.stopAfterTranslation();
        final boolean debugTransform = !opts.showDebugOutput();
        final List<Path> modIncDirs = new ArrayList<Path>(opts.moduleIncludeDirs());
        modIncDirs.add(outModDir);
        final int n = targetModuleNames.size();
        List<Callable<Void>> tasks = new ArrayList<Callable<Void>>(n);
        class ThreadLocalData
        {
            public ThreadLocalData() throws Exception
            {
                if (saveDecSrc)
                {
                    removeIgnoreFilter = new RemoveIgnoreDirectiveFilter();
                    removeVerbatimFilter = new RemoveVerbatimDirectiveFilter();
                    decSrc = new AsciiArrayIOStream();
                    decSrcNoIgnore = new AsciiArrayIOStream();
                } else
                {
                    removeIgnoreFilter = null;
                    removeVerbatimFilter = null;
                    decSrc = null;
                    decSrcNoIgnore = null;
                }
            }

            public void reset()
            {
                if (saveDecSrc)
                {
                    decSrc.reset();
                    decSrcNoIgnore.reset();
                }
            }

            public final RemoveIgnoreDirectiveFilter removeIgnoreFilter;
            public final RemoveVerbatimDirectiveFilter removeVerbatimFilter;
            public final AsciiArrayIOStream decSrc;
            public final AsciiArrayIOStream decSrcNoIgnore;
        }
        final ThreadLocal<ThreadLocalData> threadLocalData = new ThreadLocal<ThreadLocalData>();
        for (final String unitName : targetModuleNames)
        {
            final ProgramUnitInfo unitInfo = usedModules.get(unitName);
            tasks.add(new Callable<Void>()
            {
                public Void call() throws Exception
                {
                    ThreadLocalData localData = threadLocalData.get();
                    if (localData == null)
                    {
                        localData = new ThreadLocalData();
                        threadLocalData.set(localData);
                    }
                    localData.reset();
                    // -----------------------
                    claw.wani.x2t.configuration.Configuration cfg = ClawX2T.createCfg(cfgOpts);
                    AsciiArrayIOStream transXast = saveTransXast ? new AsciiArrayIOStream() : null;
                    AsciiArrayIOStream transReport = genTransReport ? new AsciiArrayIOStream() : null;
                    AsciiArrayIOStream errStrmBuf = new AsciiArrayIOStream();
                    try
                    {
                        AsciiArrayIOStream xast = unitInfo.getXast();
                        ClawX2T.run(cfg, modIncDirs, xast.getAsInputStreamUnsafe(), transReport, transXast,
                                localData.decSrc, new PrintStream(errStrmBuf), null);
                    } catch (Exception e)
                    {
                        String errMsg = sprintf("Call to CX2T translator for %s %s failed", unitInfo.getType(),
                                Build.moduleNameWithLocation(unitInfo));
                        if (debugTransform)
                        {
                            errMsg += sprintf("\nstderr:\n\n%s",
                                    collectIntoString(errStrmBuf.getAsInputStreamUnsafe()));
                        }
                        error(errMsg);
                        throw new Exception(errMsg, e);
                    }
                    if (saveTransXast)
                    {
                        ((ProgramUnitData) unitInfo).setTransXast(transXast);
                    }
                    if (genTransReport)
                    {
                        ((ProgramUnitData) unitInfo).setTransReport(transReport);
                    }
                    if (saveDecSrc)
                    {
                        try
                        {
                            localData.removeIgnoreFilter.run(localData.decSrc.getAsInputStreamUnsafe(),
                                    localData.decSrcNoIgnore);
                        } catch (Exception e)
                        {
                            String errMsg = sprintf("Removal of ignore directives failed for module %s",
                                    Build.moduleNameWithLocation(unitInfo));
                            error(errMsg);
                            throw new Exception(errMsg, e);
                        }
                        AsciiArrayIOStream decSrc = new AsciiArrayIOStream();
                        try
                        {
                            localData.removeVerbatimFilter.run(localData.decSrcNoIgnore.getAsInputStreamUnsafe(),
                                    decSrc);
                        } catch (Exception e)
                        {
                            String errMsg = sprintf("Removal of verbatim directives failed for module %s",
                                    Build.moduleNameWithLocation(unitInfo));
                            error(errMsg);
                            throw new Exception(errMsg, e);
                        }
                        ((ProgramUnitData) unitInfo).setTransSrc(decSrc);
                    }
                    return null;
                }
            });
        }
        executeTasksUntilFirstError(tasks, maxNumMPJobs);
    }

    static void saveModData(final String dataType, final String extension,
            Function<ProgramUnitInfo, AsciiArrayIOStream> getData, Map<String, ProgramUnitInfo> usedModules,
            Set<String> targetModuleNames, final Path outDirPath, int maxNumMPJobs) throws Exception
    {
        getOrCreateDir(outDirPath);
        final int n = targetModuleNames.size();
        List<Callable<Void>> tasks = new ArrayList<Callable<Void>>(n);
        for (final String modName : targetModuleNames)
        {
            final ProgramUnitInfo modInfo = usedModules.get(modName);
            final AsciiArrayIOStream data = getData.apply(modInfo);
            final Path outFilePath = outDirPath.resolve(modName + extension);
            tasks.add(new Callable<Void>()
            {
                public Void call() throws Exception
                {
                    try
                    {
                        saveToFile(data.getAsInputStreamUnsafe(), outFilePath);
                    } catch (Exception e)
                    {
                        String errMsg = sprintf("Failed to save %s for module %s", dataType,
                                Build.moduleNameWithLocation(modInfo));
                        error(errMsg);
                        throw new Exception(errMsg, e);
                    }
                    return null;
                }
            });
        }
        executeTasksUntilFirstError(tasks, maxNumMPJobs);
    }

    static void saveXast(Map<String, ProgramUnitInfo> usedModules, Set<String> targetModuleNames, final Path outDirPath,
            int maxNumMPJobs) throws Exception
    {
        saveModData("XCodeML-AST", ".xast", (ProgramUnitInfo modInfo) -> modInfo.getXast(), usedModules,
                targetModuleNames, outDirPath, maxNumMPJobs);
    }

    static void saveTransXast(Map<String, ProgramUnitInfo> usedModules, Set<String> targetModuleNames,
            final Path outDirPath, int maxNumMPJobs) throws Exception
    {
        saveModData("translated XCodeML-AST", ".xast", (ProgramUnitInfo modInfo) -> modInfo.getTransXast(), usedModules,
                targetModuleNames, outDirPath, maxNumMPJobs);
    }

    static void saveTransSrc(Map<String, ProgramUnitInfo> usedModules, Set<String> targetModuleNames,
            final Path outDirPath, int maxNumMPJobs) throws Exception
    {
        saveModData("translated source", ".f90", (ProgramUnitInfo modInfo) -> modInfo.getTransSrc(), usedModules,
                targetModuleNames, outDirPath, maxNumMPJobs);
    }

    static void saveTransReport(Map<String, ProgramUnitInfo> usedModules, Set<String> targetModuleNames,
            final Path outDirPath, int maxNumMPJobs) throws Exception
    {
        saveModData("transformation report", ".lst", (ProgramUnitInfo modInfo) -> modInfo.getTransReport(), usedModules,
                targetModuleNames, outDirPath, maxNumMPJobs);
    }

    static Map<String, ProgramUnitInfo> getAvailableModulesInfo(
            Map<Path, FortranFileProgramUnitInfoData> buildInfoBySrcPath,
            Map<Path, PreprocessedFortranSourceData> inputPPSrcFiles,
            Map<Path, PreprocessedFortranSourceData> incPPSrcFiles, Map<String, XmodData> inputXmods) throws Exception
    {
        Map<String, ProgramUnitInfo> infoByName = new HashMap<String, ProgramUnitInfo>();
        // Add all modules with infos
        for (Map.Entry<Path, FortranFileProgramUnitInfoData> entry : buildInfoBySrcPath.entrySet())
        {
            final Path srcFilePath = entry.getKey();
            final FortranFileProgramUnitInfoData fileData = entry.getValue();
            UnitDesignation modDesignation;
            PreprocessedFortranSourceData srcData;
            srcData = inputPPSrcFiles.get(srcFilePath);
            if (srcData != null)
            {
                modDesignation = UnitDesignation.INPUT;
            } else
            {
                srcData = incPPSrcFiles.get(srcFilePath);
                if (srcData != null)
                {
                    modDesignation = UnitDesignation.INCLUDE;
                } else
                {// Should be unreachable
                    final String errStr = sprintf(
                            "FortranFileProgramUnitInfoData for %s does not have corresponding preprocessed source data",
                            srcFilePath);
                    throw new Exception(errStr);
                }
            }
            for (FortranProgramUnitInfo unitInfo : fileData.getInfo().getUnits())
            {
                final String unitName = unitInfo.getName();
                final ProgramUnitData unitData = new ProgramUnitData(modDesignation, unitInfo, fileData, srcData);
                ProgramUnitInfo oldData = infoByName.put(unitName, unitData);
                if (oldData != null)
                {
                    final String errStr = sprintf(
                            "Program units with the same name %s are defined in 2 source files:\n\t%s\n\t%s", unitName,
                            oldData.getSrcPath(), unitData.getSrcPath());
                    throw new Exception(errStr);
                }
            }
        }
        // Add all modules without buildinfo
        for (Map.Entry<String, XmodData> entry : inputXmods.entrySet())
        {
            final String modName = entry.getKey();
            ProgramUnitInfo modData = infoByName.get(modName);
            if (modData == null)
            {
                modData = new ProgramUnitData(modName, UnitDesignation.INCLUDE, entry.getValue());
                infoByName.put(modName, modData);
            }
        }
        return Collections.unmodifiableMap(infoByName);
    }

    static void setXmodData(Map<String, ProgramUnitInfo> infoByName, Set<String> targetModuleNames,
            Map<String, XmodData> inputXmods)
    {
        BuildOrder buildOrder = Build.getParallelOrder(infoByName, targetModuleNames);
        Map<String, FileTime> lastTS = new HashMap<String, FileTime>();
        while (!buildOrder.done())
        {
            final String modName = buildOrder.next();
            ProgramUnitInfo modInfo = infoByName.get(modName);
            XmodData modXmodData = modInfo.getXMod();
            if (modXmodData == null)
            {
                FileTime modLastTS = modInfo.getSrcFileBinfoData().getTimestamp();
                for (String depModName : modInfo.getUsedModules())
                {
                    final FileTime depModLastTS = lastTS.get(depModName);
                    modLastTS = max(modLastTS, depModLastTS);
                }
                XmodData inXmod = inputXmods.get(modName);
                if (inXmod != null)
                {
                    final FileTime inXmodTS = inXmod.getTimestamp();
                    if (inXmodTS.compareTo(modLastTS) >= 0)
                    {
                        ((ProgramUnitData) modInfo).setXMod(inXmod);
                        modLastTS = inXmodTS;
                    } else
                    {
                        warning(sprintf("Ignoring outdated input Xmod file \"%s\"", inXmod.getFilePath()));
                    }
                }
                lastTS.put(modName, modLastTS);
            } else
            {
                lastTS.put(modName, modXmodData.getTimestamp());
            }
            buildOrder.onProcessed(modName);
        }
    }

    static Set<String> getTargetModuleNames(Map<String, ProgramUnitInfo> availModules, boolean onlyModules,
            boolean onlyCLAW)
    {
        Set<String> targetModules = new LinkedHashSet<String>();
        for (Map.Entry<String, ProgramUnitInfo> entry : availModules.entrySet())
        {
            String modName = entry.getKey();
            ProgramUnitInfo data = entry.getValue();
            if (data.isInput())
            {
                if (onlyModules && !data.isModule())
                {
                    continue;
                }
                if (onlyCLAW && !data.usesCLAW())
                {
                    continue;
                }
                targetModules.add(modName);
            }
        }
        return Collections.unmodifiableSet(targetModules);
    }

    static void printCLAWFiles(List<Path> inputFiles, Map<Path, FortranFileProgramUnitInfoData> binfoBySrcPath)
    {
        for (Path inputFilePath : inputFiles)
        {
            boolean usesCLAW = false;
            FortranFileProgramUnitInfo binfo = binfoBySrcPath.get(inputFilePath).getInfo();
            for (FortranProgramUnitInfo mInfo : binfo.getUnits())
            {
                if (mInfo.getUsesClaw())
                {
                    usesCLAW = true;
                    break;
                }
            }
            if (usesCLAW)
            {
                println(inputFilePath.toString());
            }
        }
    }

    static Map<String, XmodData> loadXmodFiles(List<Path> includeDirs, boolean addStdCompilerLibs) throws Exception
    {
        Map<String, XmodData> modByName = new LinkedHashMap<String, XmodData>();
        final List<Path> stdCompilerLibsPaths = Arrays.asList(cfg().defaultStdXmodDir(), cfg().omniDefaultStdXmodDir());
        if (addStdCompilerLibs)
        {
            includeDirs = new ArrayList<Path>(includeDirs);
            includeDirs.addAll(stdCompilerLibsPaths);
        }
        List<Path> uniqueDirs = BuildInfo.createDirListFromPaths(includeDirs);
        Map<Path, List<Path>> incDirFiles = BuildInfo.createModuleDirFileLists(uniqueDirs);
        for (Map.Entry<Path, List<Path>> dirFiles : incDirFiles.entrySet())
        {
            final Path dirPath = dirFiles.getKey();
            final boolean isStdMod = stdCompilerLibsPaths.contains(dirPath);
            for (Path modFilePath : dirFiles.getValue())
            {
                final String modFileName = modFilePath.getFileName().toString();
                final String modName = removeExtension(modFileName);
                XmodData data = new XmodData(modName, new FileInfoImpl(modFilePath), isStdMod);
                XmodData oldData = modByName.put(modName, data);
                if (oldData != null)
                {
                    final String errMsg = sprintf("Ignoring duplicate module files \"%s\" and \"%s\"",
                            oldData.getFilePath(), data.getFilePath());
                    warning(errMsg);
                    continue;
                }
            }
        }
        return Collections.unmodifiableMap(modByName);
    }

    static List<Path> createIncludeFilesList(List<Path> includeDirs, List<Path> inputFiles) throws Exception
    {
        Set<Path> inputFilesSet = Collections.unmodifiableSet(inputFiles.stream().collect(Collectors.toSet()));
        List<Path> uniqueDirs = BuildInfo.createDirListFromPaths(includeDirs);
        Map<Path, List<Path>> incDirFiles = BuildInfo.createDirFileLists(uniqueDirs);
        List<Path> includeFiles = new ArrayList<Path>();
        for (Map.Entry<Path, List<Path>> dirFiles : incDirFiles.entrySet())
        {
            for (Path incFilePath : dirFiles.getValue())
            {
                if (!inputFilesSet.contains(incFilePath))
                {
                    includeFiles.add(incFilePath);
                }
            }
        }
        return Collections.unmodifiableList(includeFiles);
    }

    static Map<Path, PreprocessedFortranSourceData> preprocessFiles(List<Path> inputSrcFiles,
            final Map<Path, FortranFileProgramUnitInfoData> buildInfoBySrcPath, final Preprocessor pp,
            final boolean skipPreprocessing, int maxNumMPJobs) throws Exception
    {
        final int n = inputSrcFiles.size();
        PreprocessedFortranSourceData[] ppSrcData = new PreprocessedFortranSourceData[n];
        List<Callable<Void>> tasks = new ArrayList<Callable<Void>>(n);
        for (int i = 0; i < n; ++i)
        {
            final int taskIdx = i;
            final Path inputSrcFilePath = inputSrcFiles.get(i);
            tasks.add(new Callable<Void>()
            {
                public Void call() throws Exception
                {
                    FortranFileProgramUnitInfoData binfoData = buildInfoBySrcPath.get(inputSrcFilePath);
                    PreprocessedFortranSourceData ppSrcFileData = null;
                    if (binfoData != null)
                    {
                        ppSrcFileData = PreprocessedFortranSourceData.load(inputSrcFilePath, binfoData.getInfo());
                    } else if (!skipPreprocessing)
                    {
                        ppSrcFileData = PreprocessedFortranSourceData.create(inputSrcFilePath, pp);
                    } else
                    {
                        ppSrcFileData = PreprocessedFortranSourceData.load(inputSrcFilePath);
                    }
                    ppSrcData[taskIdx] = ppSrcFileData;
                    return null;
                }
            });
        }
        executeTasksUntilFirstError(tasks, maxNumMPJobs);
        Map<Path, PreprocessedFortranSourceData> ppSrcByPath = new LinkedHashMap<Path, PreprocessedFortranSourceData>();
        for (int i = 0; i < n; ++i)
        {
            ppSrcByPath.put(inputSrcFiles.get(i), ppSrcData[i]);
        }
        return Collections.unmodifiableMap(ppSrcByPath);
    }

    static Map<Path, Path> savePreprocessedSources(String category, List<Path> srcPaths,
            Map<Path, PreprocessedFortranSourceData> ppSrcByPath, final Path outDirPath,
            final PathHashGenerator pathHashGen, boolean skipPP, int maxNumMPJobs) throws Exception
    {
        Map<Path, Path> ppSrcPathBySrcPath;
        if (outDirPath != null)
        {
            info("Verifying output directory for preprocessed sources...");
            getOrCreateDir(outDirPath);
            info(sprintf("Saving preprocessed %s sources...", category));
            ppSrcPathBySrcPath = savePreprocessedSources(srcPaths, ppSrcByPath, outDirPath, pathHashGen, maxNumMPJobs);
        } else if (skipPP)
        {
            ppSrcPathBySrcPath = new HashMap<Path, Path>();
            for (Path path : srcPaths)
            {
                ppSrcPathBySrcPath.put(path, path);
            }
        } else
        {
            ppSrcPathBySrcPath = Collections.emptyMap();
        }
        return ppSrcPathBySrcPath;
    }

    /**
     * @return Mapping between source files and preprocessed source files
     */
    static Map<Path, Path> savePreprocessedSources(List<Path> srcPaths,
            Map<Path, PreprocessedFortranSourceData> ppSrcByPath, final Path outDirPath,
            final PathHashGenerator pathHashGen, int maxNumMPJobs) throws Exception
    {
        final int n = srcPaths.size();
        List<Callable<Void>> tasks = new ArrayList<Callable<Void>>(n);
        Path[] ppSrcPaths = new Path[n];
        for (int i = 0; i < n; ++i)
        {
            final int taskIdx = i;
            Path srcFilePath = srcPaths.get(i);
            final PreprocessedFortranSourceData ppSrcData = ppSrcByPath.get(srcFilePath);
            final String dirHash = pathHashGen.generate(srcFilePath.getParent());
            tasks.add(new Callable<Void>()
            {
                public Void call() throws Exception
                {
                    final Path ppSrcPath = ppSrcData.save(outDirPath, dirHash);
                    ppSrcPaths[taskIdx] = ppSrcPath;
                    return null;
                }
            });
        }
        executeTasksUntilFirstError(tasks, maxNumMPJobs);
        Map<Path, Path> ppSrcBySrc = new LinkedHashMap<Path, Path>();
        for (int i = 0; i < n; ++i)
        {
            Path srcFilePath = srcPaths.get(i);
            Path ppSrcFilePath = ppSrcPaths[i];
            ppSrcBySrc.put(srcFilePath, ppSrcFilePath);
        }
        return ppSrcBySrc;
    }

    static List<Path> createBuildInfoFilesList(List<Path> binfoDirs) throws Exception
    {
        List<Path> uniqueDirs = BuildInfo.createDirListFromPaths(binfoDirs);
        Map<Path, List<Path>> binfoDirFiles = BuildInfo.createBuildinfoDirFileLists(uniqueDirs);
        List<Path> binfoFiles = new ArrayList<Path>();
        for (Map.Entry<Path, List<Path>> incDir : binfoDirFiles.entrySet())
        {
            binfoFiles.addAll(incDir.getValue());
        }
        return Collections.unmodifiableList(binfoFiles);
    }

    static Map<Path, FortranFileProgramUnitInfoData> loadBuildInfoFromFiles(List<Path> binfoDirs, List<Path> inputFiles,
            List<Path> includeDirs, int maxNumMPJobs) throws Exception
    {
        final List<Path> binfoFilePaths = createBuildInfoFilesList(binfoDirs);
        final int n = binfoFilePaths.size();
        FortranFileProgramUnitInfoData[] data = new FortranFileProgramUnitInfoData[n];
        // -----------------------------------------------
        List<Callable<Void>> tasks = new ArrayList<Callable<Void>>();
        final ThreadLocal<FortranFileProgramUnitInfoDeserializer> deserializer = new ThreadLocal<FortranFileProgramUnitInfoDeserializer>();
        // -----------------------------------------------
        final Set<Path> inputFilesSet = Collections.unmodifiableSet(new HashSet<Path>(inputFiles));
        final Set<Path> includeDirsSet = Collections.unmodifiableSet(new HashSet<Path>(includeDirs));
        for (int i = 0; i < n; ++i)
        {
            final int dataIdx = i;
            final Path binfoPath = binfoFilePaths.get(i);
            tasks.add(new Callable<Void>()
            {
                public Void call() throws Exception
                {
                    FortranFileProgramUnitInfoDeserializer localDeserializer = deserializer.get();
                    if (localDeserializer == null)
                    {
                        localDeserializer = new FortranFileProgramUnitInfoDeserializer(true);
                        deserializer.set(localDeserializer);
                    }
                    try
                    {
                        FortranFileProgramUnitInfoData binfoData = FortranFileProgramUnitInfoData.load(binfoPath,
                                localDeserializer);
                        Path srcFilePath = binfoData.info.getSrcFilePath();
                        Path srcFileDirPath = srcFilePath.getParent();
                        if (inputFilesSet.contains(srcFilePath) || includeDirsSet.contains(srcFileDirPath))
                        {
                            data[dataIdx] = binfoData;
                        } else
                        {
                            Driver.warning(sprintf(
                                    "Build information file \"%s\" was discarded. It refers to source file outside input and include directories.",
                                    binfoPath));
                        }
                    } catch (FortranFileProgramUnitInfoData.LoadFailed e)
                    {
                        Driver.warning(e.getMessage());
                    } catch (Exception e)
                    {
                        String errMsg = sprintf("Exception thrown while loading build information file \"%s\"",
                                binfoPath.toString());
                        throw new Exception(errMsg, e);
                    }
                    return null;
                }
            });
        }
        executeTasksUntilFirstError(tasks, maxNumMPJobs);
        Map<Path, FortranFileProgramUnitInfoData> res = new HashMap<Path, FortranFileProgramUnitInfoData>();
        for (FortranFileProgramUnitInfoData binfoData : data)
        {
            if (binfoData != null)
            {
                final Path srcFilePath = binfoData.info.getSrcFilePath();
                FortranFileProgramUnitInfoData prevData = res.put(srcFilePath, binfoData);
                if (prevData != null)
                {
                    Path firstInfoFilePath = prevData.filePath;
                    String errFormat = "Ignoring build information file \"%s\" referring to the same source file \"%s\" "
                            + "as earlier processed build information file \"%s\"";
                    String errMsg = sprintf(errFormat, binfoData.filePath, srcFilePath, firstInfoFilePath);
                    Driver.warning(errMsg);
                }
            }
        }
        return res;
    }

    static void saveBuildInfo(List<Path> srcPaths, Map<Path, FortranFileProgramUnitInfoData> binfoBySrcPath,
            final Path outBuildInfoDir, final PathHashGenerator pathHashGen, int maxNumMPJobs) throws Exception
    {
        final ThreadLocal<FortranFileProgramUnitInfoSerializer> serializer = new ThreadLocal<FortranFileProgramUnitInfoSerializer>();
        List<Callable<Void>> tasks = new ArrayList<Callable<Void>>();
        for (final Path srcPath : srcPaths)
        {
            tasks.add(new Callable<Void>()
            {
                public Void call() throws Exception
                {
                    FortranFileProgramUnitInfoSerializer localSerializer = serializer.get();
                    if (localSerializer == null)
                    {
                        localSerializer = new FortranFileProgramUnitInfoSerializer();
                        serializer.set(localSerializer);
                    }
                    FortranFileProgramUnitInfoData data = binfoBySrcPath.get(srcPath);
                    final String dirHash = pathHashGen != null ? pathHashGen.generate(srcPath.getParent()) : null;
                    data.save(outBuildInfoDir, dirHash, localSerializer);
                    return null;
                }
            });
        }
        executeTasksUntilFirstError(tasks, maxNumMPJobs);
    }

    static void scanFiles(List<Path> inputFiles, Map<Path, FortranFileProgramUnitInfoData> binfoBySrcPath,
            Map<Path, PreprocessedFortranSourceData> ppSrcBySrcPath, Map<Path, Path> ppSrcPathBySrcPath,
            int maxNumMPJobs) throws Exception
    {
        final int n = inputFiles.size();
        List<Callable<Void>> tasks = new ArrayList<Callable<Void>>();
        final FortranFileProgramUnitInfoData[] binfoDataLst = new FortranFileProgramUnitInfoData[n];
        final ThreadLocal<FortranDepScanner> scanner = new ThreadLocal<FortranDepScanner>();
        for (int i = 0; i < n; ++i)
        {
            final int taskIdx = i;
            final Path srcPath = inputFiles.get(i);
            if (binfoBySrcPath.containsKey(srcPath))
            {
                continue;
            }
            tasks.add(new Callable<Void>()
            {
                public Void call() throws Exception
                {
                    FortranDepScanner localScanner = scanner.get();
                    if (localScanner == null)
                    {
                        localScanner = new FortranDepScanner();
                        scanner.set(localScanner);
                    }
                    final PreprocessedFortranSourceData ppData = ppSrcBySrcPath.get(srcPath);
                    final AsciiArrayIOStream ppSrc = ppData.getPPSource();
                    final List<Path> includes = ppData.getIncludeFilePaths();
                    FortranFileProgramUnitInfo binfo;
                    try
                    {
                        binfo = localScanner.scan(ppSrc.getAsInputStreamUnsafe());
                    } catch (Exception e)
                    {
                        Path ppSrcPath = ppSrcPathBySrcPath.get(srcPath);
                        String errMsg;
                        String lineNumStr = "";
                        if (e instanceof FortranException)
                        {
                            final Integer lineNum = ((FortranException) e).getLineIndex();
                            if (lineNum != null)
                            {
                                lineNumStr = sprintf(":%s", lineNum);
                            }
                        }
                        if (ppSrcPath == null)
                        {
                            errMsg = sprintf("Exception thrown while scanning (preprocessed) file \"%s%s\"", srcPath,
                                    lineNumStr);
                        } else
                        {
                            errMsg = sprintf("Exception thrown while scanning \"%s%s\" (preprocessed \"%s\")",
                                    ppSrcPath, lineNumStr, srcPath);
                        }
                        throw new Exception(errMsg, e);
                    }
                    binfo.setSrcFilePath(srcPath);
                    final Path ppSrcPath = ppSrcPathBySrcPath.get(srcPath);
                    if (ppSrcPath != null)
                    {
                        binfo.setPPSrcFilePath(ppSrcPath);
                    }
                    binfo.setIncludes(includes);
                    FortranFileProgramUnitInfoData binfoData = new FortranFileProgramUnitInfoData(binfo);
                    binfoBySrcPath.put(srcPath, binfoData);
                    binfoDataLst[taskIdx] = binfoData;
                    return null;
                }
            });
        }
        executeTasksUntilFirstError(tasks, maxNumMPJobs);
        for (int i = 0; i < n; ++i)
        {
            final FortranFileProgramUnitInfoData binfoData = binfoDataLst[i];
            if (binfoData != null)
            {
                final Path srcPath = inputFiles.get(i);
                binfoBySrcPath.put(srcPath, binfoData);
            }
        }
    }

    static void print(String s)
    {
        System.out.print(s);
    }

    static void println(String s)
    {
        System.out.println(s);
    }

    static void info(String txt, int subLevel)
    {
        String prefix = String.join("", Collections.nCopies(subLevel, "\n"));
        Utils.log.info(prefix + txt);
    }

    static void info(String txt)
    {
        info(txt, 0);
    }

    static void warning(String txt)
    {
        Utils.log.warning(txt);
    }

    static void error(String txt, int subLevel)
    {
        String prefix = String.join("", Collections.nCopies(subLevel, "\n"));
        Utils.log.severe(prefix + txt);
    }

    static void error(String txt)
    {
        error(txt, 0);
    }

    static Path createTempDir(Options opts) throws IOException
    {
        if (opts.intermediateFilesDir() != null)
        {
            final Path intDir = opts.intermediateFilesDir();
            if (Utils.dirExists(intDir))
            {
                Utils.removeDir(intDir);
            }
            Files.createDirectories(intDir);
            return intDir;
        } else
        {
            Path tmpDirPath = Files.createTempDirectory(Paths.get(Utils.DEFAULT_TOP_TEMP_DIR), "clawfc");
            tmpDirPath.toFile().deleteOnExit();
            return tmpDirPath;
        }
    }

    static void verifyOptions(Options opts) throws Exception
    {
        if (opts.maxNumMultiprocJobs() != null && opts.maxNumMultiprocJobs() < 1)
        {
            throw new Exception(sprintf("Maximum number of multiprocessing jobs \"%d \"must be positive",
                    opts.maxNumMultiprocJobs()));
        }
        {
            Map<Path, Path> fileNames = new HashMap<Path, Path>();
            for (Path inFilePath : opts.inputFiles())
            {
                if (!fileExists(inFilePath))
                {
                    throw new Exception(
                            sprintf("Input file \"%s\" does not exist or is a directory", inFilePath.toString()));
                }
                Path filename = inFilePath.getFileName();
                Path oldPath = fileNames.put(filename, inFilePath);
                if (oldPath != null)
                {
                    throw new Exception(sprintf("Input files cannot have identical names: \n\"%s\"\n\"%s\"",
                            oldPath.toString(), inFilePath.toString()));
                }
            }

        }
        if (opts.fortranCompilerType() != null ^ opts.fortranCompilerCmd() != null)
        {
            throw new Exception(sprintf("Options --fc-type and --fc-cmd must be specified together"));
        }
        for (Path incPath : opts.preprocessorIncludeDirs())
        {
            if (!Utils.dirExists(incPath))
            {
                throw new Exception(
                        sprintf("Include dir \"%s\" does not exist or is not a directory", incPath.toString()));
            }
        }
        for (Path incPath : opts.buildInfoIncludeDirs())
        {
            if (!Utils.dirExists(incPath))
            {
                throw new Exception(sprintf("Buildinfo include dir \"%s\" does not exist or is not a directory",
                        incPath.toString()));
            }
        }
        for (Path incPath : opts.moduleIncludeDirs())
        {
            if (!Utils.dirExists(incPath))
            {
                throw new Exception(
                        sprintf("Module include dir \"%s\" does not exist or is not a directory", incPath.toString()));
            }
        }
        if (!opts.inputFiles().isEmpty())
        {
            if (opts.stopAfterPreprocessing())
            {
                return;
            }
            if (opts.buildInfoOutputDir() != null)
            {
                if (opts.preprocessedSourcesOutputDir() == null && !opts.skipPreprocessing())
                {
                    throw new Exception("Output directory for preprocessed FORTRAN source files -PO/--pp-output-dir"
                            + " must be specified too when using -BO/--buildinfo-output-dir");
                }
            }
            if (opts.stopAfterDepScan() || opts.generateBuildInfoFiles() || opts.printCLAWFiles())
            {
                if (opts.generateBuildInfoFiles())
                {
                    if (opts.buildInfoOutputDir() == null)
                    {
                        throw new Exception("Output buildinfo dir must be specified when using --gen-buildinfo-files");
                    }
                }
                return;
            }
            if (opts.generateModFiles())
            {
                if (opts.xmodOutputDir() == null)
                {
                    throw new Exception("Output directory for .xmod files must be specified");
                }
                return;
            }
            if (opts.stopAfterXmodGeneration())
            {
            } else if (opts.stopAfterXastGeneration())
            {

            } else
            {
                ClawX2T.verifyDirectiveOption(opts.acceleratorDirectiveLanguage());
                ClawX2T.verifyTargetOption(opts.targetPlatform());
                ClawX2T.verifyConfigFile(opts.configFile());
                ClawX2T.verifyModelConfigFile(opts.modelConfigFile());
                if (opts.stopAfterTranslation())
                {

                } else if (opts.stopAfterDecompilation())
                {

                } else
                {
                    if (opts.inputFiles().size() == 1)
                    {
                        if (opts.outputFile() == null && opts.outputDir() == null)
                        {
                            throw new Exception(sprintf("Either output file or output dir must be specified"));
                        }
                    } else if (opts.inputFiles().size() > 1)
                    {
                        if (opts.outputDir() == null)
                        {
                            throw new Exception(
                                    sprintf("Output dir must be specified when multiple input files are used"));
                        }
                    }
                }
            }

        }
    }

    static void printVersion()
    {
        String vStr = sprintf("%s %s \"%s\" %s ", cfg().name(), cfg().version(), cfg().commit(), cfg().omniVersion());
        print(vStr);
    }
};
