/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc;

import static clawfc.Utils.executeTasksUntilFirstError;
import static clawfc.Utils.fileExists;
import static clawfc.Utils.getOrCreateDir;
import static clawfc.Utils.max;
import static clawfc.Utils.removeExtension;
import static clawfc.Utils.saveToFile;
import static clawfc.Utils.sprintf;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.stream.Collectors;

import claw.ClawX2T;
import clawfc.ModuleData.ModuleDesignation;
import clawfc.ModuleData.ModuleType;
import clawfc.Utils.ExecuteTasks;
import clawfc.depscan.FortranDepScanner;
import clawfc.depscan.FortranFileBuildInfo;
import clawfc.depscan.FortranFileBuildInfoDeserializer;
import clawfc.depscan.FortranFileBuildInfoSerializer;
import clawfc.depscan.FortranModuleInfo;
import clawfc.utils.AsciiArrayIOStream;
import clawfc.utils.ByteArrayIOStream;
import clawfc.utils.FileInfo;
import clawfc.utils.FileInfoImpl;
import clawfc.utils.PathHashGenerator;
import clawfc.utils.UniquePathHashGenerator;

public class Driver
{
    public static void main(String[] args) throws Exception
    {
        run(args);
        System.exit(0);
    }

    public static void run(String[] args) throws Exception
    {
        Utils.log.setLevel(java.util.logging.Level.INFO);
        Driver driver = new Driver();
        Driver.verifyInstall();
        Options opts = Options.parseCmdlineArguments(args);
        if (!opts.verbose())
        {
            Utils.log.setLevel(java.util.logging.Level.WARNING);
        }
        if (opts != null)
        {
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
        if (!Files.isDirectory(cfg().installRoot()))
        {
            throw new RuntimeException((sprintf("CLAW install directory \"%s\" does not exist or is not a directory",
                    cfg().installRoot())));
        }
        if (!Files.isDirectory(cfg().omniInstallRoot()))
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
            ClawX2T.main(new String[] { "--target-list" });
        } else if (opts.printDirectives())
        {
            ClawX2T.main(new String[] { "--directive-list" });
        } else if (opts.printCfg())
        {
            ArrayList<String> args = new ArrayList<String>(
                    Arrays.asList("--show-config", "--config-path=" + cfg().configDir()));
            String cfg = opts.configFile();
            if (cfg != null)
            {
                args.add("--config=" + cfg);
            }
            ClawX2T.main(args.stream().toArray(String[]::new));
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
                final boolean enableMultiprocessing = !opts.disableMultiprocessing();
                Map<Path, FortranFileBuildInfoData> buildInfoBySrcPath;
                if (!opts.buildInfoIncludeDirs().isEmpty())
                {
                    info("Loading input build information files...");
                    buildInfoBySrcPath = loadBuildInfoFromFiles(opts.buildInfoIncludeDirs(), opts.inputFiles(),
                            opts.sourceIncludeDirs(), enableMultiprocessing);
                } else
                {
                    buildInfoBySrcPath = new HashMap<Path, FortranFileBuildInfoData>();
                }
                info("Creating temp files directory...");
                tmpDir = createTempDir(opts);
                info(tmpDir.toString(), 1);
                info("Creating temp dirs for input files...");
                final Preprocessor pp = new Preprocessor(cfg(), opts, tmpDir);
                info("Preprocessing input files...");
                Map<Path, PreprocessedFortranSourceData> inputPPSrcFiles = preprocessFiles(opts.inputFiles(),
                        buildInfoBySrcPath, pp, opts.skipPreprocessing(), enableMultiprocessing);
                final UniquePathHashGenerator dirHashGen = new UniquePathHashGenerator();
                precomputeDirHashes(dirHashGen, opts.inputFiles(), opts.preprocessorIncludeDirs());
                final Map<Path, Path> ppSrcPathBySrcPath = savePreprocessedSources("input", opts.inputFiles(),
                        inputPPSrcFiles, opts.preprocessedSourcesOutputDir(), dirHashGen, opts.skipPreprocessing(),
                        enableMultiprocessing);
                if (!opts.stopAfterPreprocessing())
                {
                    info("Scanning input files for build information...");
                    scanFiles(opts.inputFiles(), buildInfoBySrcPath, inputPPSrcFiles, ppSrcPathBySrcPath,
                            enableMultiprocessing);
                    if (opts.buildInfoOutputDir() != null)
                    {
                        info("Verifying output buildinfo dir...");
                        getOrCreateDir(opts.buildInfoOutputDir());
                        if (!opts.generateBuildInfoFiles())
                        {
                            saveBuildInfo(opts.inputFiles(), buildInfoBySrcPath, opts.buildInfoOutputDir(), dirHashGen,
                                    enableMultiprocessing);
                        } else
                        {
                            saveBuildInfo(opts.inputFiles(), buildInfoBySrcPath, opts.buildInfoOutputDir(), null,
                                    enableMultiprocessing);
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
                        buildInfoBySrcPath, pp, opts.skipPreprocessing(), enableMultiprocessing);
                Map<Path, Path> ppIncSrcPathBySrcPath = savePreprocessedSources("include", includeFiles, incPPSrcFiles,
                        opts.preprocessedSourcesOutputDir(), dirHashGen, opts.skipPreprocessing(),
                        enableMultiprocessing);
                if (opts.stopAfterPreprocessing())
                {
                    return;
                }
                info("Scanning include files for build information...");
                scanFiles(includeFiles, buildInfoBySrcPath, incPPSrcFiles, ppIncSrcPathBySrcPath,
                        enableMultiprocessing);
                if (opts.buildInfoOutputDir() != null)
                {
                    saveBuildInfo(includeFiles, buildInfoBySrcPath, opts.buildInfoOutputDir(), dirHashGen,
                            enableMultiprocessing);
                }
                if (opts.stopAfterDepScan())
                {
                    return;
                }
                final Map<String, XmodData> inputXmods;
                if (!opts.moduleIncludeDirs().isEmpty())
                {
                    info("Load input Xmod files...");
                    inputXmods = loadXmodFiles(opts.moduleIncludeDirs());
                } else
                {
                    inputXmods = Collections.emptyMap();
                }
                info("Verifying build set...");
                final Map<String, ModuleInfo> availModules = getAvailableModulesInfo(buildInfoBySrcPath,
                        inputPPSrcFiles, incPPSrcFiles, inputXmods);
                final boolean onlyCLAWTargets = !opts.forceTranslation();
                final Set<String> targetModuleNames = getTargetModuleNames(availModules, false, onlyCLAWTargets);
                final Map<String, ModuleInfo> usedModules = Build.removeUnreferencedModules(availModules,
                        targetModuleNames);
                info("Verifying input xmod files...");
                setXmodData(usedModules, targetModuleNames, inputXmods);
                final FortranFrontEnd ffront = new FortranFrontEnd(cfg(), opts, tmpDir);
                if (opts.resolveDependencies())
                {
                    info("Generating xmods...");
                    generateXmods(ffront, usedModules, targetModuleNames, false, enableMultiprocessing,
                            opts.showDebugOutput());
                } else
                {
                    if (opts.generateModFiles())
                    {
                        info("Generating xmods for targets...");
                        generateXmods(ffront, usedModules, targetModuleNames, true, enableMultiprocessing,
                                opts.showDebugOutput());
                    }
                }
                if (opts.xmodOutputDir() != null)
                {
                    saveXmods(usedModules, targetModuleNames, !opts.resolveDependencies(), opts.xmodOutputDir());
                }
                if (opts.generateModFiles())
                {
                    return;
                }
                if (opts.stopAfterXmodGeneration())
                {
                    return;
                }
                info("Generating xast...");
                generateXast(ffront, usedModules, targetModuleNames, enableMultiprocessing, opts.showDebugOutput());
                if (opts.xastOutputDir() != null)
                {
                    saveXast(usedModules, targetModuleNames, opts.xastOutputDir());
                }
                if (opts.stopAfterXastGeneration())
                {
                    return;
                }

            } finally
            {
                if (tmpDir != null && !opts.keepIntermediateFiles())
                {
                    Utils.removeDir(tmpDir);
                }
            }
        }
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

    static class GenerateXmods extends ExecuteTasks
    {
        final FortranFrontEnd ffront;
        final Map<String, ModuleInfo> usedModules;
        final Set<String> targetModuleNames;
        final boolean onlyForTargets;
        final boolean printDebugOutput;

        final BuildOrder buildOrder;
        public final Set<String> successful;
        public final Set<String> failed;

        public GenerateXmods(FortranFrontEnd ffront, Map<String, ModuleInfo> usedModules, Set<String> targetModuleNames,
                final boolean onlyForTargets, boolean enableMultiprocessing, boolean printDebugOutput)
        {
            super(enableMultiprocessing);
            this.ffront = ffront;
            this.usedModules = usedModules;
            this.targetModuleNames = targetModuleNames;
            this.onlyForTargets = onlyForTargets;
            this.printDebugOutput = printDebugOutput;
            buildOrder = Build.getParallelOrder(usedModules, targetModuleNames);
            successful = Collections.synchronizedSet(new LinkedHashSet<String>());
            failed = Collections.synchronizedSet(new LinkedHashSet<String>());
        }

        void submitWaitingTasks()
        {
            for (String modNameIt = buildOrder.next(); modNameIt != null; modNameIt = buildOrder.next())
            {
                final String modName = modNameIt;
                final ModuleInfo modInfo = usedModules.get(modName);
                submitTask(new Callable<Void>()
                {
                    public Void call() throws Exception
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

        boolean generateXmod(ModuleInfo modInfo) throws Exception
        {
            final String modName = modInfo.getName();
            if (modInfo.isProgram())
            {
                info(sprintf("Xmod generation: skipping %s, it is a program", modName,
                        Build.moduleNameWithLocation(modInfo)));
                return true;
            }
            if (modInfo.getXMod() != null)
            {
                info(sprintf("Xmod generation: %s.xmod for dependency module %s is up to date", modName,
                        Build.moduleNameWithLocation(modInfo)));
                return true;
            }
            final boolean isTarget = targetModuleNames.contains(modName);
            if (onlyForTargets && !isTarget)
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
            ByteArrayIOStream xmodDataStrm = new ByteArrayIOStream();
            try
            {
                AsciiArrayIOStream modPPSrc = modInfo.getPreprocessedSrc(true);
                ffront.generateXmod(modInfo.getPPSrcPath(), modPPSrc.getAsInputStreamUnsafe(), xmodDataStrm);
            } catch (FortranFrontEnd.Failed failed)
            {
                String errMsg = sprintf("Xmod generation: Error! Call to Omni frontend for %s failed",
                        Build.moduleNameWithLocation(modInfo));
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
            ((ModuleData) modInfo).setXMod(xmodData);
            return true;
        }
    };

    static void generateXmods(FortranFrontEnd ffront, Map<String, ModuleInfo> usedModules,
            Set<String> targetModuleNames, final boolean onlyForTargets, boolean enableMultiprocessing,
            boolean printDebugOutput) throws Exception
    {
        GenerateXmods genXmodTasks = new GenerateXmods(ffront, usedModules, targetModuleNames, onlyForTargets,
                enableMultiprocessing, printDebugOutput);
        genXmodTasks.run();
        final boolean res = genXmodTasks.failed.isEmpty() && (genXmodTasks.successful.size() == usedModules.size());
        if (!res)
        {
            String errMsg = sprintf("Xmod generation failed: only %s out of %s successful",
                    genXmodTasks.successful.size(), usedModules.size());
            throw new Exception(errMsg);
        }
    }

    static void saveXmods(Map<String, ModuleInfo> usedModules, Set<String> targetModuleNames,
            final boolean onlyForTargets, final Path outDirPath) throws Exception
    {// It is best to save xmods in order so that file modifications dates will be in
     // the right sequence
        BuildOrder buildOrder = Build.getParallelOrder(usedModules, targetModuleNames);
        for (String modName = buildOrder.next(); modName != null; modName = buildOrder.next())
        {
            if (!(onlyForTargets && !targetModuleNames.contains(modName)))
            {
                final ModuleInfo modInfo = usedModules.get(modName);
                if (modInfo.isModule())
                {
                    final XmodData xmodData = modInfo.getXMod();
                    xmodData.save(outDirPath);
                }
            }
            buildOrder.onProcessed(modName);
        }
    }

    static void generateXast(FortranFrontEnd ffront, Map<String, ModuleInfo> usedModules, Set<String> targetModuleNames,
            boolean enableMultiprocessing, boolean printFfrontDebugOutput) throws Exception
    {
        final int n = targetModuleNames.size();
        List<Callable<Void>> tasks = new ArrayList<Callable<Void>>(n);
        final ThreadLocal<AddIgnoreDirectiveFilter> addIgnoreFilter = new ThreadLocal<AddIgnoreDirectiveFilter>();
        for (final String modName : targetModuleNames)
        {
            final ModuleInfo modInfo = usedModules.get(modName);
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
                    AsciiArrayIOStream modPPSrcWithIgnore = new AsciiArrayIOStream();
                    try
                    {
                        localAddIgnoreFilter.run(modPPSrc.getAsInputStreamUnsafe(), modPPSrcWithIgnore);
                    } catch (Exception e)
                    {
                        String errMsg = sprintf("Exception throws while applying ignore directive to %s",
                                Build.moduleNameWithLocation(modInfo));
                        throw new Exception(errMsg, e);
                    }
                    modPPSrc = null;
                    try
                    {
                        ffront.generateAST(modInfo.getPPSrcPath(), modPPSrcWithIgnore.getAsInputStreamUnsafe(),
                                xastDataStrm);
                    } catch (FortranFrontEnd.Failed failed)
                    {
                        String errMsg = sprintf("Xmod generation: Error! Call to Omni frontend for %s failed",
                                Build.moduleNameWithLocation(modInfo));
                        if (printFfrontDebugOutput)
                        {
                            errMsg += "\n" + failed.getMessage();
                        }
                        error(errMsg);
                        throw new Exception(errMsg, failed);
                    }
                    ((ModuleData) modInfo).setXast(xastDataStrm);
                    return null;
                }
            });
        }
        executeTasksUntilFirstError(tasks, enableMultiprocessing);
    }

    static void saveXast(Map<String, ModuleInfo> usedModules, Set<String> targetModuleNames, final Path outDirPath)
            throws Exception
    {
        getOrCreateDir(outDirPath);
        for (final String modName : targetModuleNames)
        {
            final ModuleInfo modInfo = usedModules.get(modName);
            AsciiArrayIOStream xast = modInfo.getXast();
            final Path outFilePath = outDirPath.resolve(modName + ".xast");
            saveToFile(xast.getAsInputStreamUnsafe(), outFilePath);
        }
    }

    static Map<String, ModuleInfo> getAvailableModulesInfo(Map<Path, FortranFileBuildInfoData> buildInfoBySrcPath,
            Map<Path, PreprocessedFortranSourceData> inputPPSrcFiles,
            Map<Path, PreprocessedFortranSourceData> incPPSrcFiles, Map<String, XmodData> inputXmods) throws Exception
    {
        Map<String, ModuleInfo> infoByName = new HashMap<String, ModuleInfo>();
        // Add all modules with infos
        for (Map.Entry<Path, FortranFileBuildInfoData> entry : buildInfoBySrcPath.entrySet())
        {
            final Path srcFilePath = entry.getKey();
            final FortranFileBuildInfoData fileData = entry.getValue();
            ModuleDesignation modDesignation;
            PreprocessedFortranSourceData srcData;
            srcData = inputPPSrcFiles.get(srcFilePath);
            if (srcData != null)
            {
                modDesignation = ModuleDesignation.Input;
            } else
            {
                srcData = incPPSrcFiles.get(srcFilePath);
                if (srcData != null)
                {
                    modDesignation = ModuleDesignation.Include;
                } else
                {// Should be unreachable
                    final String errStr = sprintf(
                            "FortranFileBuildInfoData for %s does not have corresponding preprocessed source data",
                            srcFilePath);
                    throw new Exception(errStr);
                }
            }
            for (FortranModuleInfo modInfo : fileData.getInfo().getModules())
            {
                final String modName = modInfo.getName();
                final ModuleData modData = new ModuleData(ModuleType.Module, modDesignation, modInfo, fileData,
                        srcData);
                ModuleInfo oldData = infoByName.put(modName, modData);
                if (oldData != null)
                {
                    final String errStr = sprintf("Module %s is defined in 2 source files:\n\t%s\n\t%s", modName,
                            oldData.getSrcPath(), modData.getSrcPath());
                    throw new Exception(errStr);
                }
            }
            if (fileData.getInfo().getProgram() != null)
            {
                final FortranModuleInfo modInfo = fileData.getInfo().getProgram();
                final String modName = modInfo.getName();
                final ModuleData modData = new ModuleData(ModuleType.Program, modDesignation, modInfo, fileData,
                        srcData);
                ModuleInfo oldData = infoByName.put(modName, modData);
                if (oldData != null)
                {
                    final String errStr = sprintf("Program %s is defined in 2 source files:\n\t%s\n\t%s", modName,
                            oldData.getSrcPath(), modData.getSrcPath());
                    throw new Exception(errStr);
                }
            }
        }
        // Add all modules without buildinfo
        for (Map.Entry<String, XmodData> entry : inputXmods.entrySet())
        {
            final String modName = entry.getKey();
            ModuleInfo modData = infoByName.get(modName);
            if (modData == null)
            {
                modData = new ModuleData(modName, ModuleDesignation.Include, entry.getValue());
                infoByName.put(modName, modData);
            }
        }
        return Collections.unmodifiableMap(infoByName);
    }

    static void setXmodData(Map<String, ModuleInfo> infoByName, Set<String> targetModuleNames,
            Map<String, XmodData> inputXmods)
    {
        BuildOrder buildOrder = Build.getParallelOrder(infoByName, targetModuleNames);
        Map<String, FileTime> lastTS = new HashMap<String, FileTime>();
        while (!buildOrder.done())
        {
            final String modName = buildOrder.next();
            ModuleInfo modInfo = infoByName.get(modName);
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
                        ((ModuleData) modInfo).setXMod(inXmod);
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

    static Set<String> getTargetModuleNames(Map<String, ModuleInfo> availModules, boolean onlyModules, boolean onlyCLAW)
    {
        Set<String> targetModules = new LinkedHashSet<String>();
        for (Map.Entry<String, ModuleInfo> entry : availModules.entrySet())
        {
            String modName = entry.getKey();
            ModuleInfo data = entry.getValue();
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

    static void printCLAWFiles(List<Path> inputFiles, Map<Path, FortranFileBuildInfoData> binfoBySrcPath)
    {
        for (Path inputFilePath : inputFiles)
        {
            boolean usesCLAW = false;
            FortranFileBuildInfo binfo = binfoBySrcPath.get(inputFilePath).getInfo();
            for (FortranModuleInfo mInfo : binfo.getModules())
            {
                if (mInfo.getUsesClaw())
                {
                    usesCLAW = true;
                    break;
                }
            }
            if (binfo.getProgram() != null)
            {
                usesCLAW |= binfo.getProgram().getUsesClaw();
            }
            if (usesCLAW)
            {
                println(inputFilePath.toString());
            }
        }
    }

    static Map<String, XmodData> loadXmodFiles(List<Path> includeDirs) throws Exception
    {
        Map<String, XmodData> modByName = new LinkedHashMap<String, XmodData>();
        List<Path> uniqueDirs = BuildInfo.createDirListFromPaths(includeDirs);
        Map<Path, List<Path>> incDirFiles = BuildInfo.createModuleDirFileLists(uniqueDirs);
        for (Map.Entry<Path, List<Path>> dirFiles : incDirFiles.entrySet())
        {
            for (Path modFilePath : dirFiles.getValue())
            {
                final String modFileName = modFilePath.getFileName().toString();
                final String modName = removeExtension(modFileName);
                XmodData data = new XmodData(modName, new FileInfoImpl(modFilePath));
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
            final Map<Path, FortranFileBuildInfoData> buildInfoBySrcPath, final Preprocessor pp,
            final boolean skipPreprocessing, boolean enableMultiprocessing) throws Exception
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
                    FortranFileBuildInfoData binfoData = buildInfoBySrcPath.get(inputSrcFilePath);
                    PreprocessedFortranSourceData ppSrcFileData = null;
                    if (binfoData != null)
                    {
                        ppSrcFileData = PreprocessedFortranSourceData.load(inputSrcFilePath, binfoData.getInfo());
                    } else if (!skipPreprocessing)
                    {
                        ppSrcFileData = PreprocessedFortranSourceData.load(inputSrcFilePath, pp);
                    } else
                    {
                        ppSrcFileData = PreprocessedFortranSourceData.load(inputSrcFilePath);
                    }
                    ppSrcData[taskIdx] = ppSrcFileData;
                    return null;
                }
            });
        }
        executeTasksUntilFirstError(tasks, enableMultiprocessing);
        Map<Path, PreprocessedFortranSourceData> ppSrcByPath = new LinkedHashMap<Path, PreprocessedFortranSourceData>();
        for (int i = 0; i < n; ++i)
        {
            ppSrcByPath.put(inputSrcFiles.get(i), ppSrcData[i]);
        }
        return Collections.unmodifiableMap(ppSrcByPath);
    }

    static Map<Path, Path> savePreprocessedSources(String category, List<Path> srcPaths,
            Map<Path, PreprocessedFortranSourceData> ppSrcByPath, final Path outDirPath,
            final PathHashGenerator pathHashGen, boolean skipPP, boolean enableMultiprocessing) throws Exception
    {
        Map<Path, Path> ppSrcPathBySrcPath;
        if (outDirPath != null)
        {
            info("Verifying output directory for preprocessed sources...");
            getOrCreateDir(outDirPath);
            info(sprintf("Saving preprocessed %s sources...", category));
            ppSrcPathBySrcPath = savePreprocessedSources(srcPaths, ppSrcByPath, outDirPath, pathHashGen,
                    enableMultiprocessing);
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
            final PathHashGenerator pathHashGen, boolean enableMultiprocessing) throws Exception
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
        executeTasksUntilFirstError(tasks, enableMultiprocessing);
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

    static Map<Path, FortranFileBuildInfoData> loadBuildInfoFromFiles(List<Path> binfoDirs, List<Path> inputFiles,
            List<Path> includeDirs, boolean enableMultiprocessing) throws Exception
    {
        final List<Path> binfoFilePaths = createBuildInfoFilesList(binfoDirs);
        final int n = binfoFilePaths.size();
        FortranFileBuildInfoData[] data = new FortranFileBuildInfoData[n];
        // -----------------------------------------------
        List<Callable<Void>> tasks = new ArrayList<Callable<Void>>();
        final ThreadLocal<FortranFileBuildInfoDeserializer> deserializer = new ThreadLocal<FortranFileBuildInfoDeserializer>();
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
                    FortranFileBuildInfoDeserializer localDeserializer = deserializer.get();
                    if (localDeserializer == null)
                    {
                        localDeserializer = new FortranFileBuildInfoDeserializer(true);
                        deserializer.set(localDeserializer);
                    }
                    try
                    {
                        FortranFileBuildInfoData binfoData = FortranFileBuildInfoData.load(binfoPath,
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
                    } catch (FortranFileBuildInfoData.LoadFailed e)
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
        executeTasksUntilFirstError(tasks, enableMultiprocessing);
        Map<Path, FortranFileBuildInfoData> res = new HashMap<Path, FortranFileBuildInfoData>();
        for (FortranFileBuildInfoData binfoData : data)
        {
            if (binfoData != null)
            {
                final Path srcFilePath = binfoData.info.getSrcFilePath();
                FortranFileBuildInfoData prevData = res.put(srcFilePath, binfoData);
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

    static void saveBuildInfo(List<Path> srcPaths, Map<Path, FortranFileBuildInfoData> binfoBySrcPath,
            final Path outBuildInfoDir, final PathHashGenerator pathHashGen, boolean enableMultiprocessing)
            throws Exception
    {
        final ThreadLocal<FortranFileBuildInfoSerializer> serializer = new ThreadLocal<FortranFileBuildInfoSerializer>();
        List<Callable<Void>> tasks = new ArrayList<Callable<Void>>();
        for (final Path srcPath : srcPaths)
        {
            tasks.add(new Callable<Void>()
            {
                public Void call() throws Exception
                {
                    FortranFileBuildInfoSerializer localSerializer = serializer.get();
                    if (localSerializer == null)
                    {
                        localSerializer = new FortranFileBuildInfoSerializer();
                        serializer.set(localSerializer);
                    }
                    FortranFileBuildInfoData data = binfoBySrcPath.get(srcPath);
                    final String dirHash = pathHashGen != null ? pathHashGen.generate(srcPath.getParent()) : null;
                    data.save(outBuildInfoDir, dirHash, localSerializer);
                    return null;
                }
            });
        }
        executeTasksUntilFirstError(tasks, enableMultiprocessing);
    }

    static void scanFiles(List<Path> inputFiles, Map<Path, FortranFileBuildInfoData> binfoBySrcPath,
            Map<Path, PreprocessedFortranSourceData> ppSrcBySrcPath, Map<Path, Path> ppSrcPathBySrcPath,
            boolean enableMultiprocessing) throws Exception
    {
        final int n = inputFiles.size();
        List<Callable<Void>> tasks = new ArrayList<Callable<Void>>();
        final FortranFileBuildInfoData[] binfoDataLst = new FortranFileBuildInfoData[n];
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
                    FortranFileBuildInfo binfo;
                    try
                    {
                        binfo = localScanner.scan(ppSrc.getAsInputStreamUnsafe());
                    } catch (Exception e)
                    {
                        Path ppSrcPath = ppSrcPathBySrcPath.get(srcPath);
                        String errMsg;
                        if (ppSrcPath == null)
                        {
                            errMsg = sprintf("Exception thrown while scanning (preprocessed) file \"%s\"", srcPath);
                        } else
                        {
                            errMsg = sprintf("Exception thrown while scanning \"%s\" (preprocessed \"%s\")", ppSrcPath,
                                    srcPath);
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
                    FortranFileBuildInfoData binfoData = new FortranFileBuildInfoData(binfo);
                    binfoBySrcPath.put(srcPath, binfoData);
                    binfoDataLst[taskIdx] = binfoData;
                    return null;
                }
            });
        }
        executeTasksUntilFirstError(tasks, enableMultiprocessing);
        for (int i = 0; i < n; ++i)
        {
            final FortranFileBuildInfoData binfoData = binfoDataLst[i];
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
            } else if (opts.stopAfterDepResolution())
            {
            } else if (opts.stopAfterXastGeneration())
            {

            } else if (opts.stopAfterTranslation())
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
                        throw new Exception(sprintf("Output dir must be specified when multiple input files are used"));
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