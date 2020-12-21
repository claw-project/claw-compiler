/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc;

import static clawfc.Utils.ASCII_NEWLINE_VALUE;
import static clawfc.Utils.ASCII_SPACE_VALUE;
import static clawfc.Utils.copy;
import static clawfc.Utils.executeTasksUntilFirstError;
import static clawfc.Utils.fileExists;
import static clawfc.Utils.getOrCreateDir;
import static clawfc.Utils.sprintf;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
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
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.stream.Collectors;

import claw.ClawX2T;
import clawfc.Utils.ExecuteTasks;
import clawfc.depscan.FortranDepScanner;
import clawfc.depscan.FortranFileBuildInfo;
import clawfc.depscan.FortranFileBuildInfoDeserializer;
import clawfc.depscan.FortranFileBuildInfoSerializer;
import clawfc.depscan.FortranModuleInfo;
import clawfc.depscan.FortranSemanticException;
import clawfc.utils.AsciiArrayIOStream;
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

    static class SourceFileData
    {
        final Path inDir;
        final FileInfo inFileInfo;

        public AsciiArrayIOStream pp;
        public AsciiArrayIOStream ppWithResolvedIncludes;
        public Path tempDir;
        public Path ppFilename;
        public FortranFileBuildInfo info;
        public List<FileInfo> incFilesInfo;

        public Path inPath()
        {
            return inFileInfo.getPath();
        }

        public Path ppPath()
        {
            return tempDir.resolve(ppFilename);
        }

        public Path ppErrLogPath()
        {
            return tempDir.resolve(ppFilename + ".log");
        }

        public Path infoFilePath(Path dir)
        {
            return dir.resolve(inPath().getFileName() + ".fif");
        }

        public Path infoFilePath()
        {
            return infoFilePath(tempDir);
        }

        public SourceFileData(Path inPath) throws Exception
        {
            inDir = Utils.dirPath(inPath);
            inFileInfo = new clawfc.utils.FileInfoImpl(inPath);
            pp = null;
            ppWithResolvedIncludes = null;
            tempDir = null;
            ppFilename = null;
            info = null;
            incFilesInfo = null;
        }

        public Path getPath()
        {
            return inFileInfo.getPath();
        }

        public FileTime getLastModifiedTS()
        {
            return inFileInfo.getLastModifiedTS();
        }
    }

    static class BuildInfoFileData
    {
        private FortranFileBuildInfo info;
        public final Path filePath;
        private List<FileInfo> incFilesInfo;

        FortranFileBuildInfo getInfo()
        {
            return info;
        }

        void setIncludeFilesInfo(List<FileInfo> info)
        {
            this.incFilesInfo = info;
        }

        List<FileInfo> getIncludeFilesInfo()
        {
            return incFilesInfo;
        }

        void setInfo(FortranFileBuildInfo info)
        {
            this.info = info;
        }

        public BuildInfoFileData(Path filePath)
        {
            this.info = null;
            this.filePath = filePath;
            this.incFilesInfo = null;
        }

        public BuildInfoFileData(FortranFileBuildInfo info, Path filePath)
        {
            this.info = info;
            this.filePath = filePath;
            this.incFilesInfo = null;
        }
    }

    class PPSourceFileData
    {
        public final AsciiArrayIOStream ppSrc;
        public final Set<Path> incFilePaths;

        public PPSourceFileData(AsciiArrayIOStream ppSrc, Set<Path> incFilePaths)
        {
            this.ppSrc = ppSrc;
            this.incFilePaths = incFilePaths;
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
                /*
                 * Map<String, ModuleFileData> inputModules; if
                 * (!opts.moduleIncludeDirs().isEmpty()) {
                 * info("Searching for module files..."); inputModules =
                 * searchForModules(opts.moduleIncludeDirs()); } else { inputModules =
                 * Collections.emptyMap(); }
                 */
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
                /*
                 * SourceFileData[] inputFilesData = createInputData(opts.inputFiles()); if
                 * (opts.keepIntermediateFiles()) { createInputFilesTempDir(inputFilesData,
                 * tmpDir); }
                 */
                final Preprocessor pp = new Preprocessor(cfg(), opts, tmpDir);
                info("Preprocessing input files...");
                Map<Path, PreprocessedFortranSourceData> inputPPSrcFiles = preprocessFiles(opts.inputFiles(),
                        buildInfoBySrcPath, pp, opts.skipPreprocessing(), enableMultiprocessing);
                final UniquePathHashGenerator dirHashGen = new UniquePathHashGenerator();
                precomputeDirHashes(dirHashGen, opts.inputFiles(), opts.preprocessorIncludeDirs());
                Map<Path, Path> ppSrcPathBySrcPath;
                if (opts.preprocessedSourcesOutputDir() != null)
                {
                    info("Verifying output directory for preprocessed sources...");
                    getOrCreateDir(opts.preprocessedSourcesOutputDir());
                    info("Saving preprocessed input sources...");
                    ppSrcPathBySrcPath = savePreprocessedSources(opts.inputFiles(), inputPPSrcFiles,
                            opts.preprocessedSourcesOutputDir(), dirHashGen, enableMultiprocessing);
                } else
                {
                    ppSrcPathBySrcPath = Collections.emptyMap();
                }
                if (!opts.stopAfterPreprocessing())
                {
                    info("Scanning input files for build information...");
                    scanFiles(opts.inputFiles(), buildInfoBySrcPath, inputPPSrcFiles, ppSrcPathBySrcPath,
                            enableMultiprocessing);
                    if (opts.buildInfoOutputDir() != null)
                    {
                        info("Verifying output buildinfo dir...");
                        getOrCreateDir(opts.buildInfoOutputDir());
                        saveBuildInfo(opts.inputFiles(), buildInfoBySrcPath, opts.buildInfoOutputDir(), dirHashGen,
                                enableMultiprocessing);
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
                Map<Path, Path> ppIncSrcPathBySrcPath;
                if (opts.preprocessedSourcesOutputDir() != null)
                {
                    info("Saving preprocessed include sources...");
                    ppIncSrcPathBySrcPath = savePreprocessedSources(includeFiles, incPPSrcFiles,
                            opts.preprocessedSourcesOutputDir(), dirHashGen, enableMultiprocessing);
                } else
                {
                    ppIncSrcPathBySrcPath = Collections.emptyMap();
                }
                if (opts.stopAfterPreprocessing())
                {
                    return;
                }
                info("Scanning include files for build information...");
                scanFiles(includeFiles, buildInfoBySrcPath, incPPSrcFiles, ppIncSrcPathBySrcPath,
                        enableMultiprocessing);
                if (opts.stopAfterDepScan())
                {
                    return;
                }
                /*
                 * info("Verifying build set..."); final Map<String, ModuleInfo> availModules =
                 * getAvailableModulesInfo(inputFilesData, includeFilesData, inputModules);
                 * final boolean onlyCLAWTargets = !opts.forceTranslation(); final Set<String>
                 * targetModuleNames = getTargetModuleNames(availModules, false,
                 * onlyCLAWTargets); final Map<String, ModuleInfo> usedModules =
                 * Build.removeUnreferencedModules(availModules, targetModuleNames); final
                 * FortranFrontEnd ffront = new FortranFrontEnd(cfg(), opts, tmpDir); if
                 * (opts.resolveDependencies()) { info("Generating xmods...");
                 * generateXmods(ffront, usedModules, targetModuleNames, false,
                 * enableMultiprocessing, opts.showDebugOutput()); } else { if
                 * (opts.generateModFiles()) { info("Generating xmods for targets...");
                 * generateXmods(ffront, usedModules, targetModuleNames, true,
                 * enableMultiprocessing, opts.showDebugOutput()); } } if
                 * (opts.generateModFiles()) { return; } if (opts.stopAfterXmodGeneration()) {
                 * return; }
                 */
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
            final boolean isTarget = targetModuleNames.contains(modName);
            if (onlyForTargets && !isTarget)
            {
                if (!modInfo.hasXModFile())
                {
                    error(sprintf("Xmod generation: Error! Xmod file for dependency module %s is not available",
                            Build.moduleNameWithLocation(modInfo)));
                    return false;
                }
                if (!modInfo.XModIsUpToDate(usedModules))
                {
                    error(sprintf("Xmod generation: Error! Xmod file for dependency module %s is not up to date",
                            Build.moduleNameWithLocation(modInfo)));
                    return false;
                }
                info(sprintf("Xmod generation: %s.xmod for dependency module %s is up to date", modName,
                        Build.moduleNameWithLocation(modInfo)));
                return true;
            }
            final Path outFilePath = ffront.getOutModDir().resolve(modName + ".xmod");
            if (modInfo.XModIsUpToDate(usedModules))
            {
                StringBuilder infoMsg = new StringBuilder();
                infoMsg.append(sprintf("Xmod generation: %s.xmod is up to date", modName));
                if (isTarget)
                {
                    final Path currentXModPath = modInfo.getXModFileInfo().getPath();
                    if (!currentXModPath.equals(outFilePath))
                    {
                        Files.copy(currentXModPath, outFilePath);
                        infoMsg.append('\n');
                        infoMsg.append(sprintf("Xmod generation: copied %s.xmod to output dir", modName));
                    }
                    info(infoMsg.toString());
                }
                return true;
            } else
            {
                if (modInfo.hasSource())
                {
                    AsciiArrayIOStream outStrm = new AsciiArrayIOStream();
                    try
                    {
                        AsciiArrayIOStream modPPSrc = modInfo.getPreprocessedSrc(true);
                        ffront.generateXmod(modInfo.getSrcFileInfo().getPath(), modPPSrc.getAsInputStreamUnsafe(),
                                outStrm);
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
                    try (OutputStream outFile = Files.newOutputStream(outFilePath))
                    {
                        copy(outStrm.getAsInputStreamUnsafe(), outFile);
                    }
                    return true;
                } else
                {
                    error(sprintf("Xmod generation: Error! Source for %s is not available",
                            Build.moduleNameWithLocation(modInfo)));
                    return false;
                }
            }
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

    static class ModuleData implements ModuleInfo
    {
        final String name;
        final boolean _isProgram;
        final boolean _isInput;
        SourceFileData srcFileData;
        ModuleFileData modFileData;
        FortranModuleInfo modInfo;

        public ModuleData(SourceFileData srcData, FortranModuleInfo info, ModuleFileData modFileData, boolean isInput,
                boolean isTarget)
        {
            name = info.getName();
            _isProgram = isInput;
            _isInput = isTarget;
            srcFileData = srcData;
            this.modFileData = modFileData;
            modInfo = info;
        }

        public ModuleData(String name, ModuleFileData modFileData)
        {
            this.name = name;
            _isProgram = false;
            _isInput = false;
            srcFileData = null;
            this.modFileData = modFileData;
            modInfo = null;
        }

        @Override
        public String getName()
        {
            return name;
        }

        @Override
        public boolean isProgram()
        {
            return _isProgram;
        }

        @Override
        public boolean isModule()
        {
            return !isProgram();
        }

        public SourceFileData getSourceFileData()
        {
            return srcFileData;
        }

        @Override
        public boolean isInput()
        {
            return _isInput;
        }

        @Override
        public boolean usesCLAW()
        {
            return modInfo.getUsesClaw();
        }

        @Override
        public boolean hasSource()
        {
            return srcFileData != null;
        }

        @Override
        public Collection<String> getUsedModules()
        {
            if (!hasSource())
            {
                return (Collection<String>) null;
            } else
            {
                return modInfo.getUsedModuleNames();
            }
        }

        @Override
        public clawfc.depscan.FortranModuleInfo getModuleSrcInfo()
        {
            return modInfo;
        }

        @Override
        public FileInfo getSrcFileInfo()
        {
            return srcFileData.inFileInfo;
        }

        @Override
        public FileInfo getXModFileInfo()
        {
            return modFileData;
        }

        @Override
        public boolean hasXModFile()
        {
            return modFileData != null;
        }

        @Override
        public boolean XModIsUpToDate(Map<String, ModuleInfo> availModsByName)
        {
            if (!hasXModFile())
            {
                return false;
            }
            final FileTime modFileTS = getXModFileInfo().getLastModifiedTS();
            if (hasSource())
            {
                final FileTime srcFileTS = getSrcFileInfo().getLastModifiedTS();
                if (srcFileTS.compareTo(modFileTS) > 0)
                {
                    return false;
                }
            }
            for (String depModname : getUsedModules())
            {
                ModuleInfo depModInfo = availModsByName.get(depModname);
                if (depModInfo.hasSource())
                {
                    final FileTime depSrcFileTS = depModInfo.getSrcFileInfo().getLastModifiedTS();
                    if (depSrcFileTS.compareTo(modFileTS) > 0)
                    {
                        return false;
                    }
                }
                if (depModInfo.hasXModFile())
                {
                    final FileTime depModFileTS = depModInfo.getXModFileInfo().getLastModifiedTS();
                    if (depModFileTS.compareTo(modFileTS) > 0)
                    {
                        return false;
                    }
                }
            }
            return true;
        }

        @Override
        public FortranFileBuildInfo getSrcSummary()
        {
            if (srcFileData != null)
            {
                return srcFileData.info;
            } else
            {
                return null;
            }
        }

        @Override
        public List<FileInfo> getIncludeFilesInfo()
        {
            return srcFileData.incFilesInfo;
        }

        @Override
        public AsciiArrayIOStream getPreprocessedSrc(boolean preserveOffset) throws IOException
        {
            AsciiArrayIOStream src = null;
            if (srcFileData.ppWithResolvedIncludes != null)
            {
                src = srcFileData.ppWithResolvedIncludes;
            } else
            {
                src = srcFileData.pp;
            }
            FortranModuleInfo mInfo = getModuleSrcInfo();
            final int startChrIdx = mInfo.getStartCharIdx();
            final int endChrIDx = mInfo.getEndCharIdx();
            final int count = endChrIDx - startChrIdx;
            if (preserveOffset)
            {
                final int lineOffset = mInfo.getStartLineIdx();
                final int lineStartChrOffset = startChrIdx - src.findLineStartChrIdx(mInfo.getStartCharIdx());
                final int size = lineOffset + lineStartChrOffset + count;
                AsciiArrayIOStream buf = new AsciiArrayIOStream(size);
                for (int i = 0; i < lineOffset; ++i)
                {
                    buf.write(ASCII_NEWLINE_VALUE);
                }
                for (int i = 0; i < lineStartChrOffset; ++i)
                {
                    buf.write(ASCII_SPACE_VALUE);
                }
                try (InputStream srcStrm = src.getAsInputStreamUnsafe(startChrIdx, count))
                {
                    copy(srcStrm, buf);
                }
                return buf;
            } else
            {
                return src;
            }
        }
    }

    static class GetAvailableModulesInfo
    {
        public static Map<String, ModuleInfo> run(SourceFileData[] inputFilesData, SourceFileData[] incFilesData,
                Map<String, ModuleFileData> inputModules) throws Exception
        {
            Map<String, ModuleInfo> res = new LinkedHashMap<String, ModuleInfo>();
            for (SourceFileData data : inputFilesData)
            {
                add(res, data, true, inputModules);
            }
            for (SourceFileData data : incFilesData)
            {
                add(res, data, false, inputModules);
            }
            for (Map.Entry<String, ModuleFileData> entry : inputModules.entrySet())
            {
                final String modName = entry.getKey();
                if (!res.containsKey(modName))
                {
                    res.put(modName, new ModuleData(modName, entry.getValue()));
                }
            }
            return Collections.unmodifiableMap(res);
        }

        static void add(Map<String, ModuleInfo> res, SourceFileData inFileData, boolean isInput,
                Map<String, ModuleFileData> inputModules) throws Exception
        {
            for (FortranModuleInfo modInfo : inFileData.info.getModules())
            {
                add(res, inFileData, modInfo, false, isInput, inputModules);
            }
            FortranModuleInfo programInfo = inFileData.info.getProgram();
            if (programInfo != null)
            {
                add(res, inFileData, programInfo, true, isInput, inputModules);
            }
        }

        static void add(Map<String, ModuleInfo> res, SourceFileData inFileData, FortranModuleInfo modInfo,
                boolean isProgram, boolean isInput, Map<String, ModuleFileData> inputModules) throws Exception
        {
            final String modName = modInfo.getName();
            if (res.containsKey(modName))
            {
                ModuleInfo oldData = res.get(modName);
                String errMsg = sprintf("Module \"%s\" defined in file \"%s\" is also defined in \"%s\"", modName,
                        oldData.getSrcFileInfo().getPath());
                throw new FortranSemanticException(errMsg);
            }
            ModuleData data = new ModuleData(inFileData, modInfo, inputModules.get(modName), isProgram, isInput);
            res.put(modName, data);
        }
    }

    static Map<String, ModuleInfo> getAvailableModulesInfo(SourceFileData[] inputFilesData,
            SourceFileData[] incFilesData, Map<String, ModuleFileData> inputModules) throws Exception
    {
        return GetAvailableModulesInfo.run(inputFilesData, incFilesData, inputModules);
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

    static SourceFileData[] createInputData(List<Path> inputFiles) throws Exception
    {
        SourceFileData[] inputFilesData = new SourceFileData[inputFiles.size()];
        for (int i = 0; i < inputFilesData.length; ++i)
        {
            inputFilesData[i] = new SourceFileData(inputFiles.get(i));
        }
        return inputFilesData;
    }

    static class ModuleFileData implements FileInfo
    {
        final Path filePath;
        final FileTime timestamp;

        public Path getPath()
        {
            return filePath;
        }

        public FileTime getLastModifiedTS()
        {
            return timestamp;
        }

        public ModuleFileData(Path filePath) throws IOException
        {
            this.filePath = filePath;
            timestamp = Files.getLastModifiedTime(filePath);
        }
    }

    static Map<String, ModuleFileData> searchForModules(List<Path> includeDirs) throws Exception
    {
        Map<String, ModuleFileData> modByName = new HashMap<String, ModuleFileData>();
        List<Path> uniqueDirs = BuildInfo.createDirListFromPaths(includeDirs);
        Map<Path, List<Path>> incDirFiles = BuildInfo.createModuleDirFileLists(uniqueDirs);
        for (Map.Entry<Path, List<Path>> dirFiles : incDirFiles.entrySet())
        {
            for (Path modFile : dirFiles.getValue())
            {
                final String modFileName = modFile.getFileName().toString();
                final String modName = modFileName.substring(0, modFileName.lastIndexOf('.'));// Remove extension
                ModuleFileData modData = new ModuleFileData(modFile);
                ModuleFileData oldData = modByName.put(modName, modData);
                if (oldData != null)
                {
                    throw new Exception(
                            sprintf("Duplicate module files \"%s\" and \"%s\"", oldData.filePath, modData.filePath));
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

    static Path createInputFilesTempDir(SourceFileData[] inputFilesData, Path tmpDir) throws IOException
    {
        Path tmpInputFilesDir = tmpDir.resolve("input");
        for (SourceFileData data : inputFilesData)
        {
            data.tempDir = tmpInputFilesDir;
        }
        Files.createDirectories(tmpInputFilesDir);
        return tmpInputFilesDir;
    }

    static Map<Path, FortranFileBuildInfoData> loadBuildInfoFromFiles(List<Path> binfoDirs, List<Path> inputFiles,
            List<Path> includeDirs, boolean enableMultiprocessing) throws Exception
    {
        List<Path> uniqueDirs = BuildInfo.createDirListFromPaths(binfoDirs);
        Map<Path, List<Path>> binfoDirFiles = BuildInfo.createBuildinfoDirFileLists(uniqueDirs);
        int n = 0;
        for (Map.Entry<Path, List<Path>> incDir : binfoDirFiles.entrySet())
        {
            n += incDir.getValue().size();
        }
        FortranFileBuildInfoData[] data = new FortranFileBuildInfoData[n];
        // -----------------------------------------------
        List<Callable<Void>> tasks = new ArrayList<Callable<Void>>();
        final ThreadLocal<FortranFileBuildInfoDeserializer> deserializer = new ThreadLocal<FortranFileBuildInfoDeserializer>();
        // -----------------------------------------------
        final Set<Path> inputFilesSet = Collections.unmodifiableSet(new HashSet<Path>(inputFiles));
        final Set<Path> includeDirsSet = Collections.unmodifiableSet(new HashSet<Path>(includeDirs));
        int i = 0;
        for (Map.Entry<Path, List<Path>> binfoDir : binfoDirFiles.entrySet())
        {
            for (final Path binfoPath : binfoDir.getValue())
            {
                final int dataIdx = i;
                ++i;
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
                    final String dirHash = pathHashGen.generate(srcPath.getParent());
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
            final Path srcPath = inputFiles.get(i);
            final FortranFileBuildInfoData binfoData = binfoDataLst[i];
            binfoBySrcPath.put(srcPath, binfoData);
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
                if (opts.preprocessedSourcesOutputDir() == null)
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
                if (opts.outputModulesDir() == null)
                {
                    throw new Exception("Output directory for .xmod files must be specified");
                }
                return;
            }
            if (opts.stopAfterXmodGeneration())
            {
            } else if (opts.stopAfterDepResolution())
            {
            } else if (opts.stopAfterOmniFFront())
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