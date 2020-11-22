/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import claw.ClawX2T;
import clawfc.depscan.FortranDepScanner;
import clawfc.depscan.FortranFileSummary;
import clawfc.depscan.FortranFileSummaryDeserializer;
import clawfc.depscan.FortranFileSummarySerializer;
import clawfc.depscan.FortranModuleInfo;
import clawfc.utils.AsciiArrayIOStream;

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
        driver.verifyInstall();
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
            throw new RuntimeException((String.format(
                    "CLAW install directory \"%s\" does not exist or is not a directory", cfg().installRoot())));
        }
        if (!Files.isDirectory(cfg().omniInstallRoot()))
        {
            throw new RuntimeException(
                    String.format("OMNI XCodeML Tools install directory \"%s\" does not exist or is not a directory",
                            cfg().omniInstallRoot()));
        }
        if (!Files.isExecutable(cfg().omniFrontEnd()))
        {
            throw new RuntimeException(
                    String.format("OMNI XCodeML Tools Fortran Frontend \"%s\" does not exist or is not a directory",
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
                        String.format("OMNI XCodeML Tools version mismatch\n\texpected: \"%s\"\n\tgot: \"%s\"",
                                cfg().omniVersionTag(), omniVersionTag));
            }
        }
    }

    static class SourceFileData
    {
        public Path inDir;
        public Path inFilename;
        public final FileTime inTimestamp;

        public Path inPath()
        {
            return inDir.resolve(inFilename);
        }

        public AsciiArrayIOStream pp;
        public Path tempDir;
        public Path ppFilename;
        public FortranFileSummary info;
        public Path infoFilename;

        public Path ppPath()
        {
            return tempDir.resolve(ppFilename);
        }

        public Path ppErrLogPath()
        {
            return tempDir.resolve(ppFilename + ".log");
        }

        public Path infoFilePath()
        {
            return infoFilePath(tempDir);
        }

        public Path infoFilePath(Path dir)
        {
            return dir.resolve(inFilename + ".fif");
        }

        public SourceFileData(Path inPath) throws IOException
        {
            inDir = Utils.dirPath(inPath);
            inFilename = inPath.getFileName();
            inTimestamp = Files.getLastModifiedTime(inPath);
            pp = null;
            tempDir = null;
            ppFilename = null;
            info = null;
            infoFilename = null;
        }
    }

    static Path getOrCreateDir(Path dirPath) throws IOException
    {
        if (!Utils.dirExists(dirPath))
        {
            Files.createDirectories(dirPath);
        }
        return dirPath;
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
            Path tmpDir = null;
            try
            {
                Map<String, ModuleData> inputModules;
                if (!opts.moduleIncludeDirs().isEmpty())
                {
                    info("Searching for module files...");
                    inputModules = searchForModules(opts.moduleIncludeDirs());
                } else
                {
                    inputModules = Collections.unmodifiableMap(new HashMap<String, ModuleData>());
                }
                Map<Path, FortranFileSummary> buildInfoBySrcPath;
                if (!opts.buildInfoIncludeDirs().isEmpty())
                {
                    info("Loading input build information files...");
                    buildInfoBySrcPath = loadBuildInfoFromFiles(opts.buildInfoIncludeDirs(),
                            !opts.disableMultiprocessing());
                } else
                {
                    buildInfoBySrcPath = Collections.unmodifiableMap(new HashMap<Path, FortranFileSummary>());
                }
                info("Creating temp files directory...");
                tmpDir = createTempDir(opts);
                info(tmpDir.toString(), 1);
                info("Creating temp dirs for input files...");
                SourceFileData[] inputFilesData = createInputData(opts.inputFiles());
                Path inputTmpDir = createInputFilesTempDir(inputFilesData, tmpDir);
                info("Preprocessing input files...");
                FortranPreprocessor pp = new FortranPreprocessor(cfg(), opts);
                preprocessFiles(inputFilesData, pp, opts.keepIntermediateFiles(), !opts.disableMultiprocessing(),
                        opts.skipPreprocessing(), null, null);
                if (!opts.stopAfterPreprocessing())
                {
                    info("Scanning input files for build information...");
                    if (inputFilesData.length > 0)
                    {
                        scanFiles(inputFilesData, opts.keepIntermediateFiles(), opts.generateDepInfoFiles(),
                                getOrCreateDir(opts.outputDir()), !opts.disableMultiprocessing(), buildInfoBySrcPath);
                    }
                    if (opts.generateDepInfoFiles())
                    {
                        return;
                    }
                }
                info("Creating temp dirs for include files...");
                SourceFileData[] includeFilesData = createIncludeFilesData(opts.sourceIncludeDirs(), tmpDir);
                info("Preprocessing include files...");
                preprocessFiles(includeFilesData, pp, opts.keepIntermediateFiles(), !opts.disableMultiprocessing(),
                        opts.skipPreprocessing(), buildInfoBySrcPath, inputModules);
                if (opts.stopAfterPreprocessing())
                {
                    return;
                }
                info("Scanning include files for build information...");
                scanFiles(includeFilesData, opts.keepIntermediateFiles(), false, null, !opts.disableMultiprocessing(),
                        buildInfoBySrcPath);
                if (opts.stopAfterDepScan())
                {
                    return;
                }
                if (opts.printCLAWFiles())
                {
                    info("Filtering CLAW files...");
                    printCLAWFiles(inputFilesData);
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

    static void printCLAWFiles(SourceFileData[] inputFilesData)
    {
        for (SourceFileData data : inputFilesData)
        {
            boolean usesCLAW = false;
            for (FortranModuleInfo mInfo : data.info.getModules())
            {
                if (mInfo.getUsesClaw())
                {
                    usesCLAW = true;
                    break;
                }
            }
            if (data.info.getProgram() != null)
            {
                usesCLAW |= data.info.getProgram().getUsesClaw();
            }
            if (usesCLAW)
            {
                println(data.inPath().toString());
            }
        }
    }

    static void executeUntilFirstError(List<Callable<Void>> tasks, boolean useMultiProcessing) throws Exception
    {
        List<Future<Void>> taskFutures = new ArrayList<Future<Void>>(tasks.size());
        ExecutorService es = createThreadPool(useMultiProcessing);
        try
        {
            for (Callable<Void> task : tasks)
            {
                taskFutures.add(es.submit(task));
            }
            for (Future<Void> taskFuture : taskFutures)
            {
                try
                {
                    taskFuture.get();
                } catch (InterruptedException e)
                {
                } catch (ExecutionException e)
                {
                    Throwable cause = e.getCause();
                    if (cause instanceof Exception)
                    {
                        throw (Exception) cause;
                    } else
                    {
                        throw e;
                    }
                }
            }
        } finally
        {
            es.shutdownNow();
        }
    }

    static ExecutorService createThreadPool(boolean useMultiProcessing)
    {
        if (useMultiProcessing)
        {
            int maxNumThreads = Runtime.getRuntime().availableProcessors();
            return Executors.newFixedThreadPool(maxNumThreads);
        } else
        {
            return Executors.newSingleThreadExecutor();
        }
    }

    static SourceFileData[] createInputData(List<Path> inputFiles) throws IOException
    {
        SourceFileData[] inputFilesData = new SourceFileData[inputFiles.size()];
        for (int i = 0; i < inputFilesData.length; ++i)
        {
            inputFilesData[i] = new SourceFileData(inputFiles.get(i));
        }
        return inputFilesData;
    }

    static class ModuleData
    {
        public final Path filePath;
        public final FileTime timestamp;

        public ModuleData(Path filePath) throws IOException
        {
            this.filePath = filePath;
            timestamp = Files.getLastModifiedTime(filePath);
        }
    }

    static Map<String, ModuleData> searchForModules(List<Path> includeDirs) throws IOException
    {
        Map<String, ModuleData> modByName = new HashMap<String, ModuleData>();
        List<Path> uniqueDirs = BuildInfo.createDirListFromPaths(includeDirs);
        Map<Path, List<Path>> incDirFiles = BuildInfo.createDirFileLists(uniqueDirs);
        for (Map.Entry<Path, List<Path>> dirFiles : incDirFiles.entrySet())
        {
            for (Path modFile : dirFiles.getValue())
            {
                String modName = modFile.getFileName().toString();
                modName = modName.substring(0, modName.length() - BuildInfo.XMOD_EXTENSION.length());
                ModuleData modData = new ModuleData(modFile);
                ModuleData oldData = modByName.put(modName, modData);
                if (oldData != null)
                {
                    throw new RuntimeException(String.format("Duplicate module files \"%s\" and \"%s\"",
                            oldData.filePath, modData.filePath));
                }
            }
        }
        return Collections.unmodifiableMap(modByName);
    }

    static SourceFileData[] createIncludeFilesData(List<Path> includeDirs, Path tmpDir) throws Exception
    {
        SourceFileData[] incFilesData = null;
        Map<Path, Path> incToTmpDir = createIncludeFilesTempDirs(includeDirs, tmpDir);
        Set<Path> uniqueDirs = incToTmpDir.keySet();
        Map<Path, List<Path>> incDirFiles = BuildInfo.createDirFileLists(uniqueDirs);
        int numFiles = 0;
        for (Map.Entry<Path, List<Path>> dirFiles : incDirFiles.entrySet())
        {
            numFiles += dirFiles.getValue().size();
        }
        incFilesData = new SourceFileData[numFiles];
        {
            int i = 0;
            for (Map.Entry<Path, List<Path>> dirFiles : incDirFiles.entrySet())
            {
                for (Path incFilePath : dirFiles.getValue())
                {
                    SourceFileData data = new SourceFileData(incFilePath);
                    data.tempDir = incToTmpDir.get(data.inDir);
                    incFilesData[i] = data;
                    ++i;
                }
            }
        }
        return incFilesData;
    }

    static Map<Path, Path> createIncludeFilesTempDirs(List<Path> includeDirs, Path tmpDir) throws Exception
    {
        List<Path> uniqueDirs = BuildInfo.createDirListFromPaths(includeDirs);
        Path tmpIncFilesDir = tmpDir.resolve("include");
        Map<Path, Path> inputToTemp = new LinkedHashMap<Path, Path>();
        for (Path dataIncDir : uniqueDirs)
        {
            Path tmpIncFileDir = Paths.get(tmpIncFilesDir.toString() + "/" + dataIncDir.toString()).normalize();
            inputToTemp.put(dataIncDir, tmpIncFileDir);
        }
        for (Map.Entry<Path, Path> entry : inputToTemp.entrySet())
        {
            Path tmpInputFileDir = entry.getValue();
            Files.createDirectories(tmpInputFileDir);
        }
        return Collections.unmodifiableMap(inputToTemp);
    }

    static boolean fileRequiresProcessing(Path path, FileTime timestamp,
            Map<Path, FortranFileSummary> buildInfoBySrcPath, Map<String, ModuleData> inputModules) throws IOException
    {
        if (buildInfoBySrcPath != null)
        {
            FortranFileSummary info = buildInfoBySrcPath.get(path);
            if (info != null)
            {
                for (FortranModuleInfo modInfo : info.getModules())
                {
                    ModuleData modData = inputModules.get(modInfo.getName());
                    if (modData != null)
                    {
                        if (modData.timestamp.compareTo(timestamp) < 0)
                        {
                            return true;
                        }
                    } else
                    {
                        return true;
                    }
                }
                return false;
            }
        }
        return true;
    }

    static void preprocessFiles(final SourceFileData[] inputFilesData, final FortranPreprocessor pp,
            final boolean createFiles, boolean enableMultiprocessing, boolean skipPreprocessing,
            final Map<Path, FortranFileSummary> buildInfoBySrcPath, final Map<String, ModuleData> inputModules)
            throws Exception
    {
        List<Callable<Void>> tasks = new ArrayList<Callable<Void>>(inputFilesData.length);
        for (int i = 0; i < inputFilesData.length; ++i)
        {
            final SourceFileData data = inputFilesData[i];
            tasks.add(new Callable<Void>()
            {
                public Void call() throws Exception
                {
                    try
                    {
                        if (!fileRequiresProcessing(data.inPath(), data.inTimestamp, buildInfoBySrcPath, inputModules))
                        {
                            Utils.log
                                    .info(String.format("Preprocessing file \"%s\" is not required...", data.inPath()));
                            return null;
                        }
                        if (createFiles)
                        {
                            data.ppFilename = Paths.get(FortranPreprocessor.outputFilename(data.inPath()));
                        }
                        if (!skipPreprocessing)
                        {
                            Utils.log.info(String.format("Preprocessing file \"%s\"...", data.inPath()));
                            data.pp = pp.apply(data.inPath(), createFiles ? data.ppPath() : null);
                            Utils.log.info(String.format("Finished preprocessing file \"%s\"...", data.inPath()));
                        } else
                        {
                            Utils.log.info(String.format("Copying file \"%s\"...", data.inPath()));
                            data.pp = new AsciiArrayIOStream();
                            try (InputStream inStrm = new FileInputStream(data.inPath().toString()))
                            {
                                Utils.copy(inStrm, data.pp);
                            }
                            if (createFiles)
                            {
                                try (OutputStream outStrm = new FileOutputStream(data.ppPath().toString()))
                                {
                                    Utils.copy(data.pp.getAsInputStreamUnsafe(), outStrm);
                                }
                            }
                            Utils.log.info(String.format("Finished copying file \"%s\"...", data.inPath()));
                        }
                    } catch (FortranPreprocessor.Failed e)
                    {
                        if (createFiles)
                        {
                            Utils.writeTextToFile(data.ppErrLogPath(), e.stderr);
                        }
                        String errMsg = String.format("Exception thrown while preprocessing input file \"%s\"",
                                data.inPath().toString());
                        if (e.stderr != null)
                        {
                            errMsg += "\n" + e.stderr;
                        }
                        throw new Exception(errMsg, e);
                    }
                    return null;
                }
            });
        }
        executeUntilFirstError(tasks, enableMultiprocessing);
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

    static Map<Path, FortranFileSummary> loadBuildInfoFromFiles(List<Path> includeDirs, boolean enableMultiprocessing)
            throws Exception
    {
        List<Path> uniqueDirs = BuildInfo.createDirListFromPaths(includeDirs);
        Map<Path, List<Path>> incDirFiles = BuildInfo.createBuildinfoDirFileLists(uniqueDirs);
        int n = 0;
        for (Map.Entry<Path, List<Path>> incDir : incDirFiles.entrySet())
        {
            n += incDir.getValue().size();
        }
        class Data
        {
            public final Path inFilePath;
            public FortranFileSummary info = null;

            public Data(Path inFilePath)
            {
                this.inFilePath = inFilePath;
            }
        }
        ;
        List<Data> data = new ArrayList<Data>(n);
        // -----------------------------------------------
        List<Callable<Void>> tasks = new ArrayList<Callable<Void>>(n);
        final ThreadLocal<FortranFileSummaryDeserializer> deserializer = new ThreadLocal<FortranFileSummaryDeserializer>();
        // -----------------------------------------------
        for (Map.Entry<Path, List<Path>> incDir : incDirFiles.entrySet())
        {
            for (final Path inPath : incDir.getValue())
            {
                final Data taskData = new Data(inPath);
                data.add(taskData);
                tasks.add(new Callable<Void>()
                {
                    public Void call() throws Exception
                    {
                        FortranFileSummaryDeserializer localDeserializer = deserializer.get();
                        if (localDeserializer == null)
                        {
                            localDeserializer = new FortranFileSummaryDeserializer(true);
                            deserializer.set(localDeserializer);
                        }
                        FortranFileSummary info = null;
                        try (InputStream inStrm = Files.newInputStream(inPath))
                        {
                            info = localDeserializer.deserialize(inStrm);
                        } catch (Exception e)
                        {
                            String errMsg = String.format(
                                    "Exception thrown while loading build information file \"%s\"", inPath.toString());
                            throw new Exception(errMsg, e);
                        }
                        Path srcFilePath = info.getFilePath();
                        if (srcFilePath == null)
                        {
                            Driver.warning(String.format(
                                    "Ignoring build information file \"%s\". It refers to unknown source file ",
                                    inPath));
                            return null;
                        }
                        if (!Utils.fileExists(srcFilePath))
                        {
                            Driver.warning(String.format(
                                    "Ignoring build information file \"%s\". It refers to non-existing source file \"%s\"",
                                    inPath, srcFilePath));
                            return null;
                        }
                        FileTime infoTS = Files.getLastModifiedTime(inPath);
                        FileTime srcTS = Files.getLastModifiedTime(srcFilePath);
                        if (infoTS.compareTo(srcTS) < 0)
                        {
                            Driver.warning(String.format(
                                    "Ignoring build information file \"%s\". It is older than referenced source file \"%s\"",
                                    inPath.toString(), srcFilePath.toString()));
                            return null;
                        }
                        taskData.info = info;
                        return null;
                    }
                });
            }
        }
        executeUntilFirstError(tasks, enableMultiprocessing);
        Map<Path, FortranFileSummary> res = new HashMap<Path, FortranFileSummary>();
        Map<Path, Path> infoBySrc = new HashMap<Path, Path>();
        for (Data taskData : data)
        {
            if (taskData.info != null)
            {
                final Path srcFilePath = taskData.info.getFilePath();
                if (!res.containsKey(srcFilePath))
                {
                    infoBySrc.put(srcFilePath, taskData.inFilePath);
                    res.put(srcFilePath, taskData.info);
                } else
                {
                    Path firstInfoFilePath = infoBySrc.get(srcFilePath);
                    String errFormat = "Ignoring build information file \"%s\" referring to the same source file \"%s\" "
                            + "as earlier processed build information file \"%s\"";
                    Driver.warning(String.format(errFormat, taskData.inFilePath, srcFilePath, firstInfoFilePath));
                }
            }
        }
        return Collections.unmodifiableMap(res);
    }

    static void scanFiles(SourceFileData[] inputFilesData, final boolean createTmpFiles,
            final boolean createOutputFiles, final Path outDir, boolean enableMultiprocessing,
            final Map<Path, FortranFileSummary> buildInfoBySrcPath) throws Exception
    {
        List<Callable<Void>> tasks = new ArrayList<Callable<Void>>(inputFilesData.length);
        final ThreadLocal<FortranDepScanner> scanner = new ThreadLocal<FortranDepScanner>();
        final ThreadLocal<FortranFileSummarySerializer> serializer = new ThreadLocal<FortranFileSummarySerializer>();
        for (int i = 0; i < inputFilesData.length; ++i)
        {
            final SourceFileData data = inputFilesData[i];
            tasks.add(new Callable<Void>()
            {
                public Void call() throws Exception
                {
                    data.info = buildInfoBySrcPath.get(data.inPath());
                    if (data.info == null)
                    {
                        try (InputStream ppInStrm = data.pp.getAsInputStreamUnsafe())
                        {
                            FortranDepScanner localScanner = scanner.get();
                            if (localScanner == null)
                            {
                                localScanner = new FortranDepScanner();
                                scanner.set(localScanner);
                            }
                            data.info = localScanner.scan(ppInStrm);
                            data.info.setFilePath(data.info.getFilePath());
                        } catch (Exception e)
                        {
                            String errMsg = String.format("Exception thrown while scanning (preprocessed) file \"%s\"",
                                    data.inPath().toString());
                            throw new Exception(errMsg, e);
                        }
                    }
                    if (createTmpFiles || createOutputFiles)
                    {
                        FortranFileSummarySerializer localSerializer = serializer.get();
                        if (localSerializer == null)
                        {
                            localSerializer = new FortranFileSummarySerializer();
                            serializer.set(localSerializer);
                        }
                        if (createTmpFiles)
                        {
                            try (OutputStream outStrm = Files.newOutputStream(data.infoFilePath()))
                            {
                                localSerializer.serialize(data.info, outStrm);
                            }
                        }
                        if (createOutputFiles)
                        {
                            data.info.setFilePath(data.inPath());
                            Path outFilePath = data.infoFilePath(outDir);
                            try (OutputStream outStrm = Files.newOutputStream(outFilePath))
                            {
                                localSerializer.serialize(data.info, outStrm);
                            }
                        }
                    }
                    return null;
                }
            });
        }
        executeUntilFirstError(tasks, enableMultiprocessing);
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
            return Files.createTempDirectory(Paths.get(Utils.DEFAULT_TOP_TEMP_DIR), "clawfc");
        }
    }

    static void verifyOptions(Options opts)
    {
        {
            Map<Path, Path> fileNames = new HashMap<Path, Path>();
            for (Path inFilePath : opts.inputFiles())
            {
                if (!Utils.fileExists(inFilePath))
                {
                    throw new RuntimeException(
                            String.format("Input file \"%s\" does not exist or is a directory", inFilePath.toString()));
                }
                Path filename = inFilePath.getFileName();
                Path oldPath = fileNames.put(filename, inFilePath);
                if (oldPath != null)
                {
                    throw new RuntimeException(
                            String.format("Input files cannot have identical names: \n\"%s\"\n\"%s\"",
                                    oldPath.toString(), inFilePath.toString()));
                }
            }

        }
        if (opts.inputFiles().size() == 1)
        {
            if (opts.outputFile() == null && opts.outputDir() == null)
            {
                throw new RuntimeException(String.format("Either output file or output dir must be specified"));
            }
        } else if (opts.inputFiles().size() > 1)
        {
            if (opts.outputDir() == null)
            {
                throw new RuntimeException(
                        String.format("Output dir must be specified when multiple input files are used"));
            }
        }
        if (opts.fortranCompilerType() != null ^ opts.fortranCompilerCmd() != null)
        {
            throw new RuntimeException(String.format("Options --fc-type and --fc-cmd must be specified together"));
        }
        for (Path incPath : opts.preprocessingIncludeDirs())
        {
            if (!Utils.dirExists(incPath))
            {
                throw new RuntimeException(
                        String.format("Include dir \"%s\" does not exist or is not a directory", incPath.toString()));
            }
        }
        for (Path incPath : opts.buildInfoIncludeDirs())
        {
            if (!Utils.dirExists(incPath))
            {
                throw new RuntimeException(String.format(
                        "Buildinfo include dir \"%s\" does not exist or is not a directory", incPath.toString()));
            }
        }
        for (Path incPath : opts.moduleIncludeDirs())
        {
            if (!Utils.dirExists(incPath))
            {
                throw new RuntimeException(String
                        .format("Module include dir \"%s\" does not exist or is not a directory", incPath.toString()));
            }
        }
        if (opts.generateDepInfoFiles())
        {
            if (opts.outputDir() == null)
            {
                throw new RuntimeException("Output dir must be specified with --gen-buildinfo-files");
            }
        }
    }

    static void printVersion()
    {
        String vStr = String.format("%s %s \"%s\" %s ", cfg().name(), cfg().version(), cfg().commit(),
                cfg().omniVersion());
        print(vStr);
    }
};