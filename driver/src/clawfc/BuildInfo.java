/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.NotDirectoryException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.regex.Pattern;

import clawfc.depscan.FortranDepScanner;
import clawfc.depscan.FortranException;
import clawfc.depscan.FortranFileBasicSummary;
import clawfc.depscan.FortranModuleBasicInfo;
import clawfc.depscan.FortranSemanticException;
import clawfc.depscan.FortranSyntaxException;

public class BuildInfo
{
    static final Pattern FORTRAN_FILE_SEARCH_PATTERN = Pattern
            .compile(String.format(".+\\.(%s)", String.join("|", Utils.FORTRAN_FILE_EXTENSIONS)));

    Map<Path, FortranFileInfo> _fileInformation;
    Map<Path, Set<Path>> _fileDependencies;

    public Map<Path, FortranFileInfo> fileInformation()
    {
        return _fileInformation;
    }

    public Map<Path, Set<Path>> fileDependencies()
    {
        return _fileDependencies;
    }

    public static class FortranFileInfo
    {
        List<String> _definedModules;
        List<String> _usedModules;
        FileTime _lastModifiedTimestamp;

        public List<String> definedModules()
        {
            return _definedModules;
        }

        public List<String> usedModules()
        {
            return _usedModules;
        }

        public FileTime lastModifiedTimestamp()
        {
            return _lastModifiedTimestamp;
        }

        public FortranFileInfo(List<String> definedModules, List<String> usedModules, FileTime lastModifiedTimestamp)
        {
            _definedModules = definedModules;
            _usedModules = usedModules;
            _lastModifiedTimestamp = lastModifiedTimestamp;
        }

        @Override
        public boolean equals(Object obj)
        {
            if (this == obj)
            {
                return true;
            }
            if (obj == null)
            {
                return false;
            }
            if (getClass() != obj.getClass())
            {
                return false;
            }
            FortranFileInfo other = (FortranFileInfo) obj;
            if (!definedModules().equals(other.definedModules()))
            {
                return false;
            }
            if (!usedModules().equals(other.usedModules()))
            {
                return false;
            }
            if (!lastModifiedTimestamp().equals(other.lastModifiedTimestamp()))
            {
                return false;
            }
            return true;
        }

        public FortranFileInfo(Path filePath) throws FortranException, IOException, Exception
        {
            _lastModifiedTimestamp = Files.getLastModifiedTime(filePath);
            FortranDepScanner depScanner = new FortranDepScanner();
            FortranFileBasicSummary depInfo;
            try
            {
                depInfo = depScanner.basicScan(Files.newInputStream(filePath));
            } catch (FortranSemanticException e)
            {
                throw new FortranSemanticException(
                        String.format("Exception thrown while processing Fortran file \"%s\": %s", filePath.toString(),
                                e.getMessage()),
                        e.line(), e.charPositionInLine());
            } catch (FortranSyntaxException e)
            {
                throw new FortranSyntaxException(
                        String.format("Exception thrown while processing Fortran file \"%s\": %s", filePath.toString(),
                                e.getMessage()),
                        e.line(), e.charPositionInLine());
            }
            _definedModules = new ArrayList<String>();
            HashSet<String> usedModules = new HashSet<String>();
            for (FortranModuleBasicInfo moduleDeps : depInfo.modules)
            {
                _definedModules.add(moduleDeps.name);
                for (String name : moduleDeps.usedModuleNames)
                {
                    usedModules.add(name);
                }
            }
            if (depInfo.program != null)
            {
                for (String name : depInfo.program.usedModuleNames)
                {
                    usedModules.add(name);
                }
            }
            for (String name : _definedModules)
            {
                usedModules.remove(name);
            }
            _usedModules = new ArrayList<String>(usedModules);
            Collections.sort(_usedModules);
            _usedModules = Collections.unmodifiableList(_usedModules);
            _definedModules = Collections.unmodifiableList(_definedModules);
        }
    }

    public static List<Path> createDirList(List<String> dirs) throws FileNotFoundException, NotDirectoryException
    {
        List<Path> dirPaths = new ArrayList<Path>(dirs.size());
        for (String s : dirs)
        {
            Path path = Paths.get(s).normalize();
            dirPaths.add(path);
        }
        return createDirListFromPaths(dirPaths);
    }

    public static List<Path> createDirListFromPaths(List<Path> dirs) throws FileNotFoundException, NotDirectoryException
    {
        SortedSet<Path> res = new TreeSet<Path>();
        for (Path path : dirs)
        {
            if (Files.notExists(path))
            {
                throw new FileNotFoundException(
                        String.format("Include directory \"%s\" does not exist", path.toString()));
            }
            if (!Files.isDirectory(path))
            {
                throw new NotDirectoryException(
                        String.format("Include path \"%s\" is not a directory", path.toString()));
            }
            res.add(path);
        }
        return Collections.unmodifiableList(new ArrayList<Path>(res));
    }

    public static List<Path> createDirFileList(Path dir) throws IOException
    {
        ArrayList<Path> files = new ArrayList<Path>();
        Files.list(dir).filter(s -> FORTRAN_FILE_SEARCH_PATTERN.matcher(s.toString()).matches()).forEach(files::add);
        Collections.sort(files);
        return Collections.unmodifiableList(files);
    }

    public static Map<Path, List<Path>> createDirFileLists(Collection<Path> dirs) throws IOException
    {
        Map<Path, List<Path>> res = new LinkedHashMap<Path, List<Path>>();
        for (Path dir : dirs)
        {
            res.put(dir, createDirFileList(dir));
        }
        return Collections.unmodifiableMap(res);
    }

    public static Map<Path, FortranFileInfo> scanFortranFiles(Map<Path, List<Path>> dirFileLists)
            throws FortranException, IOException, Exception
    {
        return scanFortranFiles(dirFileLists, false);
    }

    public static Map<Path, FortranFileInfo> scanFortranFiles(Map<Path, List<Path>> dirFileLists,
            boolean useMultithreading) throws FortranException, IOException, Exception
    {
        LinkedHashMap<Path, FortranFileInfo> fileInfoMap = new LinkedHashMap<Path, FortranFileInfo>();
        if (!useMultithreading)
        {
            for (Map.Entry<Path, List<Path>> entry : dirFileLists.entrySet())
            {
                for (Path filePath : entry.getValue())
                {
                    fileInfoMap.put(filePath, new FortranFileInfo(filePath));
                }
            }
        } else
        {
            List<Path> filePaths = new ArrayList<Path>();
            for (Map.Entry<Path, List<Path>> entry : dirFileLists.entrySet())
            {
                for (Path fPath : entry.getValue())
                {
                    filePaths.add(fPath);
                }
            }
            final int n = filePaths.size();
            final FortranFileInfo[] fileInfo = new FortranFileInfo[n];
            final int maxNumThreads = Runtime.getRuntime().availableProcessors();
            ExecutorService es = Executors.newFixedThreadPool(maxNumThreads);
            Future[] taskFuture = new Future[n];
            try
            {
                for (int i = 0; i < n; ++i)
                {
                    final Path fPath = filePaths.get(i);
                    final int curI = i;
                    taskFuture[i] = es.submit(new Callable<Void>()
                    {
                        public Void call() throws Exception
                        {
                            fileInfo[curI] = new FortranFileInfo(fPath);
                            return null;
                        }
                    });
                }
                for (int i = 0; i < n; ++i)
                {
                    taskFuture[i].get();
                }
            } finally
            {
                es.shutdownNow();
            }
            for (int i = 0; i < n; ++i)
            {
                Path fPath = filePaths.get(i);
                fileInfoMap.put(fPath, fileInfo[i]);
            }
        }
        return Collections.unmodifiableMap(fileInfoMap);
    }

    public static class CyclicDependencyDetector
    {
        Map<Path, Set<Path>> fileDeps;
        HashSet<Path> visited;
        ArrayList<Path> stack;
        HashSet<Path> checked;

        CyclicDependencyDetector(Map<Path, Set<Path>> fileDeps)
        {
            this.fileDeps = fileDeps;
        }

        public static void run(Map<Path, Set<Path>> fileDeps) throws FortranSemanticException
        {
            CyclicDependencyDetector d = new CyclicDependencyDetector(fileDeps);
            d.run();
        }

        public void run() throws FortranSemanticException
        {
            if (fileDeps.isEmpty())
            {
                return;
            }
            checked = new HashSet<Path>();
            HashSet<Path> unreferencedFiles = new HashSet<Path>();
            Set<Path> files = fileDeps.keySet();
            for (Path file : files)
            {
                unreferencedFiles.add(file);
            }
            for (Map.Entry<Path, Set<Path>> entry : fileDeps.entrySet())
            {
                Path file = entry.getKey();
                for (Path refFile : entry.getValue())
                {
                    unreferencedFiles.remove(refFile);
                }
            }
            if (unreferencedFiles.isEmpty())
            {// This indicates that all files are in cyclic dependency
             // Call DFS on first file to generate an error
                startDFS(files.iterator().next());
            }
            for (Path root : unreferencedFiles)
            {
                startDFS(root);
            }
            if (checked.size() < files.size())
            {// Some file could not be reached from unreferenced files,
             // this indicates it is in cyclic dependency
                for (Path file : files)
                {
                    if (!checked.contains(file))
                    {// Call DFS on unchecked file to generate an error
                        startDFS(file);
                    }
                }
            }
        }

        void startDFS(Path root) throws FortranSemanticException
        {
            visited = new HashSet<Path>();
            stack = new ArrayList<Path>();
            dfs(root);
        }

        void dfs(Path file) throws FortranSemanticException
        {
            checked.add(file);
            if (visited.add(file))
            {
                stack.add(file);
                for (Path depFile : fileDeps.get(file))
                {
                    dfs(depFile);
                }
                stack.remove(stack.size() - 1);
            } else
            {
                stack.add(file);
                ArrayList<String> strStack = new ArrayList<String>(stack.size());
                stack.forEach((s) -> {
                    strStack.add(s.toString());
                });
                String errMsg = String.format("Circle dependency between files: %s", String.join(" ->\n", strStack));
                throw new FortranSemanticException(errMsg);
            }
        }
    };

    public static Map<Path, Set<Path>> getFilesDependencies(Map<Path, FortranFileInfo> fileInfo)
            throws FortranSemanticException
    {
        HashMap<String, Path> moduleSrcFile = new HashMap<String, Path>();
        for (Map.Entry<Path, FortranFileInfo> entry : fileInfo.entrySet())
        {
            Path file = entry.getKey();
            FortranFileInfo fInfo = entry.getValue();
            for (String modName : fInfo.definedModules())
            {
                Path defFile = moduleSrcFile.putIfAbsent(modName, file);
                if (defFile != null)
                {
                    String errMsg = String.format("Module \"%s\" defined in file \"%s\"  is already defined in \"%s\"",
                            modName, file.toString(), defFile.toString());
                    throw new FortranSemanticException(errMsg);
                }

            }
        }
        Map<Path, Set<Path>> fileDeps = new LinkedHashMap<Path, Set<Path>>();
        for (Map.Entry<Path, FortranFileInfo> entry : fileInfo.entrySet())
        {
            Path file = entry.getKey();
            FortranFileInfo fInfo = entry.getValue();
            LinkedHashSet<Path> fDeps = (LinkedHashSet<Path>) fileDeps.get(file);
            if (fDeps == null)
            {
                fDeps = new LinkedHashSet<Path>();
                fileDeps.put(file, fDeps);
            }
            for (String modName : fInfo.usedModules())
            {
                Path depFile = moduleSrcFile.get(modName);
                if (depFile != null)
                {
                    if (!file.equals(depFile))
                    {
                        fDeps.add(depFile);
                    }
                } else
                {
                    String errMsg = String.format(
                            "Module \"%s\" used in file \"%s\"  is not defined in any file under given search path",
                            modName, file.toString());
                    throw new FortranSemanticException(errMsg);
                }
            }
        }
        CyclicDependencyDetector.run(fileDeps);
        return Collections.unmodifiableMap(fileDeps);
    }

    void initialise(List<String> includeDirs, boolean useMultithreading) throws IOException, FortranException, Exception
    {
        List<Path> dirs = createDirList(includeDirs);
        Map<Path, List<Path>> dirFileLists = createDirFileLists(dirs);
        _fileInformation = scanFortranFiles(dirFileLists);
        _fileDependencies = getFilesDependencies(_fileInformation);
    }

    public BuildInfo(List<String> includeDirs, boolean useMultithreading)
            throws IOException, FortranException, Exception
    {
        initialise(includeDirs, useMultithreading);
    }

    public BuildInfo(List<String> includeDirs) throws IOException, FortranException, Exception
    {
        initialise(includeDirs, true);
    }
};