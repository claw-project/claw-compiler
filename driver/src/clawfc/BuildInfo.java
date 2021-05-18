/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.NotDirectoryException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * <code>BuildInfo</code> is a collection of static routines which search
 * filesystem for files relevant to the build.
 *
 */
public class BuildInfo
{
    public static final String[] BUILDINFO_FILE_EXTENSIONS = new String[] { "fif", "bif" };
    public static final String XMOD_EXTENSION = "xmod";
    public static final Pattern FORTRAN_FILE_SEARCH_PATTERN = generateExtensionPattern(Utils.FORTRAN_FILE_EXTENSIONS);
    public static final Pattern BUILDINFO_FILE_SEARCH_PATTERN = generateExtensionPattern(BUILDINFO_FILE_EXTENSIONS);
    public static final Pattern XMOD_FILE_SEARCH_PATTERN = generateExtensionPattern(new String[] { XMOD_EXTENSION });

    static Pattern generateExtensionPattern(String[] extensions)
    {
        return Pattern.compile(String.format(".+\\.(%s)", String.join("|", extensions)));
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
            if (!Utils.dirExists(path))
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
        return createDirFileList(dir, FORTRAN_FILE_SEARCH_PATTERN);
    }

    public static List<Path> createDirFileList(Path dir, Pattern pattern) throws IOException
    {
        ArrayList<Path> files = new ArrayList<Path>();
        try (Stream<Path> filesStrm = Files.list(dir))
        {
            filesStrm.filter(s -> pattern.matcher(s.toString()).matches()).forEach(files::add);
        }
        Collections.sort(files);
        return Collections.unmodifiableList(files);
    }

    public static Map<Path, List<Path>> createDirFileLists(Collection<Path> dirs) throws IOException
    {
        return createDirFileLists(dirs, FORTRAN_FILE_SEARCH_PATTERN);
    }

    public static Map<Path, List<Path>> createBuildinfoDirFileLists(Collection<Path> dirs) throws IOException
    {
        return createDirFileLists(dirs, BUILDINFO_FILE_SEARCH_PATTERN);
    }

    public static Map<Path, List<Path>> createModuleDirFileLists(Collection<Path> dirs) throws IOException
    {
        return createDirFileLists(dirs, XMOD_FILE_SEARCH_PATTERN);
    }

    public static Map<Path, List<Path>> createDirFileLists(Collection<Path> dirs, Pattern pattern) throws IOException
    {
        Map<Path, List<Path>> res = new LinkedHashMap<Path, List<Path>>();
        for (Path dir : dirs)
        {
            res.put(dir, createDirFileList(dir, pattern));
        }
        return Collections.unmodifiableMap(res);
    }

};
