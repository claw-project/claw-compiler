/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;

import clawfc.utils.AsciiArrayIOStream;

public interface ModuleInfo
{
    public String getName();

    public boolean isProgram();

    public boolean isModule();

    public boolean isInput();

    // public boolean XModIsUpToDate(Map<String, ModuleInfo> availModsByName,
    // FileTime xmodTS);

    public boolean usesCLAW();

    public boolean hasSource();

    public Collection<String> getUsedModules();

    public clawfc.depscan.FortranModuleInfo getModuleSrcInfo();

    // public clawfc.depscan.FortranFileBuildInfo getSrcSummary();

    public Path getSrcPath();

    FortranFileBuildInfoData getSrcFileBinfoData();

    public XmodData getXMod();

    // public List<FileInfo> getIncludeFilesInfo();

    /**
     * @param preserveOffset Add new lines and spaces to preserve module position in
     *                       the file
     * @throws IOException
     */
    public AsciiArrayIOStream getPreprocessedSrc(boolean preserveOffset) throws IOException;
}
