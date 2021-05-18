/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;

import clawfc.depscan.FortranProgramUnitInfo;
import clawfc.depscan.serial.FortranProgramUnitType;
import clawfc.utils.AsciiArrayIOStream;

/**
 * <code>ProgramUnitInfo</code> is an interface for accessing information about
 * specific Fortran program unit (module or global function).
 *
 */
public interface ProgramUnitInfo
{
    public String getName();

    public boolean isModule();

    public FortranProgramUnitType getType();

    public boolean isInput();

    // public boolean XModIsUpToDate(Map<String, ModuleInfo> availModsByName,
    // FileTime xmodTS);

    public boolean usesCLAW();

    public boolean hasSource();

    public Collection<String> getUsedModules();

    public FortranProgramUnitInfo getSrcInfo();

    // public clawfc.depscan.FortranFileProgramUnitInfo getSrcSummary();

    public Path getSrcPath();

    public Path getPPSrcPath();

    FortranFileProgramUnitInfoData getSrcFileBinfoData();

    public XmodData getXMod();

    public AsciiArrayIOStream getXast();

    public AsciiArrayIOStream getTransXast();

    public AsciiArrayIOStream getTransReport();

    public AsciiArrayIOStream getTransSrc();

    // public List<FileInfo> getIncludeFilesInfo();

    /**
     * @param preserveOffset Add new lines and spaces to preserve module position in
     *                       the file
     * @throws IOException
     */
    public AsciiArrayIOStream getPreprocessedSrc(boolean preserveOffset) throws IOException;
}
