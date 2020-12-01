/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;

public class FortranFileSummary
{
    clawfc.depscan.serial.FortranFileSummary _data;
    List<FortranModuleInfo> _modules;
    FortranModuleInfo _program;
    Path _filePath;

    public clawfc.depscan.serial.FortranFileSummary data()
    {
        return _data;
    }

    public FortranModuleInfo getProgram()
    {
        return _program;
    }

    public List<FortranModuleInfo> getModules()
    {
        return _modules;
    }

    public Path getFilePath()
    {
        return _filePath;
    }

    public void setFilePath(Path path)
    {
        _filePath = path;
        String strPath = null;
        if (path != null)
        {
            strPath = path.toString();
        }
        _data.setFilePath(strPath);
    }

    public FortranFileSummary(clawfc.depscan.serial.FortranFileSummary data)
    {
        _data = data;
        _modules = data.getModule().stream().map(FortranModuleInfo::new).collect(Collectors.toList());
        if (data.getProgram() != null)
        {
            _program = new FortranModuleInfo(data.getProgram());
        } else
        {
            _program = null;
        }
        if (data.getFilePath() != null)
        {
            _filePath = Paths.get(data.getFilePath());
        } else
        {
            _filePath = null;
        }
    }

    /*
     * public FortranFileSummary(FortranFileBasicSummary in,
     * AsciiArrayIOStream.LinesInfo linesInfo, FortranFileCLAWLinesInfo
     * clawLinesInfo) { _data = new clawfc.depscan.serial.FortranFileSummary();
     * HasCLAW hasCLAW = new HasCLAW(linesInfo.numLines(), clawLinesInfo); _modules
     * = new ArrayList<FortranModuleInfo>(in.modules.size()); for
     * (FortranModuleBasicInfo bInfo : in.modules) { int startPos =
     * linesInfo.lineStartByteIdx((int) bInfo.getStartLineIdx()); int endPos =
     * linesInfo.lineEndByteIdx((int) bInfo.getEndLineIdx()); boolean usesCLAW =
     * hasCLAW.run((int) bInfo.getStartLineIdx(), (int) bInfo.getEndLineIdx());
     * FortranModuleInfo info = new FortranModuleInfo(bInfo, startPos, endPos,
     * usesCLAW); _modules.add(info); _data.getModule().add(info.data()); } if
     * (in.program == null) { _program = null; } else { FortranModuleBasicInfo info
     * = in.program; int startPos = linesInfo.lineStartByteIdx((int)
     * info.getStartLineIdx()); int endPos = linesInfo.lineEndByteIdx((int)
     * info.getEndLineIdx()); boolean usesCLAW = hasCLAW.run((int)
     * info.getStartLineIdx(), (int) info.getEndLineIdx()); _program = new
     * FortranModuleInfo(info, startPos, endPos, usesCLAW);
     * _data.setProgram(_program._data); } _filePath = null; }
     */

    public FortranFileSummary(List<FortranModuleInfo> modules, FortranModuleInfo program)
    {
        initialize(modules, program, null);
    }

    public FortranFileSummary(List<FortranModuleInfo> modules, FortranModuleInfo program, Path path)
    {
        initialize(modules, program, path);
    }

    void initialize(List<FortranModuleInfo> modules, FortranModuleInfo program, Path path)
    {
        _data = new clawfc.depscan.serial.FortranFileSummary();
        _modules = modules;
        _program = program;
        if (program != null)
        {
            _data.setProgram(_program._data);
        }
        modules.forEach(info -> {
            _data.getModule().add(info.data());
        });
        setFilePath(path);
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
        FortranFileSummary other = (FortranFileSummary) obj;
        if (!getModules().equals(other.getModules()))
        {
            return false;
        }
        if (getProgram() == null)
        {
            if (other.getProgram() != null)
            {
                return false;
            }
        } else
        {
            if (!getProgram().equals(other.getProgram()))
            {
                return false;
            }
        }
        if (getFilePath() == null)
        {
            if (other.getFilePath() != null)
            {
                return false;
            }
        } else
        {
            if (!getFilePath().equals(other.getFilePath()))
            {
                return false;
            }
        }
        return true;
    }
}
