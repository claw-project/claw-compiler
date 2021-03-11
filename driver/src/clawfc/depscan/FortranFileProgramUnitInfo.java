/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.depscan;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import clawfc.utils.FileInfo;
import clawfc.utils.FileInfoImpl;

public class FortranFileProgramUnitInfo
{
    clawfc.depscan.serial.FortranFileProgramUnitInfo data;
    List<FortranProgramUnitInfo> units;
    Path srcFilePath;
    Path ppSrcFilePath;
    List<Path> includes;

    public clawfc.depscan.serial.FortranFileProgramUnitInfo getData()
    {
        return data;
    }

    public List<FortranProgramUnitInfo> getUnits()
    {
        return units;
    }

    public List<Path> getIncludes()
    {
        return includes;
    }

    public List<Path> setIncludes(List<Path> includes)
    {
        List<Path> oldIncludes = this.includes;
        this.includes = includes;
        data.getInclude().clear();
        for (Path incPath : includes)
        {
            data.getInclude().add(incPath.toString());
        }
        return oldIncludes;
    }

    public Path getSrcFilePath()
    {
        return srcFilePath;
    }

    public void setSrcFilePath(Path path)
    {
        srcFilePath = path;
        String strPath = null;
        if (path != null)
        {
            strPath = path.toString();
        }
        data.setSrcPath(strPath);
    }

    public Path getPPSrcFilePath()
    {
        return ppSrcFilePath;
    }

    public void setPPSrcFilePath(Path path)
    {
        ppSrcFilePath = path;
        String strPath = null;
        if (path != null)
        {
            strPath = path.toString();
        }
        data.setPpSrcPath(strPath);
    }

    public FortranFileProgramUnitInfo(clawfc.depscan.serial.FortranFileProgramUnitInfo data)
    {
        this.data = data;
        units = data.getUnit().stream().map(FortranProgramUnitInfo::new).collect(Collectors.toList());
        if (data.getSrcPath() != null)
        {
            srcFilePath = Paths.get(data.getSrcPath());
        } else
        {
            srcFilePath = null;
        }
        if (data.getPpSrcPath() != null)
        {
            ppSrcFilePath = Paths.get(data.getPpSrcPath());
        } else
        {
            ppSrcFilePath = null;
        }
        includes = data.getInclude().stream().map(Paths::get).collect(Collectors.toList());
    }

    public FortranFileProgramUnitInfo(List<FortranProgramUnitInfo> units)
    {
        initialize(units, null, null, Collections.emptyList());
    }

    public FortranFileProgramUnitInfo(List<FortranProgramUnitInfo> units, Path srcPath)
    {
        initialize(units, srcPath, null, Collections.emptyList());
    }

    public FortranFileProgramUnitInfo(List<FortranProgramUnitInfo> units, List<Path> includes)
    {
        initialize(units, null, null, includes);
    }

    public FortranFileProgramUnitInfo(List<FortranProgramUnitInfo> units, Path srcPath, List<Path> includes)
    {
        initialize(units, srcPath, null, includes);
    }

    public FortranFileProgramUnitInfo(List<FortranProgramUnitInfo> units, Path srcPath, Path ppSrcPath,
            List<Path> includes)
    {
        initialize(units, srcPath, ppSrcPath, includes);
    }

    void initialize(List<FortranProgramUnitInfo> units, Path srcPath, Path ppSrcPath, List<Path> includes)
    {
        data = new clawfc.depscan.serial.FortranFileProgramUnitInfo();
        this.units = units;
        this.includes = includes;
        units.forEach(info -> {
            data.getUnit().add(info.data());
        });
        includes.forEach(incPath -> {
            data.getInclude().add(incPath.toString());
        });
        setSrcFilePath(srcPath);
        setPPSrcFilePath(ppSrcPath);
    }

    public List<FileInfo> getIncludeFilesInfo() throws Exception
    {
        List<Path> includes = getIncludes();
        List<FileInfo> incFilesInfo = new ArrayList<FileInfo>();
        for (Path includeFilepath : includes)
        {
            incFilesInfo.add(new FileInfoImpl(includeFilepath));
        }
        return incFilesInfo;
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
        FortranFileProgramUnitInfo other = (FortranFileProgramUnitInfo) obj;
        if (!getUnits().equals(other.getUnits()))
        {
            return false;
        }
        if (getSrcFilePath() == null)
        {
            if (other.getSrcFilePath() != null)
            {
                return false;
            }
        } else
        {
            if (!getSrcFilePath().equals(other.getSrcFilePath()))
            {
                return false;
            }
        }
        if (getPPSrcFilePath() == null)
        {
            if (other.getPPSrcFilePath() != null)
            {
                return false;
            }
        } else
        {
            if (!getPPSrcFilePath().equals(other.getPPSrcFilePath()))
            {
                return false;
            }
        }
        if (!getIncludes().equals(other.getIncludes()))
        {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode()
    {// This is to silence the warning
        return super.hashCode();
    }

    public List<String> getModuleNames()
    {
        List<String> names = getUnits().stream().map((FortranProgramUnitInfo modBInfo) -> modBInfo.getName())
                .collect(Collectors.toList());
        ;
        return Collections.unmodifiableList(names);
    }
}
