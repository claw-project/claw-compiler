/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.stream.Collectors;

import clawfc.utils.FileInfo;
import clawfc.utils.FileInfoImpl;

public class FortranFileBuildInfo
{
    clawfc.depscan.serial.FortranFileBuildInfo data;
    List<FortranModuleInfo> modules;
    FortranModuleInfo program;
    Path srcFilePath;
    Path ppSrcFilePath;
    List<Path> includes;

    public clawfc.depscan.serial.FortranFileBuildInfo getData()
    {
        return data;
    }

    public FortranModuleInfo getProgram()
    {
        return program;
    }

    public List<FortranModuleInfo> getModules()
    {
        return modules;
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

    public FortranFileBuildInfo(clawfc.depscan.serial.FortranFileBuildInfo data)
    {
        this.data = data;
        modules = data.getModule().stream().map(FortranModuleInfo::new).collect(Collectors.toList());
        if (data.getProgram() != null)
        {
            program = new FortranModuleInfo(data.getProgram());
        } else
        {
            program = null;
        }
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

    public FortranFileBuildInfo(List<FortranModuleInfo> modules, FortranModuleInfo program)
    {
        initialize(modules, program, null, null, Collections.emptyList());
    }

    public FortranFileBuildInfo(List<FortranModuleInfo> modules, FortranModuleInfo program, Path srcPath)
    {
        initialize(modules, program, srcPath, null, Collections.emptyList());
    }

    public FortranFileBuildInfo(List<FortranModuleInfo> modules, FortranModuleInfo program, List<Path> includes)
    {
        initialize(modules, program, null, null, includes);
    }

    public FortranFileBuildInfo(List<FortranModuleInfo> modules, FortranModuleInfo program, Path srcPath,
            List<Path> includes)
    {
        initialize(modules, program, srcPath, null, includes);
    }

    public FortranFileBuildInfo(List<FortranModuleInfo> modules, FortranModuleInfo program, Path srcPath,
            Path ppSrcPath, List<Path> includes)
    {
        initialize(modules, program, srcPath, ppSrcPath, includes);
    }

    void initialize(List<FortranModuleInfo> modules, FortranModuleInfo program, Path srcPath, Path ppSrcPath,
            List<Path> includes)
    {
        data = new clawfc.depscan.serial.FortranFileBuildInfo();
        this.modules = modules;
        this.program = program;
        this.includes = includes;
        if (program != null)
        {
            data.setProgram(program._data);
        }
        modules.forEach(info -> {
            data.getModule().add(info.data());
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
        FortranFileBuildInfo other = (FortranFileBuildInfo) obj;
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

    public List<String> getModuleNames(boolean includeProgram)
    {
        SortedMap<Integer, String> nameByPos = new TreeMap<Integer, String>();
        for (FortranModuleInfo modBInfo : getModules())
        {
            nameByPos.put(modBInfo.getStartCharIdx(), modBInfo.getName());
        }
        if (includeProgram && (getProgram() != null))
        {
            nameByPos.put(getProgram().getStartCharIdx(), getProgram().getName());
        }
        return Collections.unmodifiableList(new ArrayList<String>(nameByPos.values()));
    }
}
