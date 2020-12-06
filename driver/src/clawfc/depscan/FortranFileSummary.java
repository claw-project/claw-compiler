/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class FortranFileSummary
{
    clawfc.depscan.serial.FortranFileSummary _data;
    List<FortranModuleInfo> _modules;
    FortranModuleInfo _program;
    Path _filePath;
    List<Path> _includes;

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

    public List<Path> getIncludes()
    {
        return _includes;
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
        _includes = data.getInclude().stream().map(Paths::get).collect(Collectors.toList());
    }

    public FortranFileSummary(List<FortranModuleInfo> modules, FortranModuleInfo program)
    {
        initialize(modules, program, null, Arrays.asList());
    }

    public FortranFileSummary(List<FortranModuleInfo> modules, FortranModuleInfo program, Path path)
    {
        initialize(modules, program, path, Arrays.asList());
    }

    public FortranFileSummary(List<FortranModuleInfo> modules, FortranModuleInfo program, List<Path> includes)
    {
        initialize(modules, program, null, includes);
    }

    public FortranFileSummary(List<FortranModuleInfo> modules, FortranModuleInfo program, Path path,
            List<Path> includes)
    {
        initialize(modules, program, path, includes);
    }

    void initialize(List<FortranModuleInfo> modules, FortranModuleInfo program, Path path, List<Path> includes)
    {
        _data = new clawfc.depscan.serial.FortranFileSummary();
        _modules = modules;
        _program = program;
        _includes = includes;
        if (program != null)
        {
            _data.setProgram(_program._data);
        }
        modules.forEach(info -> {
            _data.getModule().add(info.data());
        });
        includes.forEach(incPath -> {
            _data.getInclude().add(incPath.toString());
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
        if (!getIncludes().equals(other.getIncludes()))
        {
            return false;
        }
        return true;
    }
}
