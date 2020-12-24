/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc;

import static clawfc.Utils.ASCII_NEWLINE_VALUE;
import static clawfc.Utils.ASCII_SPACE_VALUE;
import static clawfc.Utils.copy;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.Collection;

import clawfc.depscan.FortranModuleInfo;
import clawfc.utils.AsciiArrayIOStream;

public class ModuleData implements ModuleInfo
{
    public static enum ModuleType {
        Module, Program
    };

    public static enum ModuleDesignation {
        Input, Include
    };

    final String name;
    final ModuleType type;
    final ModuleDesignation designation;
    final FortranModuleInfo info;
    final FortranFileBuildInfoData srcFileBinfoData;
    final PreprocessedFortranSourceData srcData;
    XmodData xmodData;

    public ModuleData(ModuleType type, ModuleDesignation designation, FortranModuleInfo info,
            FortranFileBuildInfoData srcFileBinfoData, PreprocessedFortranSourceData srcData)
    {
        name = info.getName();
        this.type = type;
        this.designation = designation;
        this.info = info;
        this.srcFileBinfoData = srcFileBinfoData;
        this.srcData = srcData;
        this.xmodData = null;
    }

    public ModuleData(String name, ModuleDesignation designation, XmodData xmodData)
    {
        this.name = name;
        this.type = ModuleType.Module;
        this.designation = designation;
        this.info = null;
        this.srcFileBinfoData = null;
        this.srcData = null;
        this.xmodData = xmodData;
    }

    @Override
    public String getName()
    {
        return name;
    }

    @Override
    public boolean isProgram()
    {
        return type == ModuleType.Program;
    }

    @Override
    public boolean isModule()
    {
        return type == ModuleType.Module;
    }

    @Override
    public boolean isInput()
    {
        return designation == ModuleDesignation.Input;
    }

    @Override
    public boolean usesCLAW()
    {
        return info.getUsesClaw();
    }

    @Override
    public boolean hasSource()
    {
        return srcData != null;
    }

    @Override
    public Collection<String> getUsedModules()
    {
        if (!hasSource())
        {
            return (Collection<String>) null;
        } else
        {
            return info.getUsedModuleNames();
        }
    }

    @Override
    public clawfc.depscan.FortranModuleInfo getModuleSrcInfo()
    {
        return info;
    }

    @Override
    public XmodData getXMod()
    {
        return xmodData;
    }

    public void setXMod(XmodData xmodData)
    {
        this.xmodData = xmodData;
    }

    @Override
    public FortranFileBuildInfoData getSrcFileBinfoData()
    {
        return this.srcFileBinfoData;
    }

    @Override
    public AsciiArrayIOStream getPreprocessedSrc(boolean preserveOffset) throws IOException
    {
        final AsciiArrayIOStream src = srcData.getPPSource();
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

    @Override
    public Path getSrcPath()
    {
        return srcFileBinfoData.getInfo().getSrcFilePath();
    }
}
