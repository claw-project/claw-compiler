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

import clawfc.depscan.FortranProgramUnitInfo;
import clawfc.depscan.serial.FortranProgramUnitType;
import clawfc.utils.AsciiArrayIOStream;

public class ProgramUnitData implements ProgramUnitInfo
{

    public static enum UnitDesignation {
        Input, Include
    };

    final String name;
    final FortranProgramUnitType type;
    final UnitDesignation designation;
    final FortranProgramUnitInfo info;
    final FortranFileProgramUnitInfoData srcFileBinfoData;
    final PreprocessedFortranSourceData srcData;
    XmodData xmodData;
    AsciiArrayIOStream xast;
    AsciiArrayIOStream transXast;
    AsciiArrayIOStream transSrc;
    AsciiArrayIOStream transReport;

    public ProgramUnitData(UnitDesignation designation, FortranProgramUnitInfo info,
            FortranFileProgramUnitInfoData srcFileBinfoData, PreprocessedFortranSourceData srcData)
    {
        type = info.getType();
        name = info.getName();
        this.designation = designation;
        this.info = info;
        this.srcFileBinfoData = srcFileBinfoData;
        this.srcData = srcData;
        this.xmodData = null;
        this.xast = null;
        this.transXast = null;
        this.transSrc = null;
        this.transReport = null;
    }

    public ProgramUnitData(String name, UnitDesignation designation, XmodData xmodData)
    {
        this.name = name;
        this.type = FortranProgramUnitType.MODULE;
        this.designation = designation;
        this.info = null;
        this.srcFileBinfoData = null;
        this.srcData = null;
        this.xmodData = xmodData;
        this.xast = null;
        this.transXast = null;
        this.transSrc = null;
        this.transReport = null;
    }

    @Override
    public String getName()
    {
        return name;
    }

    @Override
    public boolean isModule()
    {
        return type.equals(FortranProgramUnitType.MODULE);
    }

    @Override
    public boolean isInput()
    {
        return designation == UnitDesignation.Input;
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
    public FortranProgramUnitInfo getSrcInfo()
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
    public FortranFileProgramUnitInfoData getSrcFileBinfoData()
    {
        return this.srcFileBinfoData;
    }

    @Override
    public AsciiArrayIOStream getPreprocessedSrc(boolean preserveOffset) throws IOException
    {
        final AsciiArrayIOStream src = srcData.getPPSource();
        FortranProgramUnitInfo mInfo = getSrcInfo();
        final int startChrIdx = mInfo.getStartCharIdx();
        final int endChrIDx = mInfo.getEndCharIdx();
        final int count = endChrIDx - startChrIdx;
        AsciiArrayIOStream buf;
        if (preserveOffset)
        {
            final int lineOffset = mInfo.getStartLineIdx();
            final int lineStartChrOffset = startChrIdx - src.findLineStartChrIdx(mInfo.getStartCharIdx());
            final int size = lineOffset + lineStartChrOffset + count;
            buf = new AsciiArrayIOStream(size + 1);
            for (int i = 0; i < lineOffset; ++i)
            {
                buf.write(ASCII_NEWLINE_VALUE);
            }
            for (int i = 0; i < lineStartChrOffset; ++i)
            {
                buf.write(ASCII_SPACE_VALUE);
            }
        } else
        {
            buf = new AsciiArrayIOStream(count + 1);
        }
        try (InputStream srcStrm = src.getAsInputStreamUnsafe(startChrIdx, count))
        {
            copy(srcStrm, buf);
        }
        if (buf.getChr(buf.getSize() - 1) != '\n')
        {
            // Most Fortran parsers breakdown without newline at the end of the text
            buf.write(ASCII_NEWLINE_VALUE);
        }
        return buf;
    }

    @Override
    public Path getSrcPath()
    {
        return srcFileBinfoData.getInfo().getSrcFilePath();
    }

    @Override
    public Path getPPSrcPath()
    {
        return srcFileBinfoData.getInfo().getPPSrcFilePath();
    }

    @Override
    public AsciiArrayIOStream getXast()
    {
        return xast;
    }

    public void setXast(AsciiArrayIOStream xast)
    {
        this.xast = xast;
    }

    @Override
    public AsciiArrayIOStream getTransXast()
    {
        return transXast;
    }

    public void setTransXast(AsciiArrayIOStream transXast)
    {
        this.transXast = transXast;
    }

    @Override
    public AsciiArrayIOStream getTransSrc()
    {
        return transSrc;
    }

    public void setTransSrc(AsciiArrayIOStream transSrc)
    {
        this.transSrc = transSrc;
    }

    @Override
    public AsciiArrayIOStream getTransReport()
    {
        return transReport;
    }

    public void setTransReport(AsciiArrayIOStream transReport)
    {
        this.transReport = transReport;
    }

    @Override
    public FortranProgramUnitType getType()
    {
        return this.getSrcInfo().getType();
    }
}
