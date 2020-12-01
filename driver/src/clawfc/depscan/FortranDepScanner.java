/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import clawfc.utils.AsciiArrayIOStream;
import clawfc.utils.ByteArrayIOStream;

public class FortranDepScanner
{
    FortranCommentsFilter commentsFilter;
    FortranLineBreaksFilter lineBreaksFilter;
    FortranDepParser parser;
    FortranCLAWScanner clawScanner;

    public FortranDepScanner() throws Exception
    {
        commentsFilter = new FortranCommentsFilter();
        lineBreaksFilter = new FortranLineBreaksFilter();
        parser = new FortranDepParser();
        clawScanner = new FortranCLAWScanner();
    }

    public FortranFileBasicSummary basicScan(InputStream input, OutputStream outWithoutComments,
            OutputStream outWithoutLineBreaks) throws FortranException, IOException, Exception
    {
        FilteredContentSequence fSeq = new FilteredContentSequence();
        try
        {
            AsciiArrayIOStream inputWithoutComments = new AsciiArrayIOStream();
            FilteredContentSequence fComments = runCommentsFilter(input, inputWithoutComments);
            dumpStream(inputWithoutComments, outWithoutComments);
            fSeq.add(fComments);
            AsciiArrayIOStream inputWithoutLineBreaks = new AsciiArrayIOStream(inputWithoutComments.size());
            FilteredContentSequence fLineBreaks = runLineBreaksFilter(inputWithoutComments, inputWithoutLineBreaks);
            dumpStream(inputWithoutLineBreaks, outWithoutLineBreaks);
            fSeq.add(fLineBreaks);
            FortranFileBasicSummary res = runParser(inputWithoutLineBreaks);
            res = restoreOriginalCharPositions(fSeq, res);
            return res;
        } catch (FortranException e)
        {
            if (e.getCharIdxInFile() != null)
            {
                e.setCharIdxInFile(fSeq.getOriginalChrIdx(e.getCharIdxInFile()));
            }
            throw e;
        }
    }

    public FortranFileBasicSummary basicScan(InputStream input) throws FortranException, IOException, Exception
    {
        return basicScan(input, null, null);
    }

    public FortranFileSummary scan(InputStream input) throws FortranException, IOException, Exception
    {
        AsciiArrayIOStream inStrm = new AsciiArrayIOStream();
        clawfc.Utils.copy(input, inStrm);
        AsciiArrayIOStream.LinesInfo linesInfo = inStrm.getLinesInfo();
        FortranFileBasicSummary basicRes = basicScan(inStrm.getAsInputStreamUnsafe(), null, null);
        Set<String> modUsesClaw = detectClaw(inStrm, basicRes);
        FortranFileSummary res = getSummary(basicRes, modUsesClaw, linesInfo);
        return res;
    }

    FortranSyntaxException setInFilePosition(FortranSyntaxException e, AsciiArrayIOStream strm)
    {
        if (e.getCharIdxInFile() == null && e.getLineIndex() != null)
        {
            AsciiArrayIOStream.LinesInfo linesInfo = strm.getLinesInfo();
            int charIdxInFile = linesInfo.getLineStartByteIdx(e.getLineIndex() + e.getCharIdxInLine());
            e.setCharIdxInFile(charIdxInFile);
            e.setLineIndex(null);
            e.setCharIdxInLine(null);
        }
        return e;
    }

    public FilteredContentSequence runCommentsFilter(InputStream input, AsciiArrayIOStream inputWithoutComments)
            throws IOException, FortranSyntaxException
    {
        FilteredContentSequence fComments = null;
        try
        {
            fComments = commentsFilter.run(input, inputWithoutComments);
        } catch (FortranSyntaxException e)
        {
            AsciiArrayIOStream inputStrm = new AsciiArrayIOStream();
            clawfc.Utils.copy(input, inputStrm);
            throw setInFilePosition(e, inputStrm);
        }
        return fComments;
    }

    public FilteredContentSequence runLineBreaksFilter(AsciiArrayIOStream inputStrm,
            AsciiArrayIOStream inputWithoutLinebreaks) throws IOException, FortranSyntaxException
    {
        FilteredContentSequence fLineBreaks = null;
        try
        {
            fLineBreaks = lineBreaksFilter.run(inputStrm.getAsInputStreamUnsafe(), inputWithoutLinebreaks, false, null);
        } catch (FortranSyntaxException e)
        {
            throw setInFilePosition(e, inputStrm);
        }
        return fLineBreaks;
    }

    public FortranFileBasicSummary runParser(AsciiArrayIOStream inputStrm) throws FortranException, Exception
    {
        try
        {
            return parser.parse(inputStrm.getAsInputStreamUnsafe());
        } catch (FortranSyntaxException e)
        {
            throw setInFilePosition(e, inputStrm);
        }
    }

    void dumpStream(AsciiArrayIOStream inStrm, OutputStream outStrm) throws IOException
    {
        if (outStrm != null)
        {
            clawfc.Utils.copy(inStrm.getAsInputStreamUnsafe(), outStrm);
            inStrm.reset();
        }
    }

    FortranFileSummary getSummary(FortranFileBasicSummary basicSummary, Set<String> modUsesClaw,
            AsciiArrayIOStream.LinesInfo linesInfo)
    {
        List<FortranModuleInfo> modules = basicSummary.modules.stream()
                .map((x) -> createInfo(x, modUsesClaw, linesInfo)).collect(Collectors.toList());
        FortranModuleInfo program = null;
        if (basicSummary.program != null)
        {
            program = createInfo(basicSummary.program, modUsesClaw, linesInfo);
        }
        return new FortranFileSummary(modules, program);
    }

    FortranModuleInfo createInfo(FortranModuleBasicInfo basicInfo, final Set<String> modUsesClaw,
            final AsciiArrayIOStream.LinesInfo linesInfo)
    {
        List<clawfc.depscan.FortranStatementPosition> useModules = basicInfo.getUseModules().stream()
                .map((x) -> createPos(x, linesInfo)).collect(Collectors.toList());
        clawfc.depscan.FortranStatementPosition modPos = createPos(basicInfo.getModule(), linesInfo);
        boolean usesClaw = modUsesClaw.contains(basicInfo.getModule().getName());
        return new FortranModuleInfo(modPos, useModules, usesClaw);
    }

    clawfc.depscan.FortranStatementPosition createPos(FortranStatementBasicPosition basicPos,
            AsciiArrayIOStream.LinesInfo linesInfo)
    {
        int startLineIdx = linesInfo.getLineIdx(basicPos.getStartCharIdx());
        int endLineIdx = linesInfo.getLineIdx(basicPos.getEndCharIdx());
        ++endLineIdx;// EndCharIdx is always on the same line as the last symbol
        return new clawfc.depscan.FortranStatementPosition(basicPos.getName(), basicPos.getStartCharIdx(),
                basicPos.getEndCharIdx(), startLineIdx, endLineIdx);
    }

    Set<String> detectClaw(ByteArrayIOStream inStrm, FortranFileBasicSummary summary)
            throws IOException, FortranSyntaxException
    {
        FortranCLAWDetector detector = new FortranCLAWDetector();
        Set<String> res = new HashSet<String>();
        for (FortranModuleBasicInfo info : summary.modules)
        {
            if (usesClaw(detector, inStrm, info))
            {
                res.add(info.getName());
            }
        }
        if (summary.program != null && usesClaw(detector, inStrm, summary.program))
        {
            res.add(summary.program.getName());
        }
        return Collections.unmodifiableSet(res);
    }

    boolean usesClaw(FortranCLAWDetector detector, ByteArrayIOStream inStrm, FortranModuleBasicInfo info)
            throws FortranSyntaxException, IOException
    {
        return detector
                .run(inStrm.getAsInputStreamUnsafe(info.getModule().getStartCharIdx(), info.getModule().length()));
    }

    static FortranStatementBasicPosition restoreOriginalCharPositions(FilteredContentSequence fSeq,
            FortranStatementBasicPosition info)
    {
        int newStartChrIdx = fSeq.getOriginalChrIdx(info.getStartCharIdx());
        int newEndChrIdx = fSeq.getOriginalChrIdx(info.getEndCharIdx() - 1) + 1;// Index is not inclusive
        return new FortranStatementBasicPosition(info.getName(), newStartChrIdx, newEndChrIdx);
    }

    static FortranModuleBasicInfo restoreOriginalCharPositions(final FilteredContentSequence fSeq,
            FortranModuleBasicInfo info)
    {
        if (info == null)
        {
            return null;
        }
        FortranStatementBasicPosition newMod = restoreOriginalCharPositions(fSeq, info.getModule());
        List<FortranStatementBasicPosition> newUseModules = info.getUseModules().stream()
                .map((x) -> restoreOriginalCharPositions(fSeq, x)).collect(Collectors.toList());
        return new FortranModuleBasicInfo(newMod, Collections.unmodifiableList(newUseModules));
    }

    static FortranFileBasicSummary restoreOriginalCharPositions(FilteredContentSequence fSeq,
            FortranFileBasicSummary summary)
    {
        List<FortranModuleBasicInfo> newModules = summary.modules.stream()
                .map((x) -> restoreOriginalCharPositions(fSeq, x)).collect(Collectors.toList());
        FortranModuleBasicInfo newProgram = restoreOriginalCharPositions(fSeq, summary.program);
        return new FortranFileBasicSummary(Collections.unmodifiableList(newModules), newProgram);
    }
}
