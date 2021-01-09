/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import static clawfc.Utils.copy;
import static clawfc.depscan.FortranStatementPosition.createPosition;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import clawfc.depscan.FortranDepParser.StatementInfo;
import clawfc.utils.AsciiArrayIOStream;
import clawfc.utils.ByteArrayIOStream;

public class FortranDepScanner
{
    FortranCommentsFilter commentsFilter;
    FortranLineBreaksFilter lineBreaksFilter;
    FortranDepParser parser;
    FortranCLAWScanner clawScanner;
    FortranIncludeChecker incChecker;
    FortranIncludesResolver incResolver;

    public static class ContainsIncludesException extends Exception
    {
    }

    public FortranDepScanner() throws Exception
    {
        commentsFilter = new FortranCommentsFilter();
        lineBreaksFilter = new FortranLineBreaksFilter();
        incChecker = new FortranIncludeChecker();
        parser = new FortranDepParser();
        clawScanner = new FortranCLAWScanner();
        incResolver = new FortranIncludesResolver();
    }

    public FortranFileBasicSummary basicScan(InputStream input, OutputStream outWithoutComments,
            OutputStream outWithoutLineBreaks)
            throws FortranException, IOException, Exception, ContainsIncludesException
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
            if (incChecker.run(inputWithoutLineBreaks.getAsInputStreamUnsafe()))
            {
                throw new ContainsIncludesException();
            }
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

    public FortranFileBasicSummary basicScan(InputStream input)
            throws FortranException, IOException, Exception, ContainsIncludesException
    {
        return basicScan(input, null, null);
    }

    public FortranFileProgramUnitInfo scan(InputStream input) throws FortranException, IOException, Exception
    {
        return scan(input, null, null, null);
    }

    public FortranFileProgramUnitInfo scan(InputStream input, Path inputFilePath,
            OutputStream inputWithResolvedIncludes, List<Path> incSearchPath)
            throws FortranException, IOException, Exception
    {
        AsciiArrayIOStream inStrm = new AsciiArrayIOStream();
        clawfc.Utils.copy(input, inStrm);
        AsciiArrayIOStream.LinesInfo linesInfo = inStrm.getLinesInfo();
        FortranFileBasicSummary basicRes = null;
        List<Path> incPaths = Collections.emptyList();
        Set<String> modUsesClaw;
        try
        {
            basicRes = basicScan(inStrm.getAsInputStreamUnsafe(), null, null);
            modUsesClaw = detectClaw(inStrm, basicRes);
        } catch (FortranException e)
        {
            if (e.getCharIdxInFile() != null)
            {
                e.setLineIndex(linesInfo.getLineIdx(e.getCharIdxInFile()));
            }
            throw e;
        } catch (ContainsIncludesException e)
        {
            AsciiArrayIOStream inputWithResolvedIncludesBuf = new AsciiArrayIOStream();
            if (incSearchPath == null)
            {
                incSearchPath = Collections.emptyList();
            }
            Set<Path> incPathsSet = incResolver.run(inputFilePath, inStrm, inputWithResolvedIncludesBuf, incSearchPath);
            linesInfo = inputWithResolvedIncludesBuf.getLinesInfo();
            if (inputWithResolvedIncludes != null)
            {
                copy(inputWithResolvedIncludesBuf.getAsInputStreamUnsafe(), inputWithResolvedIncludes);
            }
            incPaths = Collections.unmodifiableList(new ArrayList<Path>(incPathsSet));
            basicRes = basicScan(inputWithResolvedIncludesBuf.getAsInputStreamUnsafe(), null, null);
            modUsesClaw = detectClaw(inputWithResolvedIncludesBuf, basicRes);
        }
        FortranFileProgramUnitInfo res = getSummary(basicRes, modUsesClaw, linesInfo, incPaths);
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
            final List<StatementInfo> statements = parser.parse(inputStrm.getAsInputStreamUnsafe());
            final FortranFileBasicSummary summary = FortranDepParser.getSummary(statements);
            return summary;
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

    FortranFileProgramUnitInfo getSummary(FortranFileBasicSummary basicSummary, Set<String> modUsesClaw,
            AsciiArrayIOStream.LinesInfo linesInfo, List<Path> incPaths)
    {
        List<FortranProgramUnitInfo> units = basicSummary.units.stream()
                .map((x) -> createInfo(x, modUsesClaw, linesInfo)).collect(Collectors.toList());
        return new FortranFileProgramUnitInfo(units, incPaths);
    }

    FortranProgramUnitInfo createInfo(FortranProgramUnitBasicInfo basicInfo, final Set<String> modUsesClaw,
            final AsciiArrayIOStream.LinesInfo linesInfo)
    {
        List<clawfc.depscan.FortranStatementPosition> useModules = basicInfo.getUseModules().stream()
                .map((x) -> createPosition(x, linesInfo)).collect(Collectors.toList());
        clawfc.depscan.FortranStatementPosition modPos = createPosition(basicInfo.getPosition(), linesInfo);
        boolean usesClaw = modUsesClaw.contains(basicInfo.getPosition().getName());
        return new FortranProgramUnitInfo(basicInfo.getType(), modPos, useModules, usesClaw);
    }

    Set<String> detectClaw(ByteArrayIOStream inStrm, FortranFileBasicSummary summary)
            throws IOException, FortranSyntaxException
    {
        FortranCLAWDetector detector = new FortranCLAWDetector();
        Set<String> res = new HashSet<String>();
        for (FortranProgramUnitBasicInfo info : summary.units)
        {
            if (usesClaw(detector, inStrm, info))
            {
                res.add(info.getName());
            }
        }
        return Collections.unmodifiableSet(res);
    }

    boolean usesClaw(FortranCLAWDetector detector, ByteArrayIOStream inStrm, FortranProgramUnitBasicInfo info)
            throws FortranSyntaxException, IOException
    {
        return detector
                .run(inStrm.getAsInputStreamUnsafe(info.getPosition().getStartCharIdx(), info.getPosition().length()));
    }

    static FortranProgramUnitBasicInfo restoreOriginalCharPositions(final FilteredContentSequence fSeq,
            FortranProgramUnitBasicInfo info)
    {
        if (info == null)
        {
            return null;
        }
        FortranStatementBasicPosition newMod = fSeq.getOriginal(info.getPosition());
        List<FortranStatementBasicPosition> newUseModules = info.getUseModules().stream()
                .map((x) -> fSeq.getOriginal(x)).collect(Collectors.toList());
        return new FortranProgramUnitBasicInfo(info.getType(), newMod, Collections.unmodifiableList(newUseModules));
    }

    static FortranFileBasicSummary restoreOriginalCharPositions(FilteredContentSequence fSeq,
            FortranFileBasicSummary summary)
    {
        List<FortranProgramUnitBasicInfo> newUnits = summary.units.stream()
                .map((x) -> restoreOriginalCharPositions(fSeq, x)).collect(Collectors.toList());
        return new FortranFileBasicSummary(Collections.unmodifiableList(newUnits));
    }
}
