/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import static clawfc.Utils.copy;
import static clawfc.utils.AsciiArrayIOStream.getLinesInfo;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import clawfc.utils.AsciiArrayIOStream;
import clawfc.utils.AsciiArrayIOStream.LinesInfo;
import clawfc.utils.ByteArrayIOStream;

public class FortranIncludesResolver
{
    final FortranCommentsFilter commentsFilter;
    final FortranLineBreaksFilter lineBreaksFilter;
    final FortranIncludeStatementsFinder includesFinder;
    final byte[] copyBuffer;
    OutputStream output;
    Set<Path> includeFiles;
    IncludeStack includeStack;
    LinkedList<Path> includesSearchPath;

    public FortranIncludesResolver() throws Exception
    {
        commentsFilter = new FortranCommentsFilter();
        lineBreaksFilter = new FortranLineBreaksFilter();
        includesFinder = new FortranIncludeStatementsFinder();
        copyBuffer = new byte[1024];
        output = null;
        includeFiles = null;
        includeStack = null;
        includesSearchPath = null;
    }

    class IncludeStack
    {
        final Map<Path, FileData> incDataByPath;

        class FileData
        {
            public Path path;
            public InputStream strm;
            public FortranStatementBasicPosition stmtPos;

            public FileData(Path path, InputStream strm, FortranStatementBasicPosition stmtPos)
            {
                this.path = path;
                this.strm = strm;
                this.stmtPos = stmtPos;
            }
        }

        List<FileData> stack;

        public IncludeStack(Path path, InputStream strm)
        {
            incDataByPath = new HashMap<Path, FileData>();
            stack = new ArrayList<FileData>();
            push(path, strm, null);
        }

        public boolean contains(Path includedFilePath)
        {
            return incDataByPath.containsKey(includedFilePath);
        }

        public boolean push(Path incFilePath, InputStream incFileStrm, FortranStatementBasicPosition incStatementPos)
        {
            FileData data = new FileData(incFilePath, incFileStrm, incStatementPos);
            stack.add(data);
            return incDataByPath.put(incFilePath, data) == null;
        }

        public void pop()
        {
            FileData last = stack.get(stack.size() - 1);
            incDataByPath.remove(last.path);
        }

        FortranStatementPosition getPos(InputStream input, FortranStatementBasicPosition pos) throws IOException
        {// Obtain line information
            LinesInfo linesInfo = getLinesInfo(input);
            return FortranStatementPosition.createPosition(pos, linesInfo);
        }

        public String intoString() throws IOException
        {
            if (stack.size() > 1)
            {
                StringBuilder sb = new StringBuilder();
                sb.append("Include stack:\n");
                for (int i = 1, n = stack.size(); i < n; ++i)
                {
                    FileData fromPosData = stack.get(i - 1);
                    Path fromPosPath = fromPosData.path;
                    FileData incData = stack.get(i);
                    FortranStatementPosition incPos = getPos(fromPosData.strm, incData.stmtPos);
                    for (int j = 1; j < i; ++j)
                    {
                        sb.append("\t");
                    }
                    if (i == (n - 1))
                    {
                        sb.append("[");
                    }
                    sb.append(fromPosPath.toString()).append(":").append(incPos.getStartLineIdx() + 1);
                    if (i == (n - 1))
                    {
                        sb.append("]");
                    }
                    sb.append("\n");
                }
                return sb.toString();
            } else
            {
                return "";
            }
        }
    }

    /*
     * Methos is NOT thread-safe
     */
    public Set<Path> run(Path filePath, ByteArrayIOStream input, OutputStream output,
            Collection<Path> includesSearchPath) throws FortranException, IOException, Exception
    {
        this.output = output;
        this.includeFiles = new LinkedHashSet<Path>();
        this.includesSearchPath = new LinkedList<Path>(includesSearchPath);
        this.includeStack = new IncludeStack(filePath, input.getAsInputStreamUnsafe());
        run(filePath, input.getAsInputStreamUnsafe());
        return this.includeFiles;
    }

    List<FortranStatementBasicPosition> extractIncludeStatements(Path filePath, InputStream input) throws Exception
    {
        FilteredContentSequence fSeq = new FilteredContentSequence();
        try
        {
            List<FortranStatementBasicPosition> includeStatements;
            AsciiArrayIOStream inputWithoutComments = new AsciiArrayIOStream();
            FilteredContentSequence fComments = runCommentsFilter(input, inputWithoutComments);
            input.reset();
            fSeq.add(fComments);
            AsciiArrayIOStream inputWithoutLineBreaks = new AsciiArrayIOStream(inputWithoutComments.size());
            FilteredContentSequence fLineBreaks = runLineBreaksFilter(inputWithoutComments, inputWithoutLineBreaks);
            fSeq.add(fLineBreaks);
            inputWithoutComments = null;
            includeStatements = runIncludesFinder(inputWithoutLineBreaks);
            for (int i = 0, n = includeStatements.size(); i < n; ++i)
            {
                includeStatements.set(i, fSeq.getOriginal(includeStatements.get(i)));
            }
            return includeStatements;
        } catch (FortranException e)
        {
            if (e.getCharIdxInFile() != null)
            {
                e.setCharIdxInFile(fSeq.getOriginalChrIdx(e.getCharIdxInFile()));
            }
            int chrIdxInFile = e.getCharIdxInFile();
            LinesInfo linesInfo = getLinesInfo(input);
            String errMsg = String.format("Fortran parsing error at %s:%s:%s\n\t%s\n%s", filePath,
                    linesInfo.getLineIdx(chrIdxInFile), linesInfo.getLineChrOffset(chrIdxInFile), e.getMessage(),
                    includeStack.intoString());
            throw new Exception(errMsg, e);
        } catch (Exception e)
        {
            String errMsg = String.format("Error while processing %s\n\t%s\n%s", filePath, e.getMessage(),
                    includeStack.intoString());
            throw new Exception(errMsg, e);
        }
    }

    void run(Path inputFilePath, InputStream input) throws FortranException, IOException, Exception
    {
        List<FortranStatementBasicPosition> includeStatements = extractIncludeStatements(inputFilePath, input);
        int currentPos = 0;
        for (FortranStatementBasicPosition incStmt : includeStatements)
        {
            final int bytesToRead = incStmt.getStartCharIdx() - currentPos;
            currentPos += bytesToRead;
            copy(input, output, copyBuffer, bytesToRead);
            input.skip(incStmt.length());
            currentPos += incStmt.length();
            // ------------------------------------
            Path incFilePath = resolveIncludeStatement(inputFilePath, input, incStmt);
            includeFiles.add(incFilePath);
            try (AsciiArrayIOStream incFileStrm = new AsciiArrayIOStream(incFilePath))
            {
                includeStack.push(incFilePath, incFileStrm.getAsInputStreamUnsafe(), incStmt);
                run(incFilePath, incFileStrm.getAsInputStreamUnsafe());
                includeStack.pop();
            }
        }
        clawfc.Utils.copy(input, output);
    }

    Path resolveIncludeStatement(Path currentFilePath, InputStream input, FortranStatementBasicPosition incStmt)
            throws Exception
    {
        try
        {
            final String incStr = incStmt.getName();
            includesSearchPath.addFirst(clawfc.Utils.dirPath(currentFilePath));
            Path incFilePath = Utils.findFile(incStr, includesSearchPath);
            if (incFilePath == null)
            {
                StringBuilder srchPathStr = new StringBuilder();
                for (Path path : new LinkedHashSet<Path>(includesSearchPath))
                {
                    srchPathStr.append("\t");
                    srchPathStr.append(path.toString());
                    srchPathStr.append(" ;\n");
                }
                String errMsg = String.format("Failed to find file \"%s\" under given search path: \n%s", incStr,
                        srchPathStr.toString());
                throw new Exception(errMsg);
            }
            includesSearchPath.removeFirst();
            if (includeStack.contains(incFilePath))
            {
                String errMsg = String.format(
                        "Include file \"%s\" is already included. Recursive includes are forbidden.", incFilePath);
                throw new Exception(errMsg);
            }
            return incFilePath.normalize();
        } catch (Exception e)
        {
            LinesInfo linesInfo = getLinesInfo(input);
            int incStmtStartChrIdx = incStmt.getStartCharIdx();
            int lineIdx = linesInfo.getLineIdx(incStmtStartChrIdx) + 1;
            includeStack.push(currentFilePath, input, incStmt);
            String errMsg = String.format("Error while trying to resolve include at %s:%s\n%s\n%s", currentFilePath,
                    lineIdx, e.getMessage(), includeStack.intoString());
            throw new Exception(errMsg, e);
        }
    }

    FortranSyntaxException setInFilePosition(FortranSyntaxException e, AsciiArrayIOStream strm)
    {
        if (e.getCharIdxInFile() == null && e.getLineIndex() != null)
        {
            LinesInfo linesInfo = strm.getLinesInfo();
            int charIdxInFile = linesInfo.getLineStartByteIdx(e.getLineIndex() + e.getCharIdxInLine());
            e.setCharIdxInFile(charIdxInFile);
            e.setLineIndex(null);
            e.setCharIdxInLine(null);
        }
        return e;
    }

    FilteredContentSequence runCommentsFilter(InputStream input, AsciiArrayIOStream inputWithoutComments)
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

    FilteredContentSequence runLineBreaksFilter(AsciiArrayIOStream inputStrm, AsciiArrayIOStream inputWithoutLinebreaks)
            throws IOException, FortranSyntaxException
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

    List<FortranStatementBasicPosition> runIncludesFinder(AsciiArrayIOStream inputStrm) throws Exception
    {
        try
        {
            return includesFinder.run(inputStrm.getAsInputStreamUnsafe());
        } catch (FortranSyntaxException e)
        {
            throw setInFilePosition(e, inputStrm);
        }
    }
}
