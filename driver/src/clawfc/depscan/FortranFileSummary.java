/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import clawfc.depscan.parser.*;
import java.util.*;
import clawfc.utils.AsciiArrayIOStream;

public class FortranFileSummary
{
    public final List<FortranModuleInfo> modules;
    public final FortranModuleInfo program;
    
    static class HasCLAW
    {
        BitSet _lineHasCLAW;
        public HasCLAW(int numLines,
                FortranFileCLAWLinesInfo clawLinesInfo)
        {
            _lineHasCLAW = new BitSet(numLines);
            for(Integer i: clawLinesInfo.clawDirectives)
            { _lineHasCLAW.set(i); }
            for(Integer i: clawLinesInfo.clawGuards)
            { _lineHasCLAW.set(i); }
        }
        public boolean run(int startLine, int endLine)
        {
            for(int i = startLine; i <= endLine; ++i)
            {
                if(_lineHasCLAW.get(i))
                { return true; }                
            }
            return false;
            
        }        
    }
    
    public FortranFileSummary(FortranFileBasicSummary in,
                              AsciiArrayIOStream.LinesInfo linesInfo,
                              FortranFileCLAWLinesInfo clawLinesInfo)
	{
        HasCLAW hasCLAW = new HasCLAW(linesInfo.numLines(), clawLinesInfo);
	    ArrayList<FortranModuleInfo> modules = new ArrayList<FortranModuleInfo>(in.modules.size());
	    for(FortranModuleBasicInfo info: in.modules)
	    {
	        int startPos = linesInfo.lineStartByteIdx(info.startLineNum);
            int endPos = linesInfo.lineEndByteIdx(info.endLineNum);
            boolean usesCLAW = hasCLAW.run(info.startLineNum, info.endLineNum);
	        modules.add(new FortranModuleInfo(info, startPos, endPos, usesCLAW));
	    }
	    this.modules = Collections.unmodifiableList(modules);
	    if(in.program == null)
	    { program = null; }
	    else
	    {
	        FortranModuleBasicInfo info = in.program;
            int startPos = linesInfo.lineStartByteIdx(info.startLineNum);
            int endPos = linesInfo.lineEndByteIdx(info.endLineNum);
            boolean usesCLAW = hasCLAW.run(info.startLineNum, info.endLineNum);
            program = new FortranModuleInfo(info, startPos, endPos, usesCLAW);	        
	    }
	}
	
	public FortranFileSummary(List<FortranModuleInfo> modules,
							  FortranModuleInfo program)
	{
		this.modules = Collections.unmodifiableList(modules);
		this.program = program;
	}
	
	@Override
    public boolean equals(Object obj) 
    {
        if(this == obj)
        { return true; }
        if(obj == null)
        { return false; }
        if (getClass() != obj.getClass())
        { return false; }
        FortranFileSummary other = (FortranFileSummary) obj;
        if(!modules.equals(other.modules))
        { return false; }
        if(program == null)
        { 
        	if(other.program != null)
        	{ return false; }
        }
        else
        { 
        	if(!program.equals(other.program))
        	{ return false; }
        }
        return true;
    }
}
