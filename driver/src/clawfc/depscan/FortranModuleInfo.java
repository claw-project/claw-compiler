/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import clawfc.depscan.parser.*;
import java.util.*;

public class FortranModuleInfo
    extends FortranModuleBasicInfo
{
    public final int startCharPos;
    public final int endCharPos;
    public final boolean usesCLAW;
    
	public FortranModuleInfo(FortranModuleBasicInfo info,
	                         int startCharPos,
                             int endCharPos,
                             boolean usesCLAW)
	{
	    super(info);
		this.startCharPos = startCharPos;
		this.endCharPos = endCharPos;
        this.usesCLAW = usesCLAW;
	}	
    
    public FortranModuleInfo(String name,
                             int startLineNum,
                             int endLineNum,
                             Collection<String> usedModuleNames,
                             int startCharPos,
                             int endCharPos,
                             boolean usesCLAW)
    {
        super(name, startLineNum, endLineNum, usedModuleNames);
        this.startCharPos = startCharPos;
        this.endCharPos = endCharPos;
        this.usesCLAW = usesCLAW;
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
        FortranModuleInfo other = (FortranModuleInfo) obj;
        if(!super.equals(other))
        { return false; }
        if(startCharPos != other.startCharPos)
        { return false; }
        if(endCharPos != other.endCharPos)
        { return false; }
        if(usesCLAW != other.usesCLAW)
        { return false; }
        return true;
    }
}
