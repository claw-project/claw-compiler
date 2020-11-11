/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import clawfc.depscan.parser.*;
import java.util.*;

public class FortranModuleBasicInfo
{
    public final String name;
    //WARNING!!! This the number of the line AFTER "module <name>" statement
    public final int startLineNum;
    //WARNING!!! This the number of the line where "end module <name>" statement is located
    public final int endLineNum;
    
    public final List<String> usedModuleNames;
    
	public FortranModuleBasicInfo(String name,
	                         int startLineNum,
                             int endLineNum,
	                         Collection<String> usedModuleNames)
	{
		this.name = name;
		this.startLineNum = startLineNum;
        this.endLineNum = endLineNum;
		this.usedModuleNames = Collections.unmodifiableList(new ArrayList<String>(usedModuleNames));
	}
    
    public FortranModuleBasicInfo(FortranModuleBasicInfo info)
    {
        this.name = info.name;
        this.startLineNum = info.startLineNum;
        this.endLineNum = info.endLineNum;
        this.usedModuleNames = info.usedModuleNames;
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
        FortranModuleBasicInfo other = (FortranModuleBasicInfo) obj;
        if(!name.equals(other.name))
        { return false; }
        if(startLineNum != other.startLineNum)
        { return false; }
        if(endLineNum != other.endLineNum)
        { return false; }
        if(!usedModuleNames.equals(other.usedModuleNames))
        { return false; }
        return true;
    }
}
