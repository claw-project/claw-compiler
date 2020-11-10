/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import clawfc.depscan.parser.*;
import java.util.*;

public class FortranModuleInfo
{
    public final String name;
    //WARNING!!! This the number of the line AFTER "module <name>" statement
    public final int startLineNum;
    //WARNING!!! This the number of the line where "end module <name>" statement is located
    public final int endLineNum;
    //final boolean usesCLAW;
    public final List<String> usedModuleNames;
    
	public FortranModuleInfo(String name,
	                         int startLineNum,
                             int endLineNum,
                             //boolean usesCLAW,
	                         Collection<String> usedModuleNames)
	{
		this.name = name;
		this.startLineNum = startLineNum;
        this.endLineNum = endLineNum;
        //this.usesCLAW = usesCLAW;
		this.usedModuleNames = Collections.unmodifiableList(new ArrayList<String>(usedModuleNames));
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
        if(!name.equals(other.name))
        { return false; }
        if(startLineNum != other.startLineNum)
        { return false; }
        if(endLineNum != other.endLineNum)
        { return false; }
        //if(usesCLAW != other.usesCLAW)
        //{ return false; }
        if(!usedModuleNames.equals(other.usedModuleNames))
        { return false; }
        return true;
    }
}
