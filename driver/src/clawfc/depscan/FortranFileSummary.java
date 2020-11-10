/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import clawfc.depscan.parser.*;
import java.util.*;

public class FortranFileSummary
{
    public final List<FortranModuleInfo> modules;
    public final FortranModuleInfo program;
    
	public FortranFileSummary(Map<String, LinkedHashSet<String>> moduleDependencies,
	                          Map<String, Integer> moduleLineStart,
                              Map<String, Integer> moduleLineEnd,
			                  Map<String, LinkedHashSet<String>> programDependencies,
			                  Map<String, Integer> programLineStart,
                              Map<String, Integer> programLineEnd)
	{
	    ArrayList<FortranModuleInfo> modules = new ArrayList<FortranModuleInfo>(moduleDependencies.size());
		for(Map.Entry<String, LinkedHashSet<String>> entry : moduleDependencies.entrySet()) 
		{ 
		    String moduleName = entry.getKey();
		    modules.add(new FortranModuleInfo(moduleName, 
		                                      moduleLineStart.get(moduleName),
		                                      moduleLineEnd.get(moduleName),
		                                      entry.getValue())); 
		}
		this.modules = Collections.unmodifiableList(modules);
		if(!programDependencies.isEmpty())
		{
			Map.Entry<String, LinkedHashSet<String>> entry = programDependencies.entrySet().iterator().next();
			String programName = entry.getKey();
			program = new FortranModuleInfo(programName,
			                                programLineStart.get(programName),
			                                programLineEnd.get(programName),
			                                entry.getValue());			
		}
		else
		{ program = null; }
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
