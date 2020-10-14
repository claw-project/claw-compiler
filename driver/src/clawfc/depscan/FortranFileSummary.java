/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import clawfc.depscan.parser.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;

public class FortranFileSummary
{
	
	ArrayList<FortranModuleDependencies> modules;
	FortranModuleDependencies program;
	
	public FortranFileSummary(LinkedHashMap<String, LinkedHashSet<String>> moduleDependencies,
			                  LinkedHashMap<String, LinkedHashSet<String>> programDependencies)
	{
		modules = new ArrayList<FortranModuleDependencies>(moduleDependencies.size());
		for(Map.Entry<String, LinkedHashSet<String>> entry : moduleDependencies.entrySet()) 
		{ modules.add(new FortranModuleDependencies(entry.getKey(), entry.getValue())); }
		if(!programDependencies.isEmpty())
		{
			Map.Entry<String, LinkedHashSet<String>> entry = programDependencies.entrySet().iterator().next();
			program = new FortranModuleDependencies(entry.getKey(), entry.getValue());			
		}		
	}
	
	public FortranFileSummary(FortranModuleDependencies[] modules,
							  FortranModuleDependencies program)
	{
		this.modules = new ArrayList<FortranModuleDependencies>(Arrays.asList(modules));
		this.program = program;
	}
	
	public FortranFileSummary()
	{
		this.modules = new ArrayList<FortranModuleDependencies>();
		this.program = null;
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
