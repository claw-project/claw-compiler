/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import clawfc.depscan.parser.*;
import java.util.*;

public class FortranFileSummary
{
    List<FortranModuleDependencies> _modules;
    FortranModuleDependencies _program;
    
	public List<FortranModuleDependencies> modules() { return _modules; }
	public FortranModuleDependencies program() { return _program; };
	
	public FortranFileSummary(LinkedHashMap<String, LinkedHashSet<String>> moduleDependencies,
			                  LinkedHashMap<String, LinkedHashSet<String>> programDependencies)
	{
	    ArrayList<FortranModuleDependencies> modules = new ArrayList<FortranModuleDependencies>(moduleDependencies.size());
		for(Map.Entry<String, LinkedHashSet<String>> entry : moduleDependencies.entrySet()) 
		{ modules.add(new FortranModuleDependencies(entry.getKey(), entry.getValue())); }
		_modules = Collections.unmodifiableList(modules);
		if(!programDependencies.isEmpty())
		{
			Map.Entry<String, LinkedHashSet<String>> entry = programDependencies.entrySet().iterator().next();
			_program = new FortranModuleDependencies(entry.getKey(), entry.getValue());			
		}		
	}
	
	public FortranFileSummary(FortranModuleDependencies[] modules,
							  FortranModuleDependencies program)
	{
		this._modules = Collections.unmodifiableList(new ArrayList<FortranModuleDependencies>(Arrays.asList(modules)));
		this._program = program;
	}
	
	/*public FortranFileSummary()
	{
		this._modules = new ArrayList<FortranModuleDependencies>();
		this._program = null;
	}*/
	
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
        if(!modules().equals(other.modules()))
        { return false; }
        if(program() == null)
        { 
        	if(other.program() != null)
        	{ return false; }
        }
        else
        { 
        	if(!program().equals(other.program()))
        	{ return false; }
        }
        return true;
    }
}
