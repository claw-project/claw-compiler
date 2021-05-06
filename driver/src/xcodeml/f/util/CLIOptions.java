/*
 * @author Mikhail Zhigun
 */
package xcodeml.f.util;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.sourceforge.argparse4j.ArgumentParsers;
import net.sourceforge.argparse4j.helper.HelpScreenException;
import net.sourceforge.argparse4j.impl.Arguments;
import net.sourceforge.argparse4j.inf.ArgumentGroup;
import net.sourceforge.argparse4j.inf.ArgumentParser;
import net.sourceforge.argparse4j.inf.ArgumentParserException;
import net.sourceforge.argparse4j.inf.MutuallyExclusiveGroup;
import net.sourceforge.argparse4j.inf.Namespace;

public class CLIOptions
{
    final Path src_file_path;
    final Path out_file_path;
    final Path intrinsic_xmod_dir_path;
    final Path stdout_file_path;
    final List<Path> inc_dir_paths;
    final List<Path> xmod_inc_dir_paths;
    final List<Path> xmod_inc_paths;
    final Integer max_line_len;
    final Integer max_cont_line;
    final Integer auto_save_attr_kb;
    final Integer max_name_len;
    final Integer default_single_real_type_size;
    final Integer default_double_real_type_size;
    final Boolean lang_f77;
    final Boolean lang_f90;
    final Boolean lang_f95;
    final Boolean debug_enabled;
    final Boolean yacc_debug_enabled;
    final Boolean module_compile_enabled;
    final Boolean omp_enabled;
    final Boolean xmp_enabled;
    final Boolean xmp_coarray_enabled;
    final Boolean acc_enabled;
    final Boolean cond_compile_enabled;
    final Boolean leave_comment_enabled;
    final Boolean do_implicit_undef;
    final Boolean force_fixed_format_enabled;
    final Boolean force_c_comments_enabled;
    final Boolean dollar_in_id_enabled;
    final Boolean end_line_no_enabled;
    final Boolean ocl_enabled;
    final Boolean cdir_enabled;
    final Boolean pgi_enabled;
    final Boolean module_cache_enabled;
    final Boolean add_timestamp_enabled;
    final Boolean print_help;
    final Boolean print_opts;
    final Boolean native_in_mem_mode_enabled;
    final Boolean fsync_enabled;

    public static CLIOptions parseCmdlineArguments(String[] args, Path workingDir) throws Exception
    {
        ArgumentParser parser = ArgumentParsers.newFor("FFront").build().description("FFront is a "
                + "source-to-source compiler which translates Fortran source into XcodeML intermediate representation");
        Namespace parsedArgs = null;
        try
        {
            parser.addArgument("fortran-file").nargs("?").help("Input file");
            parser.addArgument("-o", "--output-file").help("output file path");
            parser.addArgument("-fintrinsic-xmodules-path").help("intrinsic xmod include dir");
            parser.addArgument("-I", "--pp-include-dir").nargs("*").action(Arguments.append()).help(
                    "Add the directory to the search path for include files reference in preprocessor directives");
            parser.addArgument("-M", "-MI", "--mod-include-dir").nargs("*").action(Arguments.append())
                    .help("Add directory to the search path for .xmod files.");
            parser.addArgument("-d").action(Arguments.storeTrue()).help("enable debug mode");
            parser.addArgument("-yd").action(Arguments.storeTrue()).help("enable YACC debug mode");
            parser.addArgument("-no-module-cache").action(Arguments.storeFalse()).help("always load module from file");
            parser.addArgument("-module-compile").action(Arguments.storeTrue()).help("only generate Xmod file ");
            parser.addArgument("-print-help").action(Arguments.storeTrue()).help("print help");
            parser.addArgument("-print-opts").action(Arguments.storeTrue()).help("print CLI options");
            parser.addArgument("-fopenmp").action(Arguments.storeTrue()).help("enable openmp translation");
            parser.addArgument("-facc").action(Arguments.storeTrue()).help("enable OpenACC translation");
            parser.addArgument("-fxmp").action(Arguments.storeTrue()).help("enable XcalableMP translation");
            parser.addArgument("-pgi").action(Arguments.storeTrue()).help("keep PGI directives");
            parser.addArgument("-fno-xmp-coarray").action(Arguments.storeFalse())
                    .help("disable translation coarray statements to XcalableMP subroutine calls");
            parser.addArgument("-Kscope-omp").action(Arguments.storeTrue()).help("enable conditional compilation");
            MutuallyExclusiveGroup fOpts = parser.addMutuallyExclusiveGroup("Format options");
            fOpts.addArgument("-force-fixed-format").action(Arguments.storeTrue()).help("read file as fixed format");
            parser.addArgument("-max-line-length").type(Integer.class)
                    .help("set max columns in a line (default is 72 in fixed format and 132 in free format)");
            parser.addArgument("-max-cont-line").type(Integer.class)
                    .help("set max number of continuation lines (default n=255)");
            parser.addArgument("-force-c-comment").action(Arguments.storeTrue())
                    .help("enable 'c' comment in free format");
            MutuallyExclusiveGroup lOpts = parser.addMutuallyExclusiveGroup("Language  options");
            lOpts.addArgument("-f77").action(Arguments.storeTrue()).help("use F77 spec intrinsic");
            lOpts.addArgument("-f90").action(Arguments.storeTrue()).help("use F90 spec intrinsic");
            lOpts.addArgument("-f95").action(Arguments.storeTrue()).help("use F95 spec intrinsic");
            parser.addArgument("-u").action(Arguments.storeTrue()).help("use no implicit type");
            parser.addArgument("-r", "--single-prec-size").type(Integer.class).choices(4, 8)
                    .help("set single precision size (default N=4)");
            parser.addArgument("--double-prec-size").type(Integer.class).choices(4, 8)
                    .help("set double precision size (default N=8)");
            parser.addArgument("--save").type(Integer.class).help(
                    "add save attribute n kbytes except in a recursive function and common variables (default n=1)");
            parser.addArgument("-max-name-len").type(Integer.class).help("set maximum identifier name length");
            parser.addArgument("-fdollar-ok").action(Arguments.storeTrue()).help("enable using '$' in identifier");
            parser.addArgument("-fleave-comment").action(Arguments.storeTrue()).help("leave comment in xcodeml file");
            parser.addArgument("-endlineno").action(Arguments.storeTrue()).help("output the endlineno attribute");
            parser.addArgument("-ocl").action(Arguments.storeTrue()).help("enable ocl");
            parser.addArgument("-cdir").action(Arguments.storeTrue()).help("enable cdir");
            parser.addArgument("-no-time").action(Arguments.storeFalse()).help("do not add timestamp to xcodeml");
            ArgumentGroup jniOpts = parser.addArgumentGroup("JNI-only options");
            jniOpts.addArgument("-m", "--input-xmod").nargs("*").action(Arguments.append())
                    .help("Add xmod file to import cache");
            jniOpts.addArgument("--in-memory-mode").action(Arguments.storeTrue()).help(
                    "In this mode native code is not allowed to access files or IO streams. All external IO will be performed by java wrapper");
            jniOpts.addArgument("-so", "--stdout-file").nargs("?").help("Path to file where stdout should be written");
            jniOpts.addArgument("--no-fsync").action(Arguments.storeFalse())
                    .help("Do not explicitly flush output files to disk");
            parsedArgs = parser.parseArgs(args);
        } catch (HelpScreenException hse)
        {
            return null;
        } catch (ArgumentParserException ape)
        {
            parser.handleError(ape);
            throw ape;
        }
        CLIOptions opts = new CLIOptions(parsedArgs, workingDir);
        return opts;
    }

    CLIOptions(Namespace parsedArgs, Path workingDir) throws Exception
    {
        src_file_path = getOptionalPath(parsedArgs, workingDir, "fortran_file");
        out_file_path = getOptionalPath(parsedArgs, workingDir, "output_file");
        intrinsic_xmod_dir_path = getOptionalPath(parsedArgs, workingDir, "fintrinsic_xmodules_path");
        inc_dir_paths = getPathList(parsedArgs, workingDir, "pp_include_dir");
        xmod_inc_dir_paths = getPathList(parsedArgs, workingDir, "mod_include_dir");
        omp_enabled = parsedArgs.getBoolean("fopenmp");
        acc_enabled = parsedArgs.getBoolean("facc");
        xmp_enabled = parsedArgs.getBoolean("fxmp");
        pgi_enabled = parsedArgs.getBoolean("pgi");
        xmp_coarray_enabled = parsedArgs.getBoolean("fno_xmp_coarray");
        cond_compile_enabled = parsedArgs.getBoolean("Kscope_omp");
        force_fixed_format_enabled = parsedArgs.getBoolean("force_fixed_format");
        max_line_len = parsedArgs.getInt("max_line_length");
        max_cont_line = parsedArgs.getInt("max_cont_line");
        force_c_comments_enabled = parsedArgs.getBoolean("force_c_comment");
        lang_f77 = parsedArgs.getBoolean("f77");
        lang_f90 = parsedArgs.getBoolean("f90");
        lang_f95 = parsedArgs.getBoolean("f95");
        do_implicit_undef = parsedArgs.getBoolean("u");
        default_single_real_type_size = parsedArgs.getInt("single_prec_size");
        default_double_real_type_size = parsedArgs.getInt("double_prec_size");
        auto_save_attr_kb = parsedArgs.getInt("save");
        max_name_len = parsedArgs.getInt("max_name_len");
        dollar_in_id_enabled = parsedArgs.getBoolean("fdollar_ok");
        leave_comment_enabled = parsedArgs.getBoolean("fleave_comment");
        end_line_no_enabled = parsedArgs.getBoolean("endlineno");
        debug_enabled = parsedArgs.getBoolean("d");
        yacc_debug_enabled = parsedArgs.getBoolean("yd");
        module_cache_enabled = parsedArgs.getBoolean("no_module_cache");
        module_compile_enabled = parsedArgs.getBoolean("module_compile");
        print_help = parsedArgs.getBoolean("print_help");
        print_opts = parsedArgs.getBoolean("print_opts");
        ocl_enabled = parsedArgs.getBoolean("ocl");
        cdir_enabled = parsedArgs.getBoolean("cdir");
        add_timestamp_enabled = parsedArgs.getBoolean("no_time");
        xmod_inc_paths = getPathList(parsedArgs, workingDir, "input_xmod");
        native_in_mem_mode_enabled = parsedArgs.getBoolean("in_memory_mode");
        stdout_file_path = getOptionalPath(parsedArgs, workingDir, "stdout_file");
        fsync_enabled = parsedArgs.getBoolean("no_fsync");
    }

    static Path getOptionalPath(Namespace parsedArgs, Path workingDir, String name)
    {
        String str = parsedArgs.getString(name);
        Path path = null;
        if (str != null)
        {
            path = toAbsPath(workingDir, str);
        }
        return path;
    }

    static List<Path> getPathList(Namespace parsedArgs, Path workingDir, String name)
    {
        List<Path> res = new ArrayList<Path>();
        List<List<String>> strs = parsedArgs.<List<String>>getList(name);
        if (strs != null)
        {
            for (List<String> lstStr : strs)
            {
                for (String s : lstStr)
                {
                    res.add(toAbsPath(workingDir, s));
                }
            }
        }
        return Collections.unmodifiableList(res);
    }

    static Path toAbsPath(Path workingDir, String pathStr)
    {
        Path path = Paths.get(pathStr);
        if (!path.isAbsolute())
        {
            path = workingDir.resolve(path);
        }
        path = path.normalize();
        return path;
    }

    public Path getSrc_file_path()
    {
        return src_file_path;
    }

    public Path getOut_file_path()
    {
        return out_file_path;
    }

    public Path getIntrinsic_xmod_dir_path()
    {
        return intrinsic_xmod_dir_path;
    }

    public List<Path> getInc_dir_paths()
    {
        return inc_dir_paths;
    }

    public List<Path> getXmod_inc_dir_paths()
    {
        return xmod_inc_dir_paths;
    }

    public Integer getMax_line_len()
    {
        return max_line_len;
    }

    public Integer getMax_cont_line()
    {
        return max_cont_line;
    }

    public Integer getAuto_save_attr_kb()
    {
        return auto_save_attr_kb;
    }

    public Integer getMax_name_len()
    {
        return max_name_len;
    }

    public Integer getDefault_single_real_type()
    {
        return default_single_real_type_size;
    }

    public Integer getDefault_double_real_type()
    {
        return default_double_real_type_size;
    }

    public Boolean getLang_f77()
    {
        return lang_f77;
    }

    public Boolean getLang_f90()
    {
        return lang_f90;
    }

    public Boolean getLang_f95()
    {
        return lang_f95;
    }

    public Boolean getDebug_enabled()
    {
        return debug_enabled;
    }

    public Boolean getYacc_debug_enabled()
    {
        return yacc_debug_enabled;
    }

    public Boolean getModule_compile_enabled()
    {
        return module_compile_enabled;
    }

    public Boolean getOmp_enabled()
    {
        return omp_enabled;
    }

    public Boolean getXmp_enabled()
    {
        return xmp_enabled;
    }

    public Boolean getXmp_coarray_disabled()
    {
        return xmp_coarray_enabled;
    }

    public Boolean getAcc_enabled()
    {
        return acc_enabled;
    }

    public Boolean getCond_compile_enabled()
    {
        return cond_compile_enabled;
    }

    public Boolean getLeave_comment_enabled()
    {
        return leave_comment_enabled;
    }

    public Boolean getDo_implicit_undef()
    {
        return do_implicit_undef;
    }

    public Boolean getForce_fixed_format_enabled()
    {
        return force_fixed_format_enabled;
    }

    public Boolean getForce_c_comments_enabled()
    {
        return force_c_comments_enabled;
    }

    public Boolean getDollar_in_id_enabled()
    {
        return dollar_in_id_enabled;
    }

    public Boolean getEnd_line_no_enabled()
    {
        return end_line_no_enabled;
    }

    public Boolean getOcl_enabled()
    {
        return ocl_enabled;
    }

    public Boolean getCdir_enabled()
    {
        return cdir_enabled;
    }

    public Boolean getPgi_enabled()
    {
        return pgi_enabled;
    }

    public Boolean getModule_cache_disabled()
    {
        return module_cache_enabled;
    }

    public Boolean getPrint_help()
    {
        return print_help;
    }

    public Boolean getPrint_opts()
    {
        return print_opts;
    }

    public Boolean addTimestamp_enabled()
    {
        return add_timestamp_enabled;
    }
}
