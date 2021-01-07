/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2021, MeteoSwiss
 */
package clawfc.depscan;

import java.io.IOException;

import clawfc.depscan.serial.FortranProgramUnitType;

public interface FortranProgramUnitStatementsRecognizer
{
    public static enum StatementType {
        ModuleOpen, ModuleClose, ProgramOpen, ProgramClose, BlockDataOpen, BlockDataClose, UseModule, SubroutineOpen,
        SubroutineClose, FunctionOpen, FunctionClose;

        public FortranProgramUnitType toUnitType()
        {
            switch (this)
            {
            case BlockDataClose:
                return FortranProgramUnitType.BLOCK_DATA;
            case BlockDataOpen:
                return FortranProgramUnitType.BLOCK_DATA;
            case FunctionClose:
                return FortranProgramUnitType.FUNCTION;
            case FunctionOpen:
                return FortranProgramUnitType.FUNCTION;
            case ModuleClose:
                return FortranProgramUnitType.MODULE;
            case ModuleOpen:
                return FortranProgramUnitType.MODULE;
            case ProgramClose:
                return FortranProgramUnitType.PROGRAM;
            case ProgramOpen:
                return FortranProgramUnitType.PROGRAM;
            case SubroutineClose:
                return FortranProgramUnitType.SUBROUTINE;
            case SubroutineOpen:
                return FortranProgramUnitType.SUBROUTINE;
            default:
                return null;
            }
        }

        public static StatementType getOpenType(FortranProgramUnitType type)
        {

            switch (type)
            {
            case BLOCK_DATA:
                return StatementType.BlockDataOpen;
            case MODULE:
                return StatementType.ModuleOpen;
            case PROGRAM:
                return StatementType.ProgramOpen;
            case SUBROUTINE:
                return StatementType.SubroutineOpen;
            case FUNCTION:
                return StatementType.FunctionOpen;
            default:
                return null;
            }
        }

        public static StatementType getCloseType(FortranProgramUnitType type)
        {
            switch (type)
            {
            case BLOCK_DATA:
                return StatementType.BlockDataClose;
            case MODULE:
                return StatementType.ModuleClose;
            case PROGRAM:
                return StatementType.ProgramClose;
            case SUBROUTINE:
                return StatementType.SubroutineClose;
            case FUNCTION:
                return StatementType.FunctionClose;
            default:
                return null;
            }
        }
    }

    /**
     * @return Name of the FORTRAN program unit
     * @throws IOException
     * @throws Exception
     */
    public String parseUnitStatement(StatementType type, String input) throws IOException, Exception;
}
