/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.depscan;

import java.io.IOException;

import clawfc.depscan.serial.FortranProgramUnitType;

public interface FortranProgramUnitStatementsRecognizer
{
    public static enum StatementType {
        MODULE_OPEN, MODULE_CLOSE, PROGRAM_OPEN, PROGRAM_CLOSE, BLOCK_DATA_OPEN, BLOCK_DATA_CLOSE, USE_MODULE, SUBROUTINE_OPEN,
        SUBROUTINE_CLOSE, FUNCTION_OPEN, FUNCTION_CLOSE;

        public FortranProgramUnitType toUnitType()
        {
            switch (this)
            {
            case BLOCK_DATA_CLOSE:
                return FortranProgramUnitType.BLOCK_DATA;
            case BLOCK_DATA_OPEN:
                return FortranProgramUnitType.BLOCK_DATA;
            case FUNCTION_CLOSE:
                return FortranProgramUnitType.FUNCTION;
            case FUNCTION_OPEN:
                return FortranProgramUnitType.FUNCTION;
            case MODULE_CLOSE:
                return FortranProgramUnitType.MODULE;
            case MODULE_OPEN:
                return FortranProgramUnitType.MODULE;
            case PROGRAM_CLOSE:
                return FortranProgramUnitType.PROGRAM;
            case PROGRAM_OPEN:
                return FortranProgramUnitType.PROGRAM;
            case SUBROUTINE_CLOSE:
                return FortranProgramUnitType.SUBROUTINE;
            case SUBROUTINE_OPEN:
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
                return StatementType.BLOCK_DATA_OPEN;
            case MODULE:
                return StatementType.MODULE_OPEN;
            case PROGRAM:
                return StatementType.PROGRAM_OPEN;
            case SUBROUTINE:
                return StatementType.SUBROUTINE_OPEN;
            case FUNCTION:
                return StatementType.FUNCTION_OPEN;
            default:
                return null;
            }
        }

        public static StatementType getCloseType(FortranProgramUnitType type)
        {
            switch (type)
            {
            case BLOCK_DATA:
                return StatementType.BLOCK_DATA_CLOSE;
            case MODULE:
                return StatementType.MODULE_CLOSE;
            case PROGRAM:
                return StatementType.PROGRAM_CLOSE;
            case SUBROUTINE:
                return StatementType.SUBROUTINE_CLOSE;
            case FUNCTION:
                return StatementType.FUNCTION_CLOSE;
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
