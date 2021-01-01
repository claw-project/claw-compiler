/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.report;

import java.io.FileWriter;
import java.nio.file.Path;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import claw.ClawVersion;
import claw.shenron.transformation.TransformationGroup;
import claw.wani.x2t.configuration.Configuration;
import claw.wani.x2t.translator.ClawTranslator;
import claw.wani.x2t.translator.ClawTranslatorDriver;

/**
 * Generation of the transformation report. Report includes information about
 * the configuration used for the transformation phase as well as information
 * about applied transformation.
 *
 * @author clementval
 */
public class ClawTransformationReport
{

    private static final int MAX_COL = 80;
    private FileWriter _report;

    /**
     * Constructs a transformation report object.
     *
     * @param reportPath Path of the report file.
     * @throws Exception If file cannot be created or cannot be written.
     */
    public ClawTransformationReport(Path reportPath) throws Exception
    {
        _report = new FileWriter(reportPath.toString());
    }

    /**
     * Generate the report to file.
     *
     * @param args       Arguments passed to the translator.
     * @param translator Current translator used during the transformation.
     * @throws Exception If file cannot be created or cannot be written.
     */
    public void generate(String[] args, ClawTranslatorDriver translator, Configuration cfg) throws Exception
    {
        printHeader("CLAW Transformation Report");
        printMainInfo(translator, args, cfg);
        printTransformationOrderInfo(translator.getTranslator());
        printTransformationInfo();
        _report.flush();
    }

    /**
     * Print a header surrounded by dashed lines.
     *
     * @param headerTitle Title to be included in the header.
     * @throws Exception If file cannot be created or cannot be written.
     */
    private void printHeader(String headerTitle) throws Exception
    {
        printDashedLine();
        printLine(generateStr((MAX_COL / 2) - (headerTitle.length() / 2), ' ') + headerTitle);
        printDashedLine();
    }

    /**
     * Write the header of the report. Contains information driving the
     * transformation.
     *
     * @param translator Current translator used during the transformation.
     * @param args       Arguments passed to the translator.
     * @throws Exception If file cannot be created or cannot be written.
     */
    private void printMainInfo(ClawTranslatorDriver translator, String[] args, Configuration cfg) throws Exception
    {
        printTitle("Information");

        DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        Date date = new Date();

        List<String[]> infos = new ArrayList<>();

        infos.add(new String[] { "File", translator.getTranslationUnit().getSource() });
        infos.add(new String[] { "Transformed", dateFormat.format(date) });
        infos.add(new String[] { "Fortran to IR", translator.getTranslationUnit().getTime() });
        infos.add(new String[] { "OMNI Front-end", translator.getTranslationUnit().getCompilerInfo() });
        infos.add(new String[] { "XcodeML/F", translator.getTranslationUnit().getVersion() });
        infos.add(new String[] { "CLAW Compiler", ClawVersion.VERSION });
        infos.add(new String[] { "Target", cfg.getCurrentTarget().toString() });
        infos.add(new String[] { "Directive", cfg.getCurrentDirective().toString() });
        infos.add(new String[] { "Driver command", "" }); // TODO
        infos.add(new String[] { "Translator command", String.join(" ", args) });

        int indentCol = 0;
        for (String[] info : infos)
        {
            if (info[0].length() > indentCol)
            {
                indentCol = info[0].length();
            }
        }

        for (String[] info : infos)
        {
            printInfo(info[0], info[1], indentCol + 1);
        }
    }

    /**
     * Print information about the transformations group and their application
     * order.
     *
     * @param translator Current translator used during the transformation.
     * @throws Exception If file cannot be created or cannot be written.
     */
    private void printTransformationOrderInfo(ClawTranslator translator) throws Exception
    {
        printTitle("Transformation information");

        // Print column header
        String format = "%-7s %-50s %-10s %-10s";
        printLine(String.format(format, "Order", "Transformation name", "Nb trans.", "Nb applied"));
        printLine(String.format(format, "-----", "-------------------", "---------", "----------"));
        int index = 1;
        for (Map.Entry<Class<?>, TransformationGroup> entry : translator.getGroups().entrySet())
        {
            printLine(String.format(format, index++, entry.getValue().transformationName(), entry.getValue().count(),
                    entry.getValue().getAppliedTransformationCount()));
        }
    }

    private void printTransformationInfo() throws Exception
    {
        printTitle("Transformation");
        // TODO
    }

    /**
     * Write information in a formatted way: title : value
     *
     * @param title Title for the information.
     * @param value Value of the information.
     * @throws Exception If file cannot be created or cannot be written.
     */
    private void printInfo(String title, String value, int indentCol) throws Exception
    {
        printLine(String.format("%s%s: %s", title, generateStr(indentCol - title.length(), ' '), value));

    }

    /**
     * Write value and add new line at the end.
     *
     * @param value Value to be written.
     * @throws Exception If file cannot be created or cannot be written.
     */
    private void printLine(String value) throws Exception
    {
        _report.write(value + "\n");
    }

    /**
     * Print a dashed line of the width of the report.
     *
     * @throws Exception If file cannot be created or cannot be written.
     */
    private void printDashedLine() throws Exception
    {
        printLine(generateStr(MAX_COL, '-'));
    }

    /**
     * Print the given text as a section title and underlined with dashes. Title is
     * preceded and followed by an empty line.
     *
     * @param title Text to be written as a section title.
     * @throws Exception If file cannot be created or cannot be written.
     */
    private void printTitle(String title) throws Exception
    {
        printLine("");
        printLine(title);
        printLine(generateStr(title.length(), '-'));
        printLine("");
    }

    /**
     * Generate a string with the given character and size.
     *
     * @param size Length of the string to be generated.
     * @param c    Character used during the generation.
     * @return A string of the given length filled with the given character.
     */
    private String generateStr(int size, char c)
    {
        StringBuilder str = new StringBuilder();
        for (int i = 0; i < size; ++i)
        {
            str.append(c);
        }
        return str.toString();
    }
}
