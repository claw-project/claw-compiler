/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.primitive;

import claw.tatsu.TatsuConstant;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import org.w3c.dom.Node;

/**
 * Primitive transformation and test applied on body node. This included: -
 * Append a body sub-tree to another one. - Shift statements into a body.
 *
 * @author clementval
 */
public final class Body
{

    // Avoid instantiation of this class
    private Body()
    {
    }

    /**
     * Append the slave body to the master body.
     *
     * @param masterBody Master body node.
     * @param slaveBody  Slave body bode.
     * @throws IllegalTransformationException If given nodes are null or not body
     *                                        nodes.
     */
    public static void append(Xnode masterBody, Xnode slaveBody) throws IllegalTransformationException
    {
        if (!Xnode.isOfCode(masterBody, Xcode.BODY) || !Xnode.isOfCode(slaveBody, Xcode.BODY))
        {
            throw new IllegalTransformationException(String.format("%s for Body.append. opcode: %s - %s",
                    TatsuConstant.ERROR_INCOMPATIBLE, masterBody == null ? "null" : masterBody.opcode(),
                    slaveBody == null ? "null" : slaveBody.opcode()));
        }

        // Move all nodes to master body
        Xnode crtNode = slaveBody.firstChild();
        while (crtNode != null)
        {
            Xnode nextSibling = crtNode.nextSibling();
            masterBody.append(crtNode);
            crtNode = nextSibling;
        }
    }

    /**
     * Shift all statements from the first siblings of the "from" element until the
     * "until" element if "included" is true.
     *
     * @param from       Start element for the shifting.
     * @param until      End element for the shifting.
     * @param targetBody Body element in which statements are inserted.
     * @param included   If true, until element is shifted.
     * @throws IllegalTransformationException If one element is null or the
     *                                        targetBody element is not a body
     *                                        element.
     */
    public static void shiftIn(Xnode from, Xnode until, Xnode targetBody, boolean included)
            throws IllegalTransformationException
    {
        if (from == null && until == null)
        {
            return;
        }

        if (from == null || until == null || !Xnode.isOfCode(targetBody, Xcode.BODY))
        {
            throw new IllegalTransformationException(
                    String.format("%s for Body.shiftIn. opcode: %s, from: %s, until: %s",
                            TatsuConstant.ERROR_INCOMPATIBLE, targetBody == null ? "null" : targetBody.opcode(),
                            from == null ? "null" : from.opcode(), until == null ? "null" : until.opcode()));
        }

        Node currentSibling = from.element();
        if (!included)
        {
            currentSibling = from.element().getNextSibling();
        }

        Node firstStatementInBody = targetBody.element().getFirstChild();
        while (currentSibling != null && currentSibling != until.element())
        {
            Node nextSibling = currentSibling.getNextSibling();
            targetBody.element().insertBefore(currentSibling, firstStatementInBody);
            currentSibling = nextSibling;
        }
        if (included && currentSibling == until.element())
        {
            targetBody.element().insertBefore(currentSibling, firstStatementInBody);
        }
    }

    /**
     * Check whether a body is empty.
     *
     * @param body Body element to check.
     * @return True if empty. False otherwise.
     * @throws IllegalTransformationException If given element is null or not a body
     *                                        element.
     */
    public static boolean isEmpty(Xnode body) throws IllegalTransformationException
    {
        if (!Xnode.isOfCode(body, Xcode.BODY))
        {
            throw new IllegalTransformationException(String.format("%s for Body.isEmpty. opcode: %s",
                    TatsuConstant.ERROR_INCOMPATIBLE, body == null ? "null" : body.opcode()));
        }

        return body.firstChild() == null;
    }
}
