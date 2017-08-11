////////////////////////////////////////////////////////////////////////////////
// checkstyle: Checks Java source code for adherence to a set of rules.
// Copyright (C) 2001-2015 the original author or authors.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
////////////////////////////////////////////////////////////////////////////////
package edu.cmu.cs.vbc.prog.checkstyle.api;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import edu.cmu.cs.vbc.prog.checkstyle.grammars.CommentListener;

import java.io.File;
import java.util.*;
import java.util.regex.Pattern;

/**
 * Represents the contents of a file.
 *
 * @author Oliver Burn
 * @version 1.0
 */
public final class FileContents implements CommentListener
{
    /**
     * the pattern to match a single line comment containing only the comment
     * itself -- no code.
     */
    private static final String MATCH_SINGLELINE_COMMENT_PAT = "^\\s*//.*$";
    /** compiled regexp to match a single-line comment line */
    private static final Pattern MATCH_SINGLELINE_COMMENT = Pattern
            .compile(MATCH_SINGLELINE_COMMENT_PAT);

    /** the file name */
    private final String filename;

    /** the text */
    private final FileText text;

    /** map of the Javadoc comments indexed on the last line of the comment.
     * The hack is it assumes that there is only one Javadoc comment per line.
     */
    private final Map<Integer, TextBlock> javadocComments = new HashMap<>();
    /** map of the C++ comments indexed on the first line of the comment. */
    private final Map<Integer, TextBlock> cppComments =
        new HashMap<>();

    /**
     * map of the C comments indexed on the first line of the comment to a list
     * of comments on that line
     */
    private final Map<Integer, List<TextBlock>> clangComments = new HashMap<>();

    /**
     * Creates a new <code>FileContents</code> instance.
     *
     * @param filename name of the file
     * @param lines the contents of the file
     * @deprecated Use {@link #FileContents(FileText)} instead
     *   in order to preserve the original line breaks where possible.
     */
    @Deprecated public FileContents(String filename, String[] lines)
    {
        this.filename = filename;
        text = FileText.fromLines(new File(filename), Arrays.asList(lines));
    }

    /**
     * Creates a new <code>FileContents</code> instance.
     *
     * @param text the contents of the file
     */
    public FileContents(FileText text)
    {
        filename = text.getFile().toString();
        this.text = text;
    }

    /** {@inheritDoc} */
    @Override
    public void reportSingleLineComment(String type, int startLineNo,
            int startColNo)
    {
        reportCppComment(startLineNo, startColNo);
    }

    /** {@inheritDoc} */
    @Override
    public void reportBlockComment(String type, int startLineNo,
            int startColNo, int endLineNo, int endColNo)
    {
        reportCComment(startLineNo, startColNo, endLineNo, endColNo);
    }

    /**
     * Report the location of a C++ style comment.
     * @param startLineNo the starting line number
     * @param startColNo the starting column number
     **/
    public void reportCppComment(int startLineNo, int startColNo)
    {
        final String line = line(startLineNo - 1);
        final String[] txt = new String[] {line.substring(startColNo)};
        final Comment comment = new Comment(txt, startColNo, startLineNo,
                line.length() - 1);
        cppComments.put(startLineNo, comment);
    }

    /**
     * Returns a map of all the C++ style comments. The key is a line number,
     * the value is the comment {@link TextBlock} at the line.
     * @return the Map of comments
     */
    public ImmutableMap<Integer, TextBlock> getCppComments()
    {
        return ImmutableMap.copyOf(cppComments);
    }

    /**
     * Report the location of a C-style comment.
     * @param startLineNo the starting line number
     * @param startColNo the starting column number
     * @param endLineNo the ending line number
     * @param endColNo the ending column number
     **/
    public void reportCComment(int startLineNo, int startColNo,
            int endLineNo, int endColNo)
    {
        final String[] cc = extractCComment(startLineNo, startColNo,
                endLineNo, endColNo);
        final Comment comment = new Comment(cc, startColNo, endLineNo,
                endColNo);

        // save the comment
        if (clangComments.containsKey(startLineNo)) {
            final List<TextBlock> entries = clangComments.get(startLineNo);
            entries.add(comment);
        }
        else {
            final List<TextBlock> entries = Lists.newArrayList();
            entries.add(comment);
            clangComments.put(startLineNo, entries);
        }

        // Remember if possible Javadoc comment
        if (line(startLineNo - 1).indexOf("/**", startColNo) != -1) {
            javadocComments.put(endLineNo - 1, comment);
        }
    }

    /**
     * Returns a map of all C style comments. The key is the line number, the
     * value is a {@link List} of C style comment {@link TextBlock}s
     * that start at that line.
     * @return the map of comments
     */
    public ImmutableMap<Integer, List<TextBlock>> getCComments()
    {
        return ImmutableMap.copyOf(clangComments);
    }

    /**
     * Returns the specified C comment as a String array.
     * @return C comment as a array
     * @param startLineNo the starting line number
     * @param startColNo the starting column number
     * @param endLineNo the ending line number
     * @param endColNo the ending column number
     **/
    private String[] extractCComment(int startLineNo, int startColNo,
            int endLineNo, int endColNo)
    {
        String[] retVal;
        if (startLineNo == endLineNo) {
            retVal = new String[1];
            retVal[0] = line(startLineNo - 1).substring(startColNo,
                    endColNo + 1);
        }
        else {
            retVal = new String[endLineNo - startLineNo + 1];
            retVal[0] = line(startLineNo - 1).substring(startColNo);
            for (int i = startLineNo; i < endLineNo; i++) {
                retVal[i - startLineNo + 1] = line(i);
            }
            retVal[retVal.length - 1] = line(endLineNo - 1).substring(0,
                    endColNo + 1);
        }
        return retVal;
    }

    /**
     * Returns the Javadoc comment before the specified line.
     * A return value of <code>null</code> means there is no such comment.
     * @return the Javadoc comment, or <code>null</code> if none
     * @param lineNoBefore the line number to check before
     **/
    public TextBlock getJavadocBefore(int lineNoBefore)
    {
        // Lines start at 1 to the callers perspective, so need to take off 2
        int lineNo = lineNoBefore - 2;

        // skip blank lines
        while ((lineNo > 0) && (lineIsBlank(lineNo) || lineIsComment(lineNo))) {
            lineNo--;
        }

        return javadocComments.get(lineNo);
    }

    /**
     * Get a single line.
     * For internal use only, as getText().get(lineNo) is just as
     * suitable for external use and avoids method duplication.
     * @param lineNo the number of the line to get
     * @return the corresponding line, without terminator
     * @throws IndexOutOfBoundsException if lineNo is invalid
     */
    private String line(int lineNo)
    {
        return text.get(lineNo);
    }

    /**
     * Get the full text of the file.
     * @return an object containing the full text of the file
     */
    public FileText getText()
    {
        return text;
    }

    /** @return the lines in the file */
    public String[] getLines()
    {
        return text.toLinesArray();
    }

    /**
     * Get the line from text of the file.
     * @param index index of the line
     * @return line from text of the file
     */
    public String getLine(int index)
    {
        return text.get(index);
    }

    /** @return the name of the file */
    public String getFilename()
    {
        return filename;
    }

    /**
     * Checks if the specified line is blank.
     * @param lineNo the line number to check
     * @return if the specified line consists only of tabs and spaces.
     **/
    public boolean lineIsBlank(int lineNo)
    {
        // possible improvement: avoid garbage creation in trim()
        return "".equals(line(lineNo).trim());
    }

    /**
     * Checks if the specified line is a single-line comment without code.
     * @param lineNo  the line number to check
     * @return if the specified line consists of only a single line comment
     *         without code.
     **/
    public boolean lineIsComment(int lineNo)
    {
        return MATCH_SINGLELINE_COMMENT.matcher(line(lineNo)).matches();
    }

    /**
     * Checks if the specified position intersects with a comment.
     * @param startLineNo the starting line number
     * @param startColNo the starting column number
     * @param endLineNo the ending line number
     * @param endColNo the ending column number
     * @return true if the positions intersects with a comment.
     **/
    public boolean hasIntersectionWithComment(int startLineNo,
            int startColNo, int endLineNo, int endColNo)
    {
        // Check C comments (all comments should be checked)
        final Collection<List<TextBlock>> values = clangComments.values();
        for (final List<TextBlock> row : values) {
            for (final TextBlock comment : row) {
                if (comment.intersects(startLineNo, startColNo, endLineNo,
                        endColNo))
                {
                    return true;
                }
            }
        }

        // Check CPP comments (line searching is possible)
        for (int lineNumber = startLineNo; lineNumber <= endLineNo;
             lineNumber++)
        {
            final TextBlock comment = cppComments.get(lineNumber);
            if ((comment != null)
                    && comment.intersects(startLineNo, startColNo,
                            endLineNo, endColNo))
            {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks if the current file is a package-info.java file.
     * @return true if the package file.
     */
    public boolean inPackageInfo()
    {
        return this.getFilename().endsWith("package-info.java");
    }
}
