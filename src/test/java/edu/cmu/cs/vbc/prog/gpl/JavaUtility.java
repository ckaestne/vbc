package edu.cmu.cs.vbc.prog.gpl;

import java.io.*;

/**
 * @author chupanw
 */
public class JavaUtility {
    public static final JavaUtility INSTANCE = new JavaUtility();

    public void writeToFile(File file, String content) throws IOException
    {
        BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(file));
        bufferedWriter.write(content);
        bufferedWriter.close();
    }

    public void writeToFile(String filename, String content) throws IOException
    {
        writeToFile(filename, content, false);
    }

    public void writeToFile(String filename, String content, boolean append) throws IOException
    {
        BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(filename, append));
        bufferedWriter.write(content);
        bufferedWriter.close();
    }

    /**
     * A utility method to read a file
     */
    public String getFileContents(String filename) throws IOException
    {
        StringBuffer fileContents = new StringBuffer();
        BufferedReader bufferedReader = new BufferedReader(new FileReader(filename));
        final int bufLength = 1024;
        char[] buf = new char[bufLength];
        int charsRead = -1;

        while((charsRead = bufferedReader.read(buf, 0, bufLength)) != -1)
        {
            // Eliminate null characters
            //int lastNonNullIndex = bufLength-1;
            //while(lastNonNullIndex >= 0 && buf[lastNonNullIndex] == '\0');
            //	lastNonNullIndex--;
            //if(lastNonNullIndex >= 0)

            fileContents.append(buf, 0, charsRead);
            buf = new char[bufLength];
        }
        bufferedReader.close();

        return fileContents.toString();
    }
}

