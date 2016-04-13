package edu.cmu.cs.vbc.prog.gpl;

import java.io.IOException;

/**
 * @author chupanw
 */
public class TreeGenerator {
    public static final int CHILDREN_COUNT = 4;
    public static final int MAX_DEPTH = 4;
    public static final String FILENAME = "C:\\papers\\shared\\casestudies-12-01-2011\\GPL\\generated-gpl-benchmark.txt";

    public static int nodeCount = 0;
    public static int edgeCount = 0;
    public static String str = "";

    public static void processNode(int x, int depth)
    {
        if(depth < MAX_DEPTH)
        {
            for(int i = 0; i < CHILDREN_COUNT; i++)
            {
                int childNode = ++nodeCount;
                str += (x + " " + childNode + "\n");
                edgeCount++;
                processNode(childNode, depth+1);
            }
        }
    }

    public static void main(String args[])
    {
        processNode(0, 0);

        int weight = 10;
        for(int i = 0; i < edgeCount; i++)
            str += weight + "\n";

        str = (nodeCount+1) + " " + edgeCount + " 99 99 99" + "\n" +
                str;

        try {
            JavaUtility.INSTANCE.writeToFile(FILENAME, str);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

