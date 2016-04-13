package edu.cmu.cs.vbc.prog.gpl;

import java.io.IOException;

/**
 * @author chupanw
 */
public class NetworkGenerator {
    public static final int WIDTH = 10;
    public static final int DEPTH = 20;

    /**
     * result[0]: string representing the edges of the network
     * result[1]: the last node number used
     * result[2]: number of nodes
     * result[3]: number of edges
     *
     * @param root
     * @return
     */
    public static Object[] getNetwork(int root, int width)
    {
        String edgesStr = "";
        int bottom = root + width + 1;
        for(int i = 0; i < width; i++)
        {
            int child = (root+i+1);
            // root to child
            edgesStr += root + " " + child + "\n";
            // child to bottom
            edgesStr += child + " " + bottom + "\n";
        }
        return new Object[]{edgesStr, bottom, width+1+1, width*2};
    }

    public static void main(String args[])
    {
        String str = "";
        int root = 0;
        int nodeCount = 0;
        int edgeCount = 0;
        for(int i = 0; i < DEPTH; i++)
        {
            Object[] result = getNetwork(root, WIDTH);
            str += (String)result[0];
            root = (Integer)result[1];
            nodeCount += (Integer)result[2];
            edgeCount += (Integer)result[3];
        }
        // roots are shared, so take it into account
        nodeCount -= (DEPTH-1);

        str = nodeCount + " " + edgeCount + " 99 99 99" + "\n" + str;
        int weight = 10;
        for(int i = 0; i < edgeCount; i++)
            str += weight + "\n";

        try {
            JavaUtility.INSTANCE.writeToFile(TreeGenerator.FILENAME, str);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

