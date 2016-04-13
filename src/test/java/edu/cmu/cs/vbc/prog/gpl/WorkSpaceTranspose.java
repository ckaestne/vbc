package edu.cmu.cs.vbc.prog.gpl;

/**
 * @author chupanw
 */
public class WorkSpaceTranspose implements  WorkSpace {
/*@(StronglyConnected)*/

    // Strongly Connected Component Counter
    int SCCCounter;
/*@(StronglyConnected)*/


    public WorkSpaceTranspose()
    {
        SCCCounter = 0;
    }
/*@(StronglyConnected)*/


    public void preVisitAction( Vertex v )
    {
        if ( v.visited!=true )
        {
            v.strongComponentNumber = SCCCounter;
        }
        ;
    }
/*@(StronglyConnected)*/


    public void nextRegionAction( Vertex v )
    {
        SCCCounter++;
    }


    //@Override
    public void checkNeighborAction(Vertex vsource, Vertex vtarget) {
        //

    }


    //@Override
    public void init_vertex(Vertex v) {
        //

    }


    //@Override
    public void postVisitAction(Vertex v) {
        //

    }

}

