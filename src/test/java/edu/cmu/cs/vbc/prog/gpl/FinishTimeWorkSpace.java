package edu.cmu.cs.vbc.prog.gpl;

/**
 * @author chupanw
 */
public class FinishTimeWorkSpace implements  WorkSpace {
/*@(StronglyConnected)*/

    int FinishCounter;
/*@(StronglyConnected)*/


    public FinishTimeWorkSpace() {
        FinishCounter = 1;
    }
/*@(StronglyConnected)*/


    public void preVisitAction( Vertex v )
    {
        if ( v.visited!=true )
            FinishCounter++;
    }
/*@(StronglyConnected)*/


    public void postVisitAction( Vertex v ) {
        v.finishTime = FinishCounter++;
    } // of postVisit


    //@Override
    public void checkNeighborAction(Vertex vsource, Vertex vtarget) {

    }


    //@Override
    public void init_vertex(Vertex v) {

    }


    //@Override
    public void nextRegionAction(Vertex v) {

    }
}

