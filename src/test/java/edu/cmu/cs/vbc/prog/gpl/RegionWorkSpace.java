package edu.cmu.cs.vbc.prog.gpl;

/**
 * @author chupanw
 */
public class RegionWorkSpace implements  WorkSpace {
/*@(Connected)*/

    int counter;
/*@(Connected)*/


    public RegionWorkSpace() {
        counter = 0;
    }
/*@(Connected)*/


    public void init_vertex( Vertex v ) {
        v.componentNumber = -1;
    }
/*@(Connected)*/


    public void postVisitAction( Vertex v ) {
        v.componentNumber = counter;
    }
/*@(Connected)*/


    public void nextRegionAction( Vertex v ) {
        counter ++;
    }


    //@Override
    public void checkNeighborAction(Vertex vsource, Vertex vtarget) {
        //

    }


    //@Override
    public void preVisitAction(Vertex v) {
        //

    }
}

