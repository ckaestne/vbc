package edu.cmu.cs.vbc.prog.gpl;

/**
 * @author chupanw
 */
public class NumberWorkSpace implements  WorkSpace {
/*@(Number)*/

    int vertexCounter;
/*@(Number)*/


    public NumberWorkSpace() {
        vertexCounter = 0;
    }
/*@(Number)*/


    public void preVisitAction( Vertex v )
    {
        // This assigns the values on the way in
        if ( v.visited!=true )
            v.VertexNumber = vertexCounter++;
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
    public void nextRegionAction(Vertex v) {
        //

    }


    //@Override
    public void postVisitAction(Vertex v) {
        //

    }
}

