package edu.cmu.cs.vbc.prog.gpl;

/**
 * @author chupanw
 */
public interface WorkSpace {
    /*@(Search)*/
    // supply template actions
    public void init_vertex( Vertex v );
/*@(Search)*/

    public void preVisitAction( Vertex v );
/*@(Search)*/

    public void postVisitAction( Vertex v );
/*@(Search)*/

    public void nextRegionAction( Vertex v );
/*@(Search)*/

    public void checkNeighborAction( Vertex vsource,
                                     Vertex vtarget );
}

