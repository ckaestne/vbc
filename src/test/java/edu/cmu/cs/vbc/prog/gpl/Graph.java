package edu.cmu.cs.vbc.prog.gpl;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;

/**
 * @author chupanw
 */
public class Graph {
    public LinkedList<Vertex> vertices;

    public Graph() {
        vertices = new LinkedList<>();
    }

    public void addVertex( Vertex v ) {
        vertices.add( v );
    }

    // Finds a vertex given its name in the vertices list
    public  Vertex findsVertex( String theName )
    {
        int i=0;
        Vertex theVertex;

        // if we are dealing with the root
        if ( theName==null )
            return null;

        for( i=0; i<vertices.size(); i++ )
        {
            theVertex = vertices.get( i );
            if ( theName.equals( theVertex.name ) )
                return theVertex;
        }
        return null;
    }

    // Adds and edge by setting start as adjacent to end and
    // viceversa
    public void addEdge( Vertex start,  Vertex end )
    {
        start.addAdjacent( end );
        if (Main.UNDIRECTED) {
            end.addAdjacent( start );
        }
    }

    // Adds an edge with weights
    public void addAnEdge( Vertex start,  Vertex end, int weight )
    {
        addEdge( start,end, weight );
    }

    public void addEdge( Vertex start,  Vertex end, int weight )
    {
        addEdge( start,end ); // adds the start and end as adjacent
        start.addWeight( weight ); // the direction layer takes care of that

        // if the graph is undirected you have to include
        // the weight of the edge coming back
        if (Main.UNDIRECTED)
            end.addWeight( weight );
    }

    public void display()
    {
        int s = vertices.size();
        int i;

        Main.splPrint___( "Vertices " );
        for ( i=0; i<s; i++ )
        {
            Vertex aVertex = vertices.get(i);
            aVertex.display();
        }
    }

    public void GraphSearch( WorkSpace w ) {
        int           s, c;
        Vertex  v = null;

        // Step 1: initialize visited member of all nodes

        s = vertices.size();
        if ( s == 0 )
            return;

        // Showing the initialization process
        for ( c = 0; c < s; c++ ) {
            v = vertices.get( c );
            v.init_vertex( w );
        }

        // Step 2: traverse adjacentVertices of each node

        for ( c = 0; c < s; c++ ) {
            v = vertices.get( c );
            if ( !v.visited ) {
                w.nextRegionAction( v );
                nodeSearch(v, w);
            }
        } //end for

    }

    public void nodeSearch(Vertex v, WorkSpace w)
    {
        if (Main.BFS) {
            v.bftNodeSearch( w );
        }
        else{
            v.dftNodeSearch( w );
        }
    }

    public void NumberVertices() {
        GraphSearch( new NumberWorkSpace() );
    }

    public void ConnectedComponents() {
        GraphSearch( new RegionWorkSpace() );
    }
	/*@(StronglyConnected)*/


    public   Graph StrongComponents() {

        FinishTimeWorkSpace FTWS = new FinishTimeWorkSpace();

        // 1. Computes the finishing times for each vertex
        GraphSearch( FTWS );

        // 2. Order in decreasing  & call DFS Transposal
        Collections.sort( vertices,
                new Comparator<Object>() {
			/*@(StronglyConnected)*/

                    public int compare( Object o1, Object o2 )
                    {
                        Vertex v1 = ( Vertex )o1;
                        Vertex v2 = ( Vertex )o2;

                        if ( v1.finishTime > v2.finishTime )
                            return -1;

                        if ( v1.finishTime == v2.finishTime )
                            return 0;
                        return 1;
                    }
                } );

        // 3. Compute the transpose of G
        // Done at layer transpose
        Graph gaux = ComputeTranspose( this );

        // 4. Traverse the transpose G
        WorkSpaceTranspose WST = new WorkSpaceTranspose();
        gaux.GraphSearch( WST );

        return gaux;

    }
	/*@(Transpose)*/


    public  Graph ComputeTranspose( Graph the_graph )
    {
        int i=0,j=0;
        int num_vertices = ( the_graph.vertices ).size();
        Vertex the_vertex, the_adjacent,
                the_new_vertex, the_new_adjacent;
        int num_adjacents =0;
        String theName;

        // Creating the new Graph
        Graph newGraph = new  Graph();

        // Creates and adds the vertices with the same name
        for ( i=0; i<num_vertices; i++ )
        {
            theName = ( the_graph.vertices ).get( i ).name;
            newGraph.addVertex( new  Vertex().assignName( theName ) );
        }

        // In the old graph, traverse all the vertices
        for ( i=0; i<num_vertices; i++ )
        {
            the_vertex = ( the_graph.vertices ).get( i );
            num_adjacents = ( the_vertex.adjacentVertices ).size();

            // finds the object reference of the_vertex in new list
            the_new_vertex = newGraph.findsVertex( the_vertex.name );

            // In each vertex traverse its adjacentVertices list
            for( j=0; j< num_adjacents; j++ )
            {
                // for each of the vertices in the list find out its vertex
                the_adjacent = the_vertex.adjacentVertices.get( j );

                // finds the object reference of the_adjacent in new list
                the_new_adjacent = newGraph.findsVertex( the_adjacent.name );

                // adds to the_new_adjacent the_new_vertex
                // switches the direction of the arrow head
                ( the_new_adjacent.adjacentVertices ).add( the_new_vertex );

                // Passes the vertex where to take the information from,
                // the index number in that list, and the reference where
                // to put the adorn value
                the_new_adjacent.adjustAdorns( the_vertex,j );

            } // for j all adjacentVertices
        } // for i all vertices

        return newGraph;
    }
	/*@(Cycle)*/


    public boolean CycleCheck() {
        CycleWorkSpace c = new CycleWorkSpace( );
        GraphSearch( c );
        return c.AnyCycles;
    }
	/*@(MSTPrim)*/


    public  Graph Prim( Vertex r ) {
        Vertex root;

        root = r;
        int numvertices = vertices.size();
        int i;
        Vertex x;

        // 2. and 3. Initializes the vertices
        for ( i=0; i < numvertices; i++ )
        {
            x = vertices.get( i );
            x.pred = null;
            x.key = Integer.MAX_VALUE;
        }

        // 4. and 5.
        root.key = 0;
        root.pred = null;

        // 2. S <- empty set

        // 1. Queue <- V[G], copy the vertex in the graph in the priority queue
        LinkedList<Vertex> Queue = new LinkedList<>();

        for( i=0; i < numvertices; i++ )
        {
            x = vertices.get( i );
            if ( x.key != 0 ) // this means, if this is not the root
                Queue.add( x );
        }

        // Inserts the root at the head of the queue
        Queue.addFirst( root );

        // 6. while Q!=0
        Vertex ucurrent;
        int j,k,l;
        int pos;
        ArrayList<Vertex> Uneighbors;
        Vertex u,v;

        int wuv;
        boolean isNeighborInQueue = false;

        while ( Queue.size()!=0 )
        {
            // 7. u <- Extract-Min(Q);
            // since this an ordered queue the first element is the min
            u = Queue.removeFirst();

            // 8. for each vertex v adjacent to u
            Uneighbors = u.adjacentVertices;

            for( k=0; k < Uneighbors.size(); k++ )
            {
                v = Uneighbors.get( k );

                // Check to see if the neighbor is in the queue
                isNeighborInQueue = false;

                // if the Neighor is in the queue
                int indexNeighbor = Queue.indexOf( v );
                if ( indexNeighbor>=0 )
                    isNeighborInQueue=true;

                wuv = u.weightsList.get( k ).intValue();

                // 9. Relax (u,v w)
                if ( isNeighborInQueue && ( wuv < v.key ) )
                {
                    v.key = wuv;
                    v.pred = u.name;
                    Uneighbors.set( k,v ); // adjust values in the adjacentVertices

                    // update the values of v in the queue
                    // if (indexNeighbor>=0) Queue.set(indexNeighbor,v);

                    // update the values of v in the queue
                    // Remove v from the Queue so that we can reinsert it
                    // in a new place according to its new value to keep
                    // the Linked List ordered
                    Object residue = Queue.remove( indexNeighbor );

                    // Get the new position for v
                    int position = Collections.binarySearch( Queue,v,
                            new Comparator<Object>() {
						/*@(MSTPrim)*/

                                public int compare( Object o1, Object o2 )
                                {
                                    Vertex v1 = ( Vertex )o1;
                                    Vertex v2 = ( Vertex )o2;

                                    if ( v1.key < v2.key )
                                        return -1;

                                    if ( v1.key == v2.key )
                                        return 0;
                                    return 1;
                                }
                            } );

                    // Adds v in its new position in Queue
                    if ( position < 0 )  // means it is not there
                    {
                        Queue.add( - ( position+1 ),v );
                    }
                    else      // means it is there
                    {
                        Queue.add( position,v );
                    }
                } // if 9. Relax

            } // for all adjacentVertices
        } // of while

        // Creates a new graph from the original vertices and the pred
        // fields
        String theName;
        Graph newGraph = new  Graph();

        // Creates and adds the vertices with the same name
        for ( i=0; i<numvertices; i++ )
        {
            theName = vertices.get( i ).name;
            newGraph.addVertex( new  Vertex().assignName( theName ) );
        }

        Vertex theVertex, thePred;
        Vertex theNewVertex, theNewPred;
        Vertex theNeighbor;
        boolean flag = false;

        // adds the adjacent Vertices based on the pred fields
        for ( i=0; i<numvertices; i++ )
        {
            // theVertex and its predecessor
            theVertex = vertices.get( i );
            thePred = findsVertex( theVertex.pred );

            // if theVertex is the source then continue we dont need
            // to create a new neighbor
            if ( thePred==null )
                continue;

            // Find the references in the new Graph
            theNewVertex = newGraph.findsVertex( theVertex.name );
            theNewPred = newGraph.findsVertex( thePred.name );

            // Creates the new neighbor in predecessor -> vertex in the new
            // Graph and adjusts the adorns based on the old edge
            ( theNewPred.adjacentVertices ).add( theNewVertex );

            // the adjacent corresponds to the neighbor formed with
            // theVertex -> thePred
            // find the corresponding neighbor of the Vertex, that is,
            // predecessor
            j=0;
            flag=false;
            do
            {
                theNeighbor = ( theVertex.adjacentVertices ).get( j );
                if ( theNeighbor.name.equals( thePred.name ) ) {
                    flag = true;
                    theNewPred.adjustAdorns( theVertex, j );
                } else
                    j++;
            }
            while( flag==false && j < theVertex.adjacentVertices.size());

            // What remains to be done is to adjust the adorns
            // by copying  the values that are embedded in
            // theNeighbor object
//			theNewPred.adjustAdorns( theVertex, j );

        } // all the vertices

        return newGraph;
    }
	/*@(MSTKruskal)*/


    public  Graph Kruskal() {

        // 1. A <- Empty set
        LinkedList<Edge> A = new LinkedList<Edge>();

        // 2. for each vertex v E V[G]
        // 3.    do Make-Set(v)
        int numvertices = vertices.size();
        int i;
        Vertex v;

        for ( i=0; i < numvertices; i++ )
        {
            v = vertices.get( i );
            v.representative = v; // I am in my set
            v.members = new LinkedList<Vertex>(); // I have no members in my set
        }

        // 4. sort the edges of E by nondecreasing weight w
        // Creates the edges objects
        int j;
        LinkedList<?> Vneighbors = new LinkedList<Object>();
        Vertex u;

        LinkedList<Edge> edges = new LinkedList<Edge>();
        int k1=0,k2=0, sizeAV=0;
        for( k1=0; k1< numvertices; k1++ )
        {
            v = vertices.get( k1 );
            sizeAV = v.adjacentVertices.size();
            for ( k2=0; k2<sizeAV; k2++ ) {
                edges.add( new  Edge( v,
                        ( v.adjacentVertices ).get( k2 ),
                        ( v.weightsList ).get( k2 ).intValue() ) );
            } // of k2
        } // of k1

        // Sort the Edges in non decreasing order
        Collections.sort( edges,
                new Comparator<Object>() {
			/*@(MSTKruskal)*/

                    public int compare( Object o1, Object o2 )
                    {
                        Edge e1 = ( Edge )o1;
                        Edge e2 = ( Edge )o2;
                        if ( e1.weight < e2.weight )
                            return -1;
                        if ( e1.weight == e2.weight )
                            return 0;
                        return 1;
                    }
                } );

        // 5. for each edge in the nondecresing order
        int numedges = edges.size();
        Edge e1;
        Vertex vaux, urep, vrep;

        for( i=0; i<numedges; i++ )
        {
            // 6. if Find-Set(u)!=Find-Set(v)
            e1 = edges.get( i );
            u = e1.start;
            v = e1.end;

            if ( ! ( v.representative.name ).equals( u.representative.name ) )
            {
                // 7. A <- A U {(u,v)}
                A.add( e1 );

                // 8. Union(u,v)
                urep = u.representative;
                vrep = v.representative;

                if ( ( urep.members ).size() > ( vrep.members ).size() )
                { // we add elements of v to u
                    for( j=0; j< ( vrep.members ).size(); j++ )
                    {
                        vaux = ( vrep.members ).get( j );
                        vaux.representative = urep;
                        ( urep.members ).add( vaux );
                    }
                    v.representative = urep;
                    vrep.representative = urep;
                    ( urep.members ).add( v );
                    if ( !v.equals( vrep ) )
                        ( urep.members ).add( vrep );
                    ( vrep.members ).clear();
                }
                else
                { // we add elements of u to v
                    for( j=0; j< ( urep.members ).size(); j++ )
                    {
                        vaux = ( urep.members ).get( j );
                        vaux.representative = vrep;
                        ( vrep.members ).add( vaux );
                    }
                    u.representative = vrep;
                    urep.representative = vrep;
                    ( vrep.members ).add( u );
                    if ( !u.equals( urep ) )
                        ( vrep.members ).add( urep );
                    ( urep.members ).clear();

                } // else

            } // of if

        } // of for numedges

        // 9. return A

        // Creates a new graph from the original vertices and the pred
        // fields
        String theName;
        Graph newGraph = new  Graph();

        // Creates and adds the vertices with the same name
        for ( i=0; i<numvertices; i++ )
        {
            theName = vertices.get( i ).name;
            newGraph.addVertex( new  Vertex().assignName( theName ) );
        }

        // Creates the new adjacent vertices based on the edges
        Vertex theNeighbor,newNeighbor;
        Vertex theStartVertex, theEndVertex;
        boolean flag = false;

        // for all the edges
        for( i=0; i< A.size(); i++ )
        {
            // The current edge
            e1 = A.get( i );

            // the starnt and end Vertex
            theStartVertex = newGraph.findsVertex( e1.start.name );
            theEndVertex = newGraph.findsVertex( e1.end.name );

            // Creates the new neighbor  from startVertex -> endVertex in the new
            // Graph and adjusts the adorns based on the old edge
            ( theStartVertex.adjacentVertices ).add( theEndVertex );

            // theNeighbor corresponds to the neighbor formed with
            // theStartVertex -> theEndVertex
            // find the corresponding neighbor of the Vertex, that is,
            // predecessor
            j=0;
            flag=false;
            do
            {
                theNeighbor = ( e1.start.adjacentVertices ).get( j );
                if ( theNeighbor.name.equals( theEndVertex.name ) )
                    flag = true;
                else
                    j++;
            }
            while( flag==false && j < e1.start.adjacentVertices.size() );

            // What remains to be done is to adjust the adorns
            // by copying  the values that are embedded in
            // theNeighbor object
            theStartVertex.adjustAdorns( e1.start,j );

        } // for all the edges

        return newGraph;
    }
	/*@(Shortest)*/


    // Executes ShortestPath
    public void run( Vertex s )
    {
        if (Main.SHORTEST) {
            System.out.println("RUN SHORTEST");
            Graph gaux3 = ShortestPath( s );
            Graph.stopProfile();
            Main.splPrint___( "******************************************" );
            Main.splPrint___("<SHORTEST___ graph>");
            gaux3.display();
            Graph.resumeProfile();
        }
        if (Main.MSTKRUSKAL) {
            System.out.println("RUN MSTK");
            Graph gaux2 = Kruskal();
            Graph.stopProfile();
            Main.splPrint___( "******************************************" );
            Main.splPrint___("<MSTKRUSKAL___ graph>");
            gaux2.display();
            Graph.resumeProfile();
        }
        if (Main.MSTPRIM) {
            System.out.println("RUN MSTP");
            Graph gaux = Prim( s );
            Graph.stopProfile();
            Main.splPrint___( "******************************************" );
            Main.splPrint___("<MSTPRIM___ graph>");
            gaux.display();
            Graph.resumeProfile();
        }
        if (Main.CYCLE) {
            System.out.println("RUN CYCLE");
            Main.splPrint___( "******************************************" );
            Graph.stopProfile();
            Main.splPrint___( "<CYCLE___>: " + CycleCheck() );
            Graph.resumeProfile();
        }
        if (Main.STRONGLYCONNECTED) {
            System.out.println("RUN STRCON");
            Graph gaux4 = StrongComponents();
            Graph.stopProfile();
            Main.splPrint___( "******************************************" );
            Main.splPrint___( "<STRONGLYCONNECTED___ graph>");
            gaux4.display();
            Graph.resumeProfile();
        }
        if (Main.CONNECTED) {
            System.out.println("RUN CON");
            Graph.stopProfile();
            ConnectedComponents();
            Graph.resumeProfile();
        }
        if (Main.NUMBER) {
            System.out.println("RUN NUMBER");
            Graph.stopProfile();
            NumberVertices();
            Graph.resumeProfile();
        }
        System.out.println("FIN");
    }
	/*@(Shortest)*/


    public  Graph ShortestPath( Vertex s ) {
        Vertex source;

        source = s;
        int numvertices = vertices.size();
        int i;
        Vertex x,n;
        int wuvn;
        int k1;

        // 1. Initializes the single source
        for ( i=0; i < numvertices; i++ )
        {
            x = vertices.get( i );
            x.predecessor = null;
            x.dweight = Integer.MAX_VALUE;
        }

        source.dweight = 0;
        source.predecessor = null;

        // 2. S <- empty set
        LinkedList<Vertex> S = new LinkedList<>();

        // 3. Queue <- V[G], copy the vertex in the graph in the priority queue
        LinkedList<Vertex> Queue = new LinkedList<>();
        for( i=0; i < numvertices; i++ )
        {
            x = vertices.get( i );
            if ( x.dweight != 0 ) // this means, if this is not the source
                Queue.add( x );
        }

        // Inserts the source at the head of the queue
        Queue.addFirst( source );

        // 4. while Q!=0
        Vertex ucurrent;
        int j,k,l;
        int pos;
        ArrayList<Vertex> Uneighbors;
        Vertex u,v;
        int wuv;

        while ( Queue.size()!=0 )
        {
            // 5. u <- Extract-Min(Q);
            u = Queue.removeFirst();

            // 6. S <- S U {u}
            S.add( u );

            // 7. for each vertex v adjacent to u
            Uneighbors = u.adjacentVertices;

            // For all the neighbors
            for( k=0; k < Uneighbors.size(); k++ )
            {
                v = Uneighbors.get( k );
                wuv = u.weightsList.get( k ).intValue();

                // 8. Relax (u,v w)
                if ( v.dweight > ( u.dweight +  wuv ) )
                {
                    v.dweight = u.dweight +  wuv;
                    v.predecessor = u.name;
                    Uneighbors.set( k,v ); // adjust values in the neighbors

                    // update the values of v in the queue
                    int indexNeighbor = Queue.indexOf( v );
                    if ( indexNeighbor>=0 )
                    {
                        Object residue = Queue.remove( indexNeighbor );

                        // Get the new position for v
                        int position = Collections.binarySearch( Queue,v,
                                new Comparator<Object>() {
							/*@(Shortest)*/

                                    public int compare( Object o1, Object o2 )
                                    {
                                        Vertex v1 = ( Vertex )o1;
                                        Vertex v2 = ( Vertex )o2;

                                        if ( v1.dweight < v2.dweight )
                                            return -1;

                                        if ( v1.dweight == v2.dweight )
                                            return 0;
                                        return 1;
                                    }
                                } );

                        // Adds v in its new position in Queue
                        if ( position < 0 )  // means it is not there
                        {
                            Queue.add( - ( position+1 ),v );
                        }
                        else      // means it is there
                        {
                            Queue.add( position,v );
                        }
                    } // if in queue
                } // if 8.
            } // for k
        } // of while

        // Creates a new graph from the original vertices and the pred
        // fields
        String theName;
        Graph newGraph = new  Graph();

        // Creates and adds the vertices with the same name
        for ( i=0; i<numvertices; i++ )
        {
            theName = vertices.get( i ).name;
            newGraph.addVertex( new  Vertex().assignName( theName ) );
        }

        Vertex theVertex, thePred;
        Vertex theNewVertex, theNewPred;
        Vertex theNeighbor;
        boolean flag = false;

        // adds the adjacent Vertices based on the pred fields
        for ( i=0; i<numvertices; i++ )
        {
            // theVertex and its predecessor
            theVertex = vertices.get( i );
            thePred = findsVertex( theVertex.predecessor );

            // if theVertex is the source then continue we dont need
            // to create a new neighbor
            if ( thePred==null )
                continue;

            // Find the references in the new Graph
            theNewVertex = newGraph.findsVertex( theVertex.name );
            theNewPred = newGraph.findsVertex( thePred.name );

            // Creates the new neighbor in predecessor -> vertex in the new
            // Graph and adjusts the adorns based on the old edge
            ( theNewPred.adjacentVertices ).add( theNewVertex );

            // the adjacent corresponds to the neighbor formed with
            // theVertex -> thePred
            // find the corresponding neighbor of the Vertex, that is,
            // predecessor
            j=0;
            flag=false;
            do
            {
                theNeighbor = ( thePred.adjacentVertices ).get( j );
                if ( theNeighbor.name.equals( theVertex.name ) )
                    flag = true;
                else
                    j++;
            }
            while( flag==false && j < thePred.adjacentVertices.size() );

            // What remains to be done is to adjust the adorns
            // by copying  the values that are embedded in
            // theNeighbor object
            theNewPred.adjustAdorns( thePred, j );

        } // all the vertices

        return newGraph;

    }
	/*@(Benchmark)*/


    public Reader inFile;
    /*@(Benchmark)*/
    // File handler for reading
    public static int ch;
	/*@(Benchmark)*/
    // Character to read/write

    // timmings
//	static long last=0, current=0, accum=0;
	/*@(Benchmark)*/


    public void runBenchmark( String FileName ) throws IOException
    {
        File f = new File(getClass().getResource("/"+FileName).getFile());
        System.out.println("FILE? " + f.exists());
        System.out.println(f.getAbsolutePath());
        inFile = new FileReader(f);
        Main.splPrint___("creating inFile");
    }
	/*@(Benchmark)*/


    public void stopBenchmark() throws IOException
    {
        inFile.close();
    }
	/*@(Benchmark)*/


    public int readNumber() throws IOException
    {
        int index =0;
        char[] word = new char[80];
        int ch=0;

        ch = inFile.read();
        while( ch==32 )
            ch = inFile.read(); // skips extra whitespaces

        // while it is not EOF, WS, NL
        while( ch!=-1 && ch!=32 && ch!=10 )
        {
            word[index++] = ( char )ch;
            ch = inFile.read();
        }
        word[index]=0;

        String theString = new String( word );

        theString = new String( theString.substring( 0,index ) ).trim();
        return Integer.parseInt( theString,10 );
    }
	/*@(Benchmark)*/


    public static void startProfile()
    {
//		accum = 0;
//		current = System.currentTimeMillis();
//		last = current;
    }
	/*@(Benchmark)*/


    public static void stopProfile()
    {
//		current = System.currentTimeMillis();
//		accum = accum + Math.max(current - last, 1);
    }
	/*@(Benchmark)*/


    public static void resumeProfile()
    {
//		current = System.currentTimeMillis();
//		last = current;
    }
	/*@(Benchmark)*/


    public static void endProfile()
    {
//		current = System.currentTimeMillis();
//		accum = accum + ( current-last );
//		Main.splPrint___("Time elapsed: " + accum);
    }
}
