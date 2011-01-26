package abs.frontend.delta;

import java.util.*;

import abs.frontend.delta.exceptions.VertexNotFoundException;

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 1/19/11
 * Time: 10:17 AM
 * Adapted from http://www.java2s.com/Code/Java/Collections-Data-Structure/Topologicalsorting.htm
 */


class Vertex {
    public Object label;

    public Vertex(Object lab) {
        label = lab;
    }
}

public class GraphTS {
//  private final int MAX_VERTS = 20;

    private Vertex vertexList[]; // list of vertices

    private int matrix[][]; // adjacency matrix

    private int numVerts; // current number of vertices

    private Map<Object,Integer> indexes = new HashMap<Object,Integer>();

    private boolean acyclic = false;

    private Object sortedArray[];

    public GraphTS(int MAX_VERTS) {
        vertexList = new Vertex[MAX_VERTS];
        matrix = new int[MAX_VERTS][MAX_VERTS];
        numVerts = 0;
        for (int i = 0; i < MAX_VERTS; i++)
            for (int k = 0; k < MAX_VERTS; k++)
                matrix[i][k] = 0;
        sortedArray = new Object[MAX_VERTS]; // sorted vert labels
    }

    public GraphTS (Object[] elems) {
        int MAX_VERTS = elems.length;
        vertexList = new Vertex[MAX_VERTS];
        matrix = new int[MAX_VERTS][MAX_VERTS];
        numVerts = 0;
        for (int i = 0; i < MAX_VERTS; i++)
            for (int k = 0; k < MAX_VERTS; k++)
                matrix[i][k] = 0;
        sortedArray = elems; // sorted vert labels

        for (Object e : elems) addVertex(e);
    }

    public void addVertex(Object lab) {
        indexes.put(lab,numVerts);
        vertexList[numVerts++] = new Vertex(lab);
    }

    public void addEdge(int start, int end) {
        matrix[start][end] = 1;
    }

    public void addEdge(Object start, Object end) throws VertexNotFoundException {
        if (!indexes.containsKey(start))
            throw new VertexNotFoundException(start.toString());
        matrix[indexes.get(start)][indexes.get(end)] = 1;
    }

    public void displayVertex(int v) {
        System.out.print(vertexList[v].label);
    }

    public void displaySortedArray() {
        if (!acyclic)
            System.out.print("Acyclic graph not found.");
        else {
            System.out.print("Topologically sorted order: ");
            for (int j = 0; j < sortedArray.length; j++)
                System.out.print(sortedArray[j]);
            System.out.println("");
        }
    }

    public Object[] getSortedArray() {
        return sortedArray;
    }

    public boolean topo() // toplogical sort
    {
//    int orig_nVerts = numVerts;

        while (numVerts > 0) // while vertices remain,
        {
            // get a vertex with no successors, or -1
            int currentVertex = noSuccessors();
            if (currentVertex == -1) // must be a cycle
            {
                return false;
            }
            // insert vertex label in sorted array (start at end)
            sortedArray[numVerts - 1] = vertexList[currentVertex].label;

            deleteVertex(currentVertex); // delete vertex
        }

        // vertices all gone; display sortedArray
        acyclic = true;
        return true;
    }

    public int noSuccessors() // returns vert with no successors (or -1 if no such verts)
    {
        boolean isEdge; // edge from row to column in adjMat

        for (int row = 0; row < numVerts; row++) {
            isEdge = false; // check edges
            for (int col = 0; col < numVerts; col++) {
                if (matrix[row][col] > 0) // if edge to another,
                {
                    isEdge = true;
                    break; // this vertex has a successor try another
                }
            }
            if (!isEdge) // if no edges, has no successors
                return row;
        }
        return -1; // no
    }

    public void deleteVertex(int delVert) {
        if (delVert != numVerts - 1) // if not last vertex, delete from vertexList
        {
            for (int j = delVert; j < numVerts - 1; j++)
                vertexList[j] = vertexList[j + 1];

            for (int row = delVert; row < numVerts - 1; row++)
                moveRowUp(row, numVerts);

            for (int col = delVert; col < numVerts - 1; col++)
                moveColLeft(col, numVerts - 1);
        }
        numVerts--; // one less vertex
    }

    private void moveRowUp(int row, int length) {
        for (int col = 0; col < length; col++)
            matrix[row][col] = matrix[row + 1][col];
    }

    private void moveColLeft(int col, int length) {
        for (int row = 0; row < length; row++)
            matrix[row][col] = matrix[row][col + 1];
    }

//  public static void othertest(String[] args) {
//    GraphTS g = new GraphTS(20);
//    g.addVertex('A'); // 0
//    g.addVertex('B'); // 1
//    g.addVertex('C'); // 2
//    g.addVertex('D'); // 3
//    g.addVertex('E'); // 4
//    g.addVertex('F'); // 5
//    g.addVertex('G'); // 6
//    g.addVertex('H'); // 7
//
//    g.addEdge(0, 3); // AD
//    g.addEdge(0, 4); // AE
//    g.addEdge(1, 4); // BE
//    g.addEdge(2, 5); // CF
//    g.addEdge(3, 6); // DG
//    g.addEdge(4, 6); // EG
//    g.addEdge(5, 7); // FH
//    g.addEdge(6, 7); // GH
//
//    g.topo(); // do the sort
//    g.displaySortedArray();
//  }
//
//  public static void main(String[] args) throws VertexNotFoundException {
//    String[] tosort = {"a","b","c","d"} ;
//
//    GraphTS g = new GraphTS(tosort);
//    g.addEdge(0,1); g.addEdge(0,2);  // a <- b c
//    g.addEdge(1,2); g.addEdge(1,3);  // b <- c d
//    g.addEdge("dd","c");             // d <- c
//
//    boolean success = g.topo();
//
//    System.out.println("Topological sort: "+success);
//    for (String s : tosort) {
//      System.out.print(s + ", ");
//    }
//    System.out.println("");
//    g.displaySortedArray();
//  }
}
