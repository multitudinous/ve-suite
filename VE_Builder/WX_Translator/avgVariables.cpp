/*
    Author: E. David Huckaby
    Org:    NETL
            huckaby@netl.doe.gov
    Date:   6/18/2004

    Makes x,y,z integrals of the data in the data set
    
*/
#include "fluentIO.h"
#include "fluentCase.h"
#include "fluentObjects.h"
#include "var_defs.h"
#include <string>
#include <vector>


/* should be a member of cell Thread */

/*
    calculate the cell centroid and change (max - min) for each direction
*/
void cell_calcCentroids( CellThread *cell, NodeThread *node,  doubleArray_2 &xbar, doubleArray_2 &delx)
{
    xbar = 0.0;    
    double x[3];
    double xmin[3];
    double xmax[3];
  
    xbar.resize( cell->nCells(), node->nDims() );
    delx.resize( cell->nCells(), node->nDims() );

    for (int i = 0; i < cell->nCells(); i++) {                    
        for (int j = 0; j < cell->nNodes(i); i++ ) {            
            int nodeID = cell->node(i,j);
            node->getCoords( nodeID, x);             
            for (int k = 0; k < node->nDims(); k++) {
                xbar(i,k) += x[k];
                if (j==0) {
                    xmin[k] = x[k];
                    xmax[k] = x[k];
                    }
                else {
                    if (x[k] > xmax[k]) xmax[k] = x[k];
                    if (x[k] < xmin[k]) xmin[k] = x[k];
                    }
                }
            }
        for (int k = 0; k < node->nDims(); k++) {
            delx(i,k) = xmax[k] - xmin[k];
            xbar(i,k) = xbar(i,k)/( (double) cell->nNodes(i) );
            }
        }
}

void avgVariables(
    FluentReader::Case *case1,
    FluentReader::Data *data1,
    std::string avg_file )
{
    /* all nodes are stored in the first thread */
    NodeThread *node_t = Case->NodeThread(0);    

    



}

    



void avgVariables( 
    std::string std::string casefile, 
    std::string datafile, 
    bool isBinary, 
    bool isGzip )
{


    check = casefile + ".check";
    info = casefile + ".info";
    FluentReader::FluentIO * infile1 = new FluentReader::FluentIO(
        casefile, info, check, isBinary, isGzip);

    FluentReader::Case *case1 = new FluentReader::Case(infile1);
    case1->read();
    delete(infile1);

    check = datafile + ".check";
    info = datafile + ".info";
    FluentReader::FluentIO * infile2 = new FluentReader::FluentIO(
        datafile, info, check, isBinary, isGzip);
    FluentReader::Case *data1 = new FluentReader::Case(infile2);
   
    data1->read();
    delete(infile2);
 
    
    avgVariables( case1, data1, "datafile_avg.dat" );

}
