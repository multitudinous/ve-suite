/*
    Author: E. David Huckaby
    Org:    NETL
            huckaby@netl.doe.gov
    Date:   3/25/2004
*/

#ifndef FLUENT_DEFS
#define FLUENT_DEFS

#include "blitz/array.h"
#include <vector>
#include <string>

namespace FluentReader {


typedef blitz::Array<double,2> doubleArray_2;
typedef blitz::Array<double,1> doubleArray_1;
typedef blitz::Array<int,1> intArray_1;
typedef blitz::Array<int,2> intArray_2;
typedef blitz::Array<int,3> intArray_3;

//typedef blitz::Array<int,1> NodeList;
typedef blitz::Array<double,2> NodePositions;
//typedef blitz::Array<int,1> FaceList;
typedef blitz::Array<int,1> faceID_list;
typedef blitz::Array<int,1> nodeID_list;

typedef blitz::Array<double,1> PosVector;
typedef std::vector<int> NodeList;
typedef std::vector<int> FaceList;
typedef std::vector<int> CellList;
typedef std::vector< PosVector > PosVectorList;

extern const int c_cellsPerFace;
extern const int c_leftCell;
extern const int c_rightCell;
extern const int c_nodesPerFace[];
extern const std::string c_faceElementNames[];
extern const int c_nodesPerCell[];
extern const int c_facesPerCell[];
extern const std::string c_cellElementNames[];
extern const int c_parentsPerKid;
extern const int c_maxNodesPerFace;
extern const int c_nodesPerEdge;
int min(int x, int y);

}

#endif
