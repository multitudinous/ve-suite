/*
    Author: E. David Huckaby
    Org:    NETL
            huckaby@netl.doe.gov
    Date:   3/25/2004
*/
#ifndef _REORDER
#define _REORDER

namespace FluentReader {

int reorderNodes( FaceThread *face, PosVectorList node_pos, int face_id);
void reorderFace_3D( CellThread *cell, FaceThread *face, int cell_id );
void reorderNode_3D( CellThread *cell, FaceThread *face, int cell_id );
void reorderFace_2D( CellThread *cell, FaceThread *face, int cell_id );

}

#endif
