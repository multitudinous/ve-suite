/*
    Author: E. David Huckaby
    Org:    NETL
            huckaby@netl.doe.gov
    Date:   3/25/2004
*/
#ifndef FLUENT_CASE
#define FLUENT_CASE
#include <vector>
#include <string>
#include "fluentObjects.h"
#include "fluentIO.h"

namespace FluentReader {

class Case
{
    public:

        Case(FluentIO *file) { m_isConnectFace = true; m_isConnectLocal = true;                        
                               m_file = file ; }
        void textDump( std::string filename );
        int getKeyword();        
        int readSection();
        void read();

        enum sectionTypes { 
            COMMENT = 0, DIMENSION = 2, MACHINE_CONFIG = 4,
            NODE = 10, CELL = 12, FACE = 13,
            PERIODIC_FACE = 18, 
            GRID_SIZE = 33, RP_VAR = 37, CX_VAR = 38, RP_TV = 39,
            PARTITION = 40, NODE_FLAG = 41, 
            CELL_TREE = 58, FACE_TREE = 59, FACE_PARENTS = 61,
            RF_SEG_DATA = 300, RF_SEG_RESIDUALS };
           

        /*  base Fluent file has face-to-cell connectivity
             this help build  cell-to-node or 
        /                   cell-to-face connectivity */
        void calcCellConnectivity();
        void switchGlobalToLocal();
        void switchLocalToGlobal();
        void toGlobalFaceConnect();
        void toLocalFaceConnect();
        void toGlobalCellConnect();
        void toLocalCellConnect();
        void switchCellToFace();
        void switchFaceToCell();

        int nCells(){ return m_nCells; }
        int nFaces(){ return m_nFaces; }
        int nNodes(){ return m_nNodes; }
        int nCellTree(){ return m_cellTree.size(); }
        
        FaceThread *faceThread(int i){ return &m_faceThread[i]; }
        CellThread *cellThread(int i){ return &m_cellThread[i]; }
        NodeThread *nodeThread(int i){ return &m_nodeThread[i]; }
        SegData *dataThread(int i){ return &m_segData[i]; }
	     ElementTree *cellTree(int i){ return &m_cellTree[i]; } 

        FaceThread *face(){ return m_globalFace; }
        CellThread *cell(){ return m_globalCell; }
        NodeThread *node(){ return m_globalNode; }
    
        int nCellThreads(){ return m_cellThread.size(); }
        int nFaceThreads(){ return m_faceThread.size(); }
        int nNodeThreads(){ return m_nodeThread.size(); }
        int nDataThreads(){ return m_segData.size(); }

        void Case::findGlobalThreads();
      
    private:
        
        std::vector<Comment> m_comment;
        std::vector<Header> m_header;
        int m_dimension;
        std::vector<NodeThread> m_nodeThread;   
        std::vector<PeriodicFace> m_periodicFace;
        std::vector<CellThread> m_cellThread;
        std::vector<FaceThread> m_faceThread;
        std::vector<ElementTree> m_faceTree;
        std::vector<ElementTree> m_cellTree;
        std::vector<FaceParents> m_faceParents; 
        std::vector<NodeFlag> m_nodeFlag;
        std::vector<VariableTag> m_variableTag;
        std::vector<Partition> m_partition;
        std::vector<GridSize> m_gridSize;
        std::vector<SegData> m_segData;
        std::vector<SegResiduals> m_segResiduals;
        FluentIO *m_file;
        int m_nDims;
        int m_nFaces;
        int m_nCells;
        int m_nNodes;

        bool m_isConnectFace;     /* face based connectivity */
        bool m_isConnectLocal;    /* connectivity is stored with threads */
        //bool m_isConnectCell;     /* cell based connectivity is stored */
        //bool m_isConnectGlobal;   /* global */  

        /* might be better to just use an index */
        CellThread *m_globalCell; /* pointer to cell Thread with a zone-ID of zero (global thread) */
        FaceThread *m_globalFace; /* pointer to face Thread with a zone-ID of zero (global thread) */
        NodeThread *m_globalNode;
};

} /* end namespace FluentReader */

#endif
