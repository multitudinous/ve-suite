#ifndef FLUENT_VTKFACTORY
#define FLUENT_VTKFACTORY

#include "vtk/vtkFloatArray.h"
#include "vtk/vtkDoubleArray.h"
#include "vtk/vtkPoints.h"
#include "vtk/vtkUnstructuredGrid.h"
#include "fluentObjects.h"
#include "fluentCase.h"

namespace FluentReader {

/*
    VtkFactory -
        provides a means to extract grid and scalar data from fluent case (m_case) 
        and data (m_data) files into a vtk unstructgrid (m_data) objects with scalars.        


*/

class VtkFactory {

    
    public:

        VtkFactory();
        VtkFactory(Case *fcase, Case *fdata);
                     
        void addPoints();           /* extract grid points from fluent to the vtk grid */
        // these assume a local-to-global connectivity call
        void addCellConnect();      /* extract cell connectivity from fluent to the vtk grid */
        void addFaceConnect();      /* extract face connectivity from to the vtk grid (not implemented) */

        /* adds a scalar of name with fluent variable index, "var_id" to the vtk grid using the
            name, "name" */
        void addScalar(int var_id, std::string name); 
  
        void addScalars(std::vector<int> var_ids, std::vector<std::string> names){
            for (int i = 0; i < var_ids.size(); i++ )
                addScalar( var_ids[i], names[i] ); }
                
        /* takes a list of scalars and makes them into a vtk vector 
        only the first 3 are used to make the vector since vtk vectors are 3 dimensional.
        If less than 3 scalars are in the list, 0.0 is used for the remaining components
        */
        void addVector(std::vector<int> var_ids, std::string name);

        /* takes a list of scalars(vectors) add them as a set of vtk-scalars with a single name */
        void addVectorAsScalar(std::vector<int> var_ids, std::string name);
   
        /* takes a list of scalars add makes tensor object (not implemented) */
        void addTensor(std::vector<int> var_ids, int n, std::string name);

        /* adds an integer scalar to the vtk dataset which indicates whether the cell has
        child cells, this occurs when the original grid has been subdivided one-or more times
        within the fluent solver 
            0 - not a parernt - indicates the cell has not been subdivided
                (does not have any children)
            1 - parent - cell has children

        this is required because solutions are only written to the child cells
        */
        void addParentFlag();
        
        void setCase(Case *fcase){ m_case = fcase; }
        void setData(Case *fdata){ m_data = fdata; }

        vtkUnstructuredGrid* vtkGrid(){ return m_grid;  }
        Case* fluentCase(){ return m_data; }
        Case* fluentData(){ return m_case;  }

        /* writes the created vtk object to a file */
        void toFile( std::string filename, bool isXML = false, bool isBinary = true);

        void enableOutput(){ m_verbose = true; }
        void disableOutput(){ m_verbose = false; }

        /* I don't think there is a need to call these outside the class */
        void nodeToVTK( NodeThread *node, vtkPoints *points, int firstNode );
        void faceToVTK( FaceThread *face );
        void cellToVTK( CellThread *in_cell );
        void dataToVTK( CellThread *cell_thread, SegData *seg_data, 
            vtkDoubleArray *scalar, int var_id, int iVar = 0);
       
    private:

        Case *m_case;
        Case *m_data;
        bool m_verbose;
        vtkUnstructuredGrid *m_grid;
        vtkUnstructuredGrid *m_faceGrid;
        
}; /* end VtkFactory */

} /* end namespace fluent */

#endif
