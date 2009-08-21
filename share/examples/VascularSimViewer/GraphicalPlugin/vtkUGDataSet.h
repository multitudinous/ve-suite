/*
This class combines the XMLUnstructured Grid Reader
with the actual Unstructured Grid

Also keeps track of the visualizations created from this
data such that when the data set is delated all derived
visualizations witll also be deleted
*/

#ifndef VTK_U_G_DATA_SET_H
#define VTK_U_G_DATA_SET_H

// --- C++ Includes --- ///
#include <string>
#include <vector>

// --- vtk Forward Declarations --- //
class vtkXMLUnstructuredGridReader;
class vtkDataArraySelection;
class vtkUnstructuredGrid;

namespace vascularsimviewer
{
class PolyVisualizationBase;

class vtkUGDataSet
{
public:
    vtkUGDataSet( const std::string& filename, std::vector< std::string > arrays );

    ~vtkUGDataSet();
    
    const std::string& GetFilename();
    
    void UpdateArrays( std::vector< std::string > array );
    
    void AddVisualization( PolyVisualizationBase* inPolyViz );

    vtkUnstructuredGrid* GetUGData();

private:

    std::string mFilename;

    vtkXMLUnstructuredGridReader* _reader;
    
    vtkDataArraySelection* _ArraySelector;
    
    std::vector< std::string > _activeArrays;
    
    //The Unstructured Grid Data
    vtkUnstructuredGrid* _UGData;
    
    std::vector< PolyVisualizationBase* > _derivedVisualizations;
    
    int _numArrays;
};

} //end vascularsimviewer

#endif // VTK_U_G_DATA_SET_H
