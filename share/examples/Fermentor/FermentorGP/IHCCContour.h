#ifndef IHCC_CONTOUR_H
#define IHCC_CONTOUR_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/event/viz/cfdObjects.h>

// --- VTK Includes --- //
class vtkLookupTable;
class vtkPolyData;
class vtkPolyDataMapper;
class vtkActor;

// --- C/C++ Libraries --- //
#include <vector>

class IHCCContour : public ves::xplorer::cfdObjects
{
public:
    IHCCContour();

    ~IHCCContour();

    void RunModel();
    void UpdateModelVariables( double* );
    void MakeLookupTable();
    void MakeSequence();
    void Update();
    void Update_One_Circle();
    void SetDataVector( std::vector< double >, double* x );
    double variables[ 6 ];

    double min, max;
    vtkLookupTable* lut;
    vtkPolyData* pData;
    vtkPolyDataMapper* mapper;
    vtkActor* actor;
    double definedRange[ 2 ];
    std::vector< double > solutions;
};

#endif // IHCC_CONTOUR_H
