/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef VE_XPLORER_CFD_DATA_SET_H
#define VE_XPLORER_CFD_DATA_SET_H

#include <ves/VEConfig.h>
#include <ves/xplorer/DataSetPtr.h>

#include <ves/xplorer/util/DataObjectHandler.h>

#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/Group.h>
#include <ves/xplorer/scenegraph/Switch.h>
#include <ves/xplorer/scenegraph/Geode.h>

#include <osg/ref_ptr>

#ifdef USE_OMP
#define MAX_DATA 20
#endif

#include <string>
#include <map>
#include <vector>

class vtkLookupTable;
class vtkPolyData;
class vtkUnstructuredGrid;
class vtkUnstructuredGridReader;
class vtkDataSet;
class vtkDataObject;

namespace ves
{
namespace xplorer
{
class cfdPlanes;
class DataSetAxis;
class DataSetScalarBar;
}
}

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class DCS;
class Group;
class Switch;
class Geode;
}
}
}

namespace ves
{
namespace xplorer
{
namespace util
{
class cfdVTKFileHandler;
}
}
}

namespace ves
{
namespace builder
{
namespace DataLoader
{
class DataLoader;
}
}
}

namespace ves
{
namespace xplorer
{
/*!\file DataSet.h
DataSet API
*/
/*!\class ves::xplorer::DataSet
* A class to load data set and pre-compute flow parameters
* or properties for virtual environment interactive
* computation.
*/
class VE_XPLORER_EXPORTS DataSet
{
public:
    DataSet();    // Construct vtkUnstructuredGrid and vtkLookupTable objects.

    ~DataSet();   // Destruct vtkUnstructuredGrid and vtkLookupTable objects.

    // Initialize the number of data to load and parallel process.
    // By default, use the octree table.
    void LoadData( const std::string fileName );
    void LoadData( vtkUnstructuredGrid*, int );
    void LoadData();
    ///Load the precomputed data directory
    void LoadPrecomputedDataSlices();


    // Set/get the range of velocity based on the data set.
    void SetRange( double dataRange[2] );
    void SetRange( double dataMin, double dataMax );
    void GetRange( double dataRange[2] );
    void GetRange( double &dataMin, double &dataMax );
    double * GetRange();

    // Set/get the min/max velocity, used defined.
    void SetUserRange( double userRange[2] );
    void SetUserRange( double userMin, double userMax );
    void GetUserRange( double userRange[2] );
    void GetUserRange( double &userMin, double &userMax );
    double * GetUserRange();

    // Set/get the length of the diagonal of the bounding box for data set.
    void SetLength( float len );
    //void GetLength( float &len );
    //float GetLength();

    // Get the length of the diagonal of the bounding box of the average cell
    //void GetMeanCellLength( float &len );
    //float GetMeanCellLength();

    // Set/get the step length for streamline integration.
    void SetStepLength( float sLen );
    void GetStepLength( float &sLen );
    float GetStepLength();

    // Set/get the maximum streamline integration time.
    void SetMaxTime( float mT );
    void GetMaxTime( float &mT );
    float GetMaxTime();

    // Set/get time step for streamline integration
    void SetTimeStep( float tStep );
    void GetTimeStep( float &tStep );
    float GetTimeStep();

    // Get the vtk look up table.
    vtkLookupTable * GetLookupTable();

    // Get the single piece original data.
    vtkUnstructuredGrid * GetUnsData();
    vtkPolyData * GetPolyData();
    vtkDataObject * GetDataSet();

    void SetType();       // compute dataset type by looking at the file
    void SetType( int );  // manually set the dataset type
    int GetType();        // get the dataset type

    // SetActiveScalar and compute the actual scalar range and the pretty range for display purposes
    // 0 <= activeScalar < numScalars
    void SetActiveScalar( int );
    void SetActiveScalar( std::string scalarName );
    int GetActiveScalar();

    void SetActiveVector( int );
    void SetActiveVector( std::string vectorName );
    int GetActiveVector();

    // Update the geometrical properties of the mesh
    void UpdatePropertiesForNewMesh();

    static void AutoComputeUserRange( const double rawRange[2],
                                      double prettyRange[2] );

    void ResetScalarBarRange( double min, double max );

    void SetFileName( const std::string filename );
    void SetFileName_OnFly( int );
    std::string GetFileName();

    void SetPrecomputedDataSliceDir( const std::string newDir );
    std::string GetPrecomputedDataSliceDir();

    void SetPrecomputedSurfaceDir( const std::string newDir );
    std::string GetPrecomputedSurfaceDir();

    cfdPlanes * GetPrecomputedXSlices();
    cfdPlanes * GetPrecomputedYSlices();
    cfdPlanes * GetPrecomputedZSlices();
    cfdPlanes * GetPrecomputedSlices( int xyz );

    void StoreScalarInfo();

#ifdef USE_OMP
    vtkUnstructuredGrid * GetData( int i );
    int GetNoOfDataForProcs();       // Set/get number of data for parallel process.
#endif

    void SetArrow( vtkPolyData * );
    vtkPolyData * GetArrow();

    void SetNewlyActivated();
    void SetNotNewlyActivated();
    int IsNewlyActivated();

    int GetNumberOfScalars();
    std::string GetScalarName( int );

    int GetNumberOfVectors();
    std::string GetVectorName( int );

    DataSet * GetParent();
    void SetParent( DataSet * );

    void SetActualScalarRange( int, double * );
    void GetActualScalarRange( int, double * );
    double* GetActualScalarRange( int );
    double* GetActualScalarRange( std::string name );

    // returns displayed range of active scalar
    double * GetDisplayedScalarRange();

    // get/set displayed range of any scalar
    double * GetDisplayedScalarRange( int );
    void SetDisplayedScalarRange( int , double * );

    double * GetVectorMagRange();

    // get/set this dataset's DCS
    ves::xplorer::scenegraph::DCS* GetDCS();
    void SetDCS( ves::xplorer::scenegraph::DCS* );

    ves::xplorer::scenegraph::Switch* GetSwitchNode( void );

    //ves::xplorer::scenegraph::cfdTempAnimation* GetAnimation( void );
    //void SetAnimation( ves::xplorer::scenegraph::cfdTempAnimation* );

    int IsPartOfTransientSeries();
    void SetAsPartOfTransientSeries();

    void Print();
    ///Accessor methods to store and query the uuids for specfic
    ///attributes of a DataSet
    void SetUUID( std::string attribute, std::string uuid );
    std::string GetUUID( std::string attribute );

    ///Create the bbox geode for the dataset
    void CreateBoundingBoxGeode( void );
    ///Create the wireframe geode for the dataset
    void CreateWireframeGeode( void );
    ///Set the bounding box for this dataset
    ///\param state The state of the bounding box 0 or 1
    void SetBoundingBoxState( unsigned int state );
    ///Set the wireframe state for this dataset
    ///\param state The state of the wireframe 0 or 1
    void SetWireframeState( unsigned int state );
    ///Set the axes state for this dataset
    ///\param state The state of the axes state 0 or 1
    void SetAxesState( unsigned int state );
    ///Set the bounding box for this dataset
    ves::xplorer::DataSetAxis* GetDataSetAxes( void );
    ///Set the scalar for this dataset
    ///\param state The state of the scalar bar 0 or 1
    void SetDataSetScalarState( unsigned int state );
    ///Get the scalar bar
    ves::xplorer::DataSetScalarBar* GetDataSetScalarBar( void );

    ///Get the bounds of the vtkDataObject contained in the DataSet
    ///\param bounds xmin,xmax,ymin,ymax,zmin,zmax
    void GetBounds( double bounds[6] );

    ///Get the bounds of the vtkDataObject contained in the DataSet
    ///\param bounds xmin,xmax,ymin,ymax,zmin,zmax
    double* GetBounds();

    ///Get the scalar range by name
    ///\param scalarName The name of the scalar to get the range
    double* GetScalarRange( std::string scalarName );

    ///Get the number of points
    unsigned int GetNumberOfPoints();
private:
    ///Operator callbacks for DataObjectHandler
    std::map<std::string, ves::xplorer::util::DataObjectHandler::DatasetOperatorCallback* > m_dataObjectOps;
    std::map< std::string, std::string > dataSetUUIDMap;

    double** actualScalarRange;
    double** displayedScalarRange;

    DataSet* parent;
    double m_bounds[6];///The bounding box data;
    int isNewlyActivated;

    int CountNumberOfParameters( const int numComponents );
    std::vector<std::string> GetParameterNames( const int numComponents, const int numParameters );

    double* range;          // Range of scalar.

    double* definedRange;   // 'prettied' range of scalar that is automatically computed or user-defined.

    double* vectorMagRange; // assumes just one vector

    float bbDiagonal;        // length of the diagonal of the bounding box.

    double meanCellBBLength; // length of diagonal of average cell bounding box.

    float stepLength;        // Step length for streamline integration.

    float maxTime;           // Maximum time of integration for streamline.

    float timeStep;          // Time step for streamline integration.

    vtkLookupTable* lut;    // Lookup table.

    vtkDataObject* dataSet;    // Original piece of vtk data.
    int datasetType;         // used by gui to place in appropriate column

    int activeScalar;
    int activeVector;

    std::string fileName;
    std::string precomputedDataSliceDir;
    std::string precomputedSurfaceDir;

    cfdPlanes* x_planes;
    cfdPlanes* y_planes;
    cfdPlanes* z_planes;

    vtkPolyData* arrow;

    int numPtDataArrays;
    int numScalars;
    int numVectors;
    std::vector< std::string > scalarName;
    std::vector< std::string > vectorName;

    osg::ref_ptr< ves::xplorer::scenegraph::Geode > wireframeGeode;
    osg::ref_ptr< ves::xplorer::scenegraph::Group > m_visualBBox;

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > dcs;
    osg::ref_ptr< ves::xplorer::scenegraph::Switch > switchNode;
    osg::ref_ptr< ves::xplorer::scenegraph::Group > classic;
    osg::ref_ptr< ves::xplorer::scenegraph::Group > textureBased;

    ves::xplorer::DataSetAxis* dataSetAxes;
    ves::xplorer::DataSetScalarBar* dataSetScalarBar;
    ves::xplorer::util::cfdVTKFileHandler* _vtkFHndlr;
    ves::xplorer::util::DataObjectHandler* m_dataObjectHandler;///<Handle vtkDataObjects
    int partOfTransientSeries;
    ves::builder::DataLoader::DataLoader* m_externalFileLoader;///<Translator interface

#ifdef USE_OMP
    unsigned int noOfData;   // Total no. of octants.
    vtkUnstructuredGridReader *dataReader[MAX_DATA];
    vtkUnstructuredGrid *data[MAX_DATA];
#endif
};
}
}
#endif
