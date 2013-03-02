/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#ifndef CFD_OBJECTS_H
#define CFD_OBJECTS_H

#include <ves/xplorer/GlobalBase.h>

#include <ves/xplorer/scenegraph/Geode.h>

#include <propertystore/PropertySetPtr.h>

#include <latticefx/core/vtk/DataSet.h>

#include <latticefx/core/DataSet.h>

#include <vector>

// VTK Classes
class vtkPolyData;
class vtkActor;
class vtkCompositeDataGeometryFilter;
class vtkGeometryFilter;
class vtkAlgorithmOutput;
class vtkDataSetSurfaceFilter;

namespace ves
{
namespace xplorer
{
/*!\file cfdObjects.h
 *   cfdObjects API
 * \class ves::xplorer::cfdObjects
 *
 */
class VE_XPLORER_EXPORTS cfdObjects : public GlobalBase
{
public:

    ///copy constructor.
    ///\param &src
    cfdObjects( const cfdObjects& src );

    ///Constructor.
    cfdObjects( void );

    ///Destructor.
    virtual ~cfdObjects( void );

    // pure virtual functions to be specified in concrete implementations

    ///Process the VECommand from Conductor
    virtual void UpdateCommand( void );

    ///update the actor
    virtual void Update() = 0;

    ///Create a copy of this object
    virtual cfdObjects* CreateCopy() = 0;

    ///Update the actors in the object
    void UpdateActors();

    ///Create a vtkPolyData based on the input vtkPolyDataAlgorithm\n
    ///and the current dataset type
    ///\param input The input vtkPolyDataAlgorithm to process
    vtkAlgorithmOutput* ApplyGeometryFilterNew( vtkAlgorithmOutput* input );

    ///Returnd geodes.
    std::vector< osg::ref_ptr< ves::xplorer::scenegraph::Geode > > GetGeodes( void );

    ///Clear the geodes.
    void ClearGeodes( void );

    ///Sets the object type.
    ///\param type
    void SetObjectType( int type );

    ///Returns object type.
    int GetObjectType( void )
    {
        return this->objectType;
    }

    ///Set the origin.
    ///\param o
    void SetOrigin( float o[3] );

    ///returns the origin.
    double* GetOrigin();

    ///Gets the origin values.
    ///\param o
    void GetOrigin( double o[3] );

    ///Sets the normals.
    ///\param n
    void SetNormal( double n[3] );

    ///Sets the size of the box.
    ///\param b
    void SetBoxSize( double b[6] );


    ///Sets and stores the requested value.
    ///\param x
    void SetRequestedValue( double x )
    {
        requestedValue = x;
    }

    ///Sets and stores the cursor type.
    ///\param x
    void SetCursorType( int x )
    {
        this->cursorType = x;
    }

    ///Sets the use for the precalculated data.
    ///\param x
    void SetPreCalcFlag( int x )
    {
        this->usePreCalcData = x;
    }

    ///Sets the update flag.
    ///\param x
    void SetUpdateFlag( bool x )
    {
        this->updateFlag = x;
    }

    ///Gets the update flag.
    ///\param x
    bool GetUpdateFlag( void )
    {
        return ( this->updateFlag );
    }

    ///Deletes the geode.
    void DeleteGeode( void );

    ///Sets the source points.
    ///\param pointSource
    void SetSourcePoints( vtkPolyData* pointSource );


    ///Adds another geode to the sequence.
    void AddGeodesToSequence( void );

    ///Sets a flag for the geode.
    ///\param x
    void SetGeodeFlag( bool x );

    ///Flag for the geode flag.
    bool GetGeodeFlag( void );

    ///Flag for transient geode.
    bool GetTransientGeodeFlag( void );

    ///Sets flag for transient geode
    void SetTransientGeodeFlag( bool x );

    ///Selects the active dataset.
    ///\param dataset
    void SetActiveDataSet( lfx::core::vtk::DataSetPtr dataset );

    ///Gets the active dataset.
    lfx::core::vtk::DataSetPtr GetActiveDataSet();

    ///Get gpu tools active
    bool IsGPUTools();

    ///Set the uui for the current feature
    void SetUUID( std::string const& uuid );

    ///Set the uui for the current feature
    std::string const& GetUUID() const;

    ///Set the property set for this feature
    void SetPropertySet( propertystore::PropertySetPtr set );

    ///Set the surface dataset name
    void SetDataMapSurfaceName( std::string const& surfName );

    ///Get the lfx dataset ptr
    lfx::core::DataSetPtr GetLFXDataSet() const;

protected:
    vtkPolyData* ComputeVolumeFlux( vtkPolyData* inputPD );
    ///active dataset.
    lfx::core::vtk::DataSetPtr activeDataSet;
    ///The current property set
    propertystore::PropertySetPtr m_propertySet;
    ///geode vector.
    std::vector< osg::ref_ptr< ves::xplorer::scenegraph::Geode > > geodes;
    ///point source for vtk polydata.
    vtkPolyData* pointSource;
    ///vtkMultiGroupGeometryFilter
    vtkCompositeDataGeometryFilter* m_multiGroupGeomFilter;
    ///vtkGeometryFilter
    vtkGeometryFilter* m_geometryFilter;

    vtkDataSetSurfaceFilter* m_surfaceFilter;
    ///The pointer to the lfx dataset that contains the rtp operation and scene data
    lfx::core::DataSetPtr m_dsp;
    
    bool updateFlag;///<flag for updating.
    int vtkToPFDebug;///<debugging for performer (may not be needed).
    int objectType;///<sets object type.
    double requestedValue;///flag for requested value.
    int cursorType;///<flag for cursor type.
    int usePreCalcData;///<flag for using the precalculated data.
    double origin[ 3 ];///<stores origin values.
    double center[ 3 ];///<stores center values.
    double normal[ 3 ];///<stores normal values.
    double box_size[ 6 ];///<stores size of box.
    ///Used to control wether GPU tools are used
    bool m_gpuTools;
    ///Used to control wether SURF tools are used
    std::string m_surfDataset;
    ///The uuid for the current feature
    std::string m_uuid;

private:
};
}
}
#endif
