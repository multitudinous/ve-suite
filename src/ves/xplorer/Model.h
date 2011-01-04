/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#ifndef VE_XPLORER_MODEL_H
#define VE_XPLORER_MODEL_H

#include <ves/xplorer/ModelPtr.h>
#include <ves/xplorer/GlobalBase.h>

#include <ves/xplorer/ModelHandlerPtr.h>
#include <ves/xplorer/ModelCADHandlerPtr.h>
#include <ves/xplorer/DataSetPtr.h>

//#include <ves/xplorer/environment/cfdSound.h>

#include <ves/xplorer/event/EventHandlerPtr.h>

#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/Group.h>
#include <ves/xplorer/scenegraph/Switch.h>

#include <ves/open/xml/model/ModelPtr.h>

#include <vrj/vrjParam.h>

#include <vpr/Sync/Mutex.h>
#include <vpr/Thread/Thread.h>

#include <osg/ref_ptr>
#include <osg/StateSet>

namespace ves
{
namespace xplorer
{
namespace volume
{
class cfdTextureDataSet;
}
}
}

#include <string>
#include <vector>
#include <map>
#include <utility>

namespace ves
{
namespace xplorer
{
class ModelDatasetHandler;
namespace scenegraph
{
class DCS;
class Group;
class Switch;
class CADEntity;
class CADEntityHelper;
class Clone;
class fileInfo;
class TextTexture;
}
}
}

class vtkDataSet;
class vtkUnstructuredGrid;

enum ModelTypeIndex
{
    SimulationModel = 1,
    ExperimentalModel,
    DesignModel,
    GeometricalModel
};

enum Operation2Model
{
    AddVTKdataset = 1,
    DelVTKdataset,
    DeleteVTKdataset,
    AddGeodataset,
    DelGeodataset,
    DeleteGeomdataset
};

namespace ves
{
namespace xplorer
{
/*!\file Model.h
Model API
*/
/*!\class ves::xplorer::Model
*
*/

/*
1.The difference between the concept of multiple models and multiple datasets
is that:

A model can contain several vtkDataSets, geometricalDataSets (surface dataset).
These datasets have the same operations. For example, for the same shape design,
if we want to see the effects of different boundary condition, we can put two
different cases into the same model, so the comparision can be made very
easily.But if we want to see the two different shape design or need to see
the difference between the experiment results and the simulation results,
it is better to treat these two dataset as two different models.
*/
class VE_XPLORER_EXPORTS Model : public GlobalBase
{
public:
    Model( ves::xplorer::scenegraph::DCS* );
    virtual ~Model();

    ///PreFrame callback to update the model based on commands from
    ///VE-Conductor
    void PreFrameUpdate();

    ///in future, multi-threaded apps will make a copy of VjObs_i commandArray
    virtual void UpdateCommand()
    {;}

    void setModelNode( ves::xplorer::scenegraph::CADEntityHelper * );
    ///four type models right now (experiment, simulation, design, and geometry)
    void setModelType( ModelTypeIndex );
    bool GetMirrorDataFlag( void );
    void SetMirrorDataFlag( bool );
    void SetMirrorNode( ves::xplorer::scenegraph::Group* );

    ves::xplorer::DataSet* GetCfdDataSet( int );
    unsigned int GetIndexOfDataSet( std::string dataSetName );
    unsigned int GetNumberOfCfdDataSets( void );
    std::string GetCfdDataSetFileName( int );
    void CreateCfdDataSet( void );
    int GetKeyForCfdDataSet( DataSet* );
    ves::xplorer::DataSet* GetActiveDataSet( void );
    void SetActiveDataSet( ves::xplorer::DataSet* );
    ///Delete the named dataset
    void DeleteDataSet( std::string dataSetName );


    ves::xplorer::scenegraph::CADEntity* GetGeomDataSet( int );
    unsigned int GetNumberOfGeomDataSets( void );
    std::string GetGeomFileName( int );
    void CreateGeomDataSet( std::string );

    ///Get the CADModelHandler for manipulation/management\n
    ///of CADNode s
    ///\return ModelCADHandler
    ves::xplorer::ModelCADHandler* GetModelCADHandler();

    ///Get the dcs for the CAD and the data viz to be added to
    ///All plugins have only one model and thus have one DCS
    ///\return The plugin DCS that everything will be added to
    ves::xplorer::scenegraph::DCS* GetDCS( void );

    ///Set the id for this model
    ///\param id the id of the model to be set
    void SetID( const std::string& id );

    ///Get the id for this model
    const std::string& GetID();

    ///Add a new sound to the model
    ///\param soundName The name of the sound
    ///\param filename The actual filename of the sound to load
    void AddNewSound( std::string soundName, std::string filename );

    ///Activate (play) the sound
    ///\param soundName The name of the sound to play
    void ActivateSound( std::string soundName );

    ///Deactivate (stop) the sound
    ///\param soundName The name of the sound to stop
    void DeactivateSound( std::string soundName );

    ///Render a display for this model
    ///\param onOff Tell the model to render its textual display
    void RenderTextualDisplay( bool onOff );

    //////////////////////////
    //texture based interface
    void SetActiveTextureDataSet( ves::xplorer::volume::cfdTextureDataSet* tDS );
    void CreateTextureDataSet();
    void AddDataSetToTextureDataSet( unsigned int index,
                                     std::string textureDescriptionFile );
    unsigned int GetNumberOfTextureDataSets();
    ves::xplorer::volume::cfdTextureDataSet* GetTextureDataSet( unsigned int index );
    ves::xplorer::volume::cfdTextureDataSet* GetActiveTextureDataSet();
    ///////////////////////////////////////////////////
    ///Get the VE-Open Model for this Model
    ves::open::xml::model::ModelPtr GetModelData();

    ///Set the VE-Open Model for this Model
    void SetModelData( ves::open::xml::model::ModelPtr tempModelData );
    ///////////////////////////////////////////////////

    //Dynamically load data from unit
    void ActiveLoadingThread();

    void GetDataFromUnit( void );

    const std::string MakeSurfaceFile( vtkDataSet*, int );
    void DynamicLoadingData( vtkUnstructuredGrid*, int, double*, double*, double* );
    void DynamicLoadingGeom( std::string, double*, double*, double*, double*, int, int );
    void AddVTKDataSet( vtkDataSet* );
    std::vector<vtkDataSet* >GetWaitingDataList();

    ///Get the dataset handler for this model
    ves::xplorer::ModelDatasetHandler* GetModelDatasetHandler();

private:
    vpr::Thread *loadDataTh;
    vpr::Mutex mValueLock;
    std::vector<vtkDataSet* > waitingdatalist;
    std::string currentsurfacefilename;
    bool mirrorDataFlag;

    //ves::xplorer::scenegraph::cfdTempAnimation* animation;
    osg::ref_ptr< ves::xplorer::scenegraph::Switch > switchNode;
    osg::ref_ptr< ves::xplorer::scenegraph::Group > classic;
    osg::ref_ptr< ves::xplorer::scenegraph::Group > textureBased;
    typedef std::vector< ves::xplorer::scenegraph::CADEntity* > GeometoryDataSetList;
    GeometoryDataSetList mGeomDataSets;
    typedef std::vector< ves::xplorer::DataSet* > VTKDataSetList;
    VTKDataSetList mVTKDataSets;

    typedef std::vector<ves::xplorer::volume::cfdTextureDataSet*> TextureDataSetList;
    TextureDataSetList mTextureDataSets;
    ves::xplorer::volume::cfdTextureDataSet* _activeTextureDataSet;

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > _worldDCS;

    DataSet* activeDataSet;
    ves::xplorer::scenegraph::Clone* mirrorNode;
    osg::ref_ptr< ves::xplorer::scenegraph::Group > mirrorGroupNode;

    //the information for following three variables should be transfered from cfdApp
    ModelTypeIndex mModelType;
    Operation2Model mActiveOperation2Model;

    bool mUpdateModelFlag;
    bool mMoveOldGeomDataSets;
    bool mMoveOldVTKDataSets;

    /*std::string rootCADNodeID;///<ID for root CAD node id*/
    std::string modelID;


    //std::map<std::string, cfdSound> _availableSounds;///<The available sounds for this model.
    std::map< std::string, std::vector< std::pair< std::string, osg::ref_ptr< osg::StateSet > > > > _nodeAttributes;///<The map of node attributes.

    ///The CADHandler for this model.
    ves::xplorer::ModelCADHandler* m_cadHandler;
    ///The Dataset Handler for this model.
    ves::xplorer::ModelDatasetHandler* m_datasetHandler;
    ///Text texture
    osg::ref_ptr< ves::xplorer::scenegraph::TextTexture > mModelText;
    ///The VE-Open Model data for this model
    ves::open::xml::model::ModelPtr m_modelVEOpenData;
};
}
}
#endif
