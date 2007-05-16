/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_MODEL_H
#define CFD_MODEL_H
/*!\file cfdModel.h
cfdModel API
*/
/*!\class VE_Xplorer::cfdModel
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
#include "VE_Installer/include/VEConfig.h"
#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"
#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"

#include "VE_Xplorer/SceneGraph/DCS.h"
#include "VE_Xplorer/SceneGraph/Group.h"
#include "VE_Xplorer/SceneGraph/Switch.h"

#include <string>
#include <vector>
#include <map>
#include <utility>

#include <vpr/Sync/Mutex.h>
#include <vpr/Thread/Thread.h>

#include <vrj/vrjParam.h>

namespace VE_SceneGraph
{
   class DCS;
	class Group;
	class Switch;
	class CADEntity;
	class CADEntityHelper;
	class Clone;
   class fileInfo;
}
namespace VE_EVENTS
{
   class EventHandler;
}
namespace VE_XML
{
namespace VE_CAD
{
   class CADNode;
   class CADAttribute;
}
}
namespace VE_Xplorer
{
   class cfdDataSet;
   class cfdCommandArray;
   class cfdSound;
}

#ifdef _OSG
#include <osg/ref_ptr>
#include <osg/StateSet>

namespace VE_TextureBased
{
   class cfdTextureDataSet;
}
#endif

class vtkDataSet;
class vtkUnstructuredGrid;

enum ModelTypeIndex
{
   SimulationModel=1,
   ExperimentalModel,
   DesignModel,
   GeometricalModel
};

enum Operation2Model
{
   AddVTKdataset=1,
   DelVTKdataset,
   DeleteVTKdataset,
   AddGeodataset,
   DelGeodataset,
   DeleteGeomdataset
};

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdModel : public cfdGlobalBase
{
public:
   cfdModel( VE_SceneGraph::DCS* );
   ~cfdModel();

   ///PreFrame callback to update the model based on commands from
   ///VE-Conductor
   void PreFrameUpdate(); 
   ///compare VjObs_i commandArray with its child's value
   virtual bool CheckCommandId( VE_Xplorer::cfdCommandArray * _cfdCommandArray ){return false;} 

   ///in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand() {}

   void setModelNode( VE_SceneGraph::CADEntityHelper * );
   void setModelType( ModelTypeIndex );//four type models right now (experiment, simulation, design, and geometry)
   bool GetMirrorDataFlag( void );
   void SetMirrorDataFlag( bool );
   void SetMirrorNode( VE_SceneGraph::Group* );

   VE_Xplorer::cfdDataSet* GetCfdDataSet( int );
   unsigned int GetIndexOfDataSet( std::string dataSetName );
   unsigned int GetNumberOfCfdDataSets( void );
   std::string GetCfdDataSetFileName( int );
   void CreateCfdDataSet( void );
   int GetKeyForCfdDataSet( cfdDataSet* );
   VE_Xplorer::cfdDataSet* GetActiveDataSet( void );
   void SetActiveDataSet( VE_Xplorer::cfdDataSet* );

   VE_SceneGraph::CADEntity* GetGeomDataSet( int );
   unsigned int GetNumberOfGeomDataSets( void );
   std::string GetGeomFileName( int );
   void CreateGeomDataSet( std::string );

   ///Set the UUID of the root CADNode 
   ///\param rootNodeId The uuid of the root CADNode in Conductor
   void SetRootCADNodeID(std::string rootNodeId);
   
   ///\param CAD goes transparent when dataset vis is active
   void MakeCADRootTransparent();

   ///\param CAD return to default state on clear all
   void MakeCADRootOpaque();

   ///Add a new attribute to a node
   ///\param nodeID The ID of the node to add Attribute to.
   ///\param nodeType The node type.
   ///\param The CADAttribute to add to the node.
   void AddAttributeToNode(std::string nodeID,
                        VE_XML::VE_CAD::CADAttribute* newAttribute);
   ///Add a new attribute to a node
   ///\param nodeID The ID of the node to add Attribute to.
   ///\param nodeType The node type.
   ///\param neAttribute The name of the CADAttribute to remove from the node.
   void RemoveAttributeFromNode(std::string nodeID,std::string nodeType,
                                std::string newAttribute);
   ///Add a new attribute to a node
   ///\param nodeID The ID of the node to add Attribute to.
   ///\param nodeType The node type.
   ///\param attributeName The name of the CADAttribute to activate on the CADNode.
   void SetActiveAttributeOnNode(std::string nodeID,
                                 std::string nodeType,
                                 std::string attributeName);
   ///Create a new assembly
   void CreateAssembly(std::string assemblyID);
   
   ///Create a new clone
   void CreateClone(std::string cloneID,
                    std::string originalID,
                    std::string orignalType);

   ///Create a new part
   void CreatePart(std::string fileName,
                   std::string partID,
                   std::string parentID);

   ///Get a specified CADAttribute for a specified CADNode
   ///\param nodeID The CADNode  
   ///\param attributeName The name of the CADAttribute to find.
   ///\param component The name of the CADMaterial component to update.
   ///\param face The face to apply the update to.
   ///\param values The new values.
   void UpdateMaterialComponent(std::string nodeID,std::string attributeName,std::string component,
                                 std::string face,std::vector<double> values);

   ///Get a specified CADAttribute for a specified CADNode
   ///\param nodeID The CADNode  
   ///\param attributeName The name of the CADAttribute to find.
   ///\param type The type of mode to update.
   ///\param mode The new mode.
   void UpdateMaterialMode(std::string nodeID,std::string attributeName,std::string type,std::string mode);

   ///Get a specific part. 
   ///\param partID The ID of the part to search form
   VE_SceneGraph::CADEntity* GetPart(std::string partID);

   ///Get a specific assembly. 
   ///\param assemblyID The ID of the assembly to search form
   VE_SceneGraph::DCS* GetAssembly(std::string assemblyID);

   ///Get a specific assembly. 
   ///\param assemblyID The ID of the assembly to search form
   VE_SceneGraph::Clone* GetClone(std::string cloneID);

   ///\param cloneID The part ID to search for.
   bool CloneExists(std::string clone);

   ///\param partID The part ID to search for.
   bool PartExists(std::string partID);

   ///\param assemblyID The assembly ID to search for.
   bool AssemblyExists(std::string assemblyID);

   ///Get the node for the cfd data set
   VE_SceneGraph::CADEntityHelper* GetCfdNode( void );

   ///Get the dcs for the cfd data set
   VE_SceneGraph::DCS* GetDCS( void );

   ///Set the id for this model
   ///\param id the id of the model to be set
   void SetID( unsigned int id );

   ///Get the id for this model
   unsigned int GetID( void );

   ///Add a new sound to the model
   ///\param soundName The name of the sound
   ///\param filename The actual filename of the sound to load
   void AddNewSound(std::string soundName,std::string filename);

   ///Activate (play) the sound
   ///\param soundName The name of the sound to play
   void ActivateSound(std::string soundName);

   ///Deactivate (stop) the sound
   ///\param soundName The name of the sound to stop
   void DeactivateSound(std::string soundName);
 
   //////////////////////////
   //texture based interface
#ifdef _OSG
   void SetActiveTextureDataSet( VE_TextureBased::cfdTextureDataSet* tDS);
#ifdef VE_PATENTED
  void CreateTextureDataSet();
   void AddDataSetToTextureDataSet(unsigned int index,
                         std::string textureDescriptionFile);
#endif
   unsigned int GetNumberOfTextureDataSets();
   VE_TextureBased::cfdTextureDataSet* GetTextureDataSet(unsigned int index);
   VE_TextureBased::cfdTextureDataSet* GetActiveTextureDataSet();
#endif
   ///////////////////////////////////////////////////

//Dynamically load data from unit
public:   
   void ActiveLoadingThread();
#if __VJ_version > 2000003
   void GetDataFromUnit(void);
#elif __VJ_version == 2000003
   void GetDataFromUnit(void* unused);
#endif
   const std::string MakeSurfaceFile(vtkDataSet*,int);
   void DynamicLoadingData(vtkUnstructuredGrid*, int, float*, float*, float*);
   void DynamicLoadingGeom(std::string, float*, float*, float*, float*, int, int);
   void AddVTKDataSet(vtkDataSet* );
   std::vector<vtkDataSet* >GetWaitingDataList();

  ///The current graph
   std::string GetRootCADNodeID();

private:
   vpr::Thread *loadDataTh;
   vpr::Mutex mValueLock;
   std::vector<vtkDataSet* > waitingdatalist; 
   std::string currentsurfacefilename;
   bool mirrorDataFlag;

private:
   //VE_SceneGraph::cfdTempAnimation* animation;
   osg::ref_ptr< VE_SceneGraph::Switch > switchNode;
   osg::ref_ptr< VE_SceneGraph::Group > classic;
   osg::ref_ptr< VE_SceneGraph::Group > textureBased;
   typedef std::vector< VE_SceneGraph::CADEntity* > GeometoryDataSetList;
   GeometoryDataSetList mGeomDataSets;
   typedef std::vector< VE_Xplorer::cfdDataSet* > VTKDataSetList;
   VTKDataSetList mVTKDataSets;

   std::map< std::string, VE_SceneGraph::CADEntity* > _partList;///<A list of the current parts.
   std::map< std::string, VE_SceneGraph::DCS* > _assemblyList;///A list of the current assemblies.
   std::map< std::string, VE_SceneGraph::Clone* > _cloneList;///A list of clones.

#ifdef _OSG
   typedef std::vector<VE_TextureBased::cfdTextureDataSet*> TextureDataSetList;
   TextureDataSetList mTextureDataSets;
   VE_TextureBased::cfdTextureDataSet* _activeTextureDataSet;
#endif

   osg::ref_ptr< VE_SceneGraph::DCS > mModelDCS;
   osg::ref_ptr< VE_SceneGraph::DCS > _worldDCS;
   VE_SceneGraph::CADEntityHelper* mModelNode;
   cfdDataSet* activeDataSet;
   VE_SceneGraph::Clone* mirrorNode;
   osg::ref_ptr< VE_SceneGraph::Group > mirrorGroupNode;

   //the information for following three variables should be transfered from cfdApp
   ModelTypeIndex mModelType;
   Operation2Model mActiveOperation2Model;

   bool mUpdateModelFlag;
   bool mMoveOldGeomDataSets;
   bool mMoveOldVTKDataSets;   

   std::string rootCADNodeID;///<ID for root CAD node id
   unsigned int modelID;

   
   std::map<std::string, cfdSound> _availableSounds;///<The available sounds for this model.
#ifdef _OSG
         std::map< std::string, std::vector< std::pair< std::string, osg::ref_ptr< osg::StateSet > > > > _nodeAttributes;///<The map of node attributes.
#endif
   };
}
#endif
