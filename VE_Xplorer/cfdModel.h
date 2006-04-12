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
 * File:          $RCSfile: cfdModel.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef CFD_MODEL_H
#define CFD_MODEL_H

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

#include <string>
#include <vector>
#include <map>
#include <utility>

#include <vpr/Sync/Mutex.h>
#include <vpr/Thread/Thread.h>

//#include "readWriteVtkThings.h"

namespace VE_SceneGraph
{
   class cfdDCS;
   class cfdNode;
   class cfdSwitch;
   class cfdGroup;
   class cfdClone;
   class cfdTempAnimation;
}
namespace VE_EVENTS
{
   class EventHandler;
}
namespace VE_CAD
{
   class CADNode;
   class CADAttribute;
}
namespace VE_Xplorer
{
   class cfdDataSet;
   class fileInfo;
   class cfdFILE;
   class cfdCommandArray;
}

#ifdef _OSG
namespace VE_TextureBased
{
   class cfdTextureDataSet;
}
#include <osg/StateSet>
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

#include "VE_Installer/include/VEConfig.h"
#include "VE_Xplorer/cfdGlobalBase.h"

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdModel : public cfdGlobalBase
{
   public:
      cfdModel(VE_SceneGraph::cfdDCS *);
      ~cfdModel();
  
      ///PreFrame callback to update the model based on commands from
      ///VE-Conductor
      void PreFrameUpdate(); 
      ///compare VjObs_i commandArray with its child's value
      virtual bool CheckCommandId( VE_Xplorer::cfdCommandArray * _cfdCommandArray ){return false;} 

      ///in future, multi-threaded apps will make a copy of VjObs_i commandArray
      virtual void UpdateCommand() {}

      void setModelNode( VE_SceneGraph::cfdNode * );
      void setModelType( ModelTypeIndex );//four type models right now (experiment, simulation, design, and geometry)
         
      void setTrans3( float x, float y, float z );
      void setTrans( float trans[3] );
      void setScale( float x,float y, float z );
      void setRot(float,float,float);
      void setRotMat(double *);

      void updateCurModel();//handling the add or delete the vtkdateset or geomdataset
      void addVTKdataset(const std::string&);
      void delVTKdataset();
      void addGeomdataset(const std::string &filename);
      void delGeomdataset(int);
      bool GetMirrorDataFlag( void );
      void SetMirrorDataFlag( bool );
      void SetMirrorNode( VE_SceneGraph::cfdGroup* );

   
      VE_Xplorer::cfdDataSet* GetCfdDataSet( int );
      unsigned int GetNumberOfCfdDataSets( void );
      std::string GetCfdDataSetFileName( int );
      void CreateCfdDataSet( void );
      int GetKeyForCfdDataSet( cfdDataSet* );
      VE_Xplorer::cfdDataSet* GetActiveDataSet( void );
      void SetActiveDataSet( VE_Xplorer::cfdDataSet* );

      VE_Xplorer::cfdFILE* GetGeomDataSet( int );
      unsigned int GetNumberOfGeomDataSets( void );
      std::string GetGeomFileName( int );
      void CreateGeomDataSet( std::string );

      ///Add a new attribute to a node
      ///\param nodeID The ID of the node to add Attribute to.
      ///\param nodeType The node type.
      ///\param The CADAttribute to add to the node.
      void AddAttributeToNode(unsigned int nodeID,
                           VE_CAD::CADAttribute* newAttribute);
      ///Add a new attribute to a node
      ///\param nodeID The ID of the node to add Attribute to.
      ///\param nodeType The node type.
      ///\param attributeName The name of the CADAttribute to activate on the CADNode.
      void SetActiveAttributeOnNode(unsigned int nodeID,
                                    std::string nodeType,
                                    std::string attributeName);
      ///Create a new assembly
      void CreateAssembly(unsigned int assemblyID);
      
      ///Create a new clone
      void CreateClone(unsigned int cloneID,
                     unsigned int originalID,
                     std::string orignalType);

      ///Create a new part
      void CreatePart(std::string fileName,
                    unsigned int partID,
                    unsigned int parentID);

      ///Get a specified CADAttribute for a specified CADNode
      ///\param nodeID The CADNode  
      ///\param attributeName The name of the CADAttribute to find.
      void UpdateMaterialComponent(unsigned int nodeID,std::string attributeName,std::string component,
                          std::string face,std::vector<double> values);

      ///Get a specific part. 
      ///\param partID The ID of the part to search form
      VE_Xplorer::cfdFILE* GetPart(unsigned int partID);

      ///Get a specific assembly. 
      ///\param assemblyID The ID of the assembly to search form
      VE_SceneGraph::cfdDCS* GetAssembly(unsigned int assemblyID);

      ///Get a specific assembly. 
      ///\param assemblyID The ID of the assembly to search form
      VE_SceneGraph::cfdClone* GetClone(unsigned int cloneID);

      ///\param cloneID The part ID to search for.
      bool CloneExists(unsigned int clone);

      ///\param partID The part ID to search for.
      bool PartExists(unsigned int partID);

      ///\param assemblyID The assembly ID to search for.
      bool AssemblyExists(unsigned int assemblyID);

      ///Get the node for the cfd data set
      VE_SceneGraph::cfdNode* GetCfdNode( void );

      ///Get the dcs for the cfd data set
      VE_SceneGraph::cfdDCS* GetCfdDCS( void );

      ///Set the id for this model
      ///\param id the id of the model to be set
      void SetID( unsigned int id );

      ///Get the id for this model
      unsigned int GetID( void );

      //////////////////////////
      //texture based interface
#ifdef _OSG
      void SetActiveTextureDataSet( VE_TextureBased::cfdTextureDataSet* tDS);
      void CreateTextureDataSet();
      void AddDataSetToTextureDataSet(unsigned int index,
                            std::string textureDescriptionFile);
      unsigned int GetNumberOfTextureDataSets();
      VE_TextureBased::cfdTextureDataSet* GetTextureDataSet(unsigned int index);
      VE_TextureBased::cfdTextureDataSet* GetActiveTextureDataSet();
#endif
      ///////////////////////////////////////////////////

      VE_SceneGraph::cfdTempAnimation* GetAnimation( void );
      std::map<int,VE_Xplorer::cfdDataSet*> transientDataSets;

//Dynamically load data from unit
public:   
   void ActiveLoadingThread();
   void GetDataFromUnit(void* unused);
   const std::string MakeSurfaceFile(vtkDataSet*,int);
   void DynamicLoadingData(vtkUnstructuredGrid*, int, float*, float*, float*);
   void DynamicLoadingGeom(std::string, float*, float*, float*, float*, int, int);
   void AddVTKDataSet(vtkDataSet* );
   std::vector<vtkDataSet* >GetWaitingDataList();
 

   ///The current graph
   VE_CAD::CADNode* GetRootCADNode();
   
      private:
         vpr::Thread *loadDataTh;
         vpr::Mutex mValueLock;
         std::vector<vtkDataSet* > waitingdatalist; 
         std::string currentsurfacefilename;
         bool mirrorDataFlag;

      private:
         VE_SceneGraph::cfdTempAnimation* animation;
         VE_SceneGraph::cfdSwitch* switchNode;
         VE_SceneGraph::cfdGroup* classic;
         VE_SceneGraph::cfdGroup* textureBased;
         typedef std::vector< VE_Xplorer::cfdFILE* > GeometoryDataSetList;
         GeometoryDataSetList mGeomDataSets;
         typedef std::vector< VE_Xplorer::cfdDataSet* > VTKDataSetList;
         VTKDataSetList mVTKDataSets;

         std::map<unsigned int,VE_Xplorer::cfdFILE*> _partList;///<A list of the current parts.
         std::map<unsigned int,VE_SceneGraph::cfdDCS*> _assemblyList;///A list of the current assemblies.
         std::map<unsigned int,VE_SceneGraph::cfdClone*> _cloneList;///A list of clones.

      #ifdef _OSG
         typedef std::vector<VE_TextureBased::cfdTextureDataSet*> TextureDataSetList;
         TextureDataSetList mTextureDataSets;
         VE_TextureBased::cfdTextureDataSet* _activeTextureDataSet;
      #endif

         VE_SceneGraph::cfdDCS* mModelDCS;
         VE_SceneGraph::cfdDCS* _worldDCS;
         VE_SceneGraph::cfdNode* mModelNode;
         fileInfo* mGeomFileInfo;
         fileInfo* mVTKFileInfo;
         cfdDataSet* activeDataSet;
         VE_SceneGraph::cfdClone* mirrorNode;
         VE_SceneGraph::cfdGroup* mirrorGroupNode;
   
         //the information for following three variables should be transfered from cfdApp
         ModelTypeIndex mModelType;
         Operation2Model mActiveOperation2Model;
   
         bool mUpdateModelFlag;
         bool mMoveOldGeomDataSets;
         bool mMoveOldVTKDataSets;   

         VE_CAD::CADNode* _rootCADNode;///<The root CADNode.
   unsigned int modelID;
#ifdef _OSG
         std::map< unsigned int, std::vector< std::pair< std::string, osg::ref_ptr< osg::StateSet > > > > _nodeAttributes;///<The map of node attributes.
#endif
   };
}
#endif
