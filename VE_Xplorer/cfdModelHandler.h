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
 * File:          $RCSfile: cfdModelHandler.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_MODELHANDLER_H
#define CFD_MODELHANDLER_H

#include <vpr/Util/Singleton.h>
#include <map>
#include <utility>
namespace VE_SceneGraph
{
   class cfdDCS;
   class cfdGroup;
}

namespace VE_Xplorer
{
   class cfdDataSet;
   class cfdModel;
   class cfdCommandArray;
   class cfdReadParam;
   class cfdScalarBarActor;
}
namespace VE_EVENTS
{
   class EventHandler;
}
#ifdef _OSG
#ifdef VE_PATENTED
namespace VE_TextureBased
{
   class cfdTextureDataSet;
}
#endif
#endif
class vtkPolyData;

#include <vector>
#include "VE_Installer/include/VEConfig.h"

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdModelHandler //: public vpr::Singleton< cfdModelHandler >
{
private:
   // Required so that vpr::Singleton can instantiate this class.
   //friend class vpr::Singleton< cfdModelHandler >;
   //cfdModelHandler(const cfdModelHandler& o) { ; }
   //cfdModelHandler& operator=(const cfdModelHandler& o) { ; }
   cfdModelHandler( void );
   ~cfdModelHandler( void ){ ; }// Never gets called, don't implement
   vprSingletonHeader( cfdModelHandler );   
public:
   void Initialize( std::string );
   void CleanUp( void );
   void InitScene( void );
   void PreFrameUpdate( void );
   cfdDataSet* GetActiveDataSet( void );

   void SetCommandArray( cfdCommandArray* );
   void CreateObjects( void );      
   void LoadSurfaceFiles( std::string );
   vtkPolyData* GetArrow( void );
   cfdModel* GetModel( int );
   void AddModel( cfdModel* );
   void RemoveModel( cfdModel* );
   cfdModel* GetActiveModel( void );
   void SetActiveModel( int modelNumber );
   int GetNumberOfModels( void );

   void ReadNNumberOfDataSets(  std::string, std::string );

   cfdScalarBarActor* GetScalarBar(void);
   //void ReadNNumberOfDataSets(  char*, char* );


   //texture manager access
#ifdef _OSG
#ifdef VE_PATENTED
   VE_TextureBased::cfdTextureDataSet* GetActiveTextureDataSet( void );
#endif
#endif
   bool GetVisOption();
protected:
   vtkPolyData* _GetArrowPolyData();
private:
   std::string _param;
   cfdDataSet* activeDataset;
   cfdCommandArray* commandArray;
   cfdReadParam* _readParam;
   cfdScalarBarActor* _scalarBar;
   cfdModel* _activeModel;

#ifdef _OSG
#ifdef VE_PATENTED
   VE_TextureBased::cfdTextureDataSet* _activeTDSet;
#endif
#endif
   bool tbased;
   vtkPolyData* arrow;
   std::vector< cfdModel* > _modelList;
   // Used to store data for multi-dataset functions
   std::string oldDatasetName;//[256];

   std::map< std::string,VE_EVENTS::EventHandler*> _eventHandlers;///<The event handler for commands.
};
}
#endif
