/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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

namespace VE_SceneGraph{
   class cfdDCS;
   class cfdGroup;
}
class cfdDataSet;
class cfdModel;
class cfdCommandArray;
class cfdReadParam;
class cfdScalarBarActor;


#ifdef _OSG
#ifdef VE_PATENTED
class cfdTextureDataSet;
#endif
#endif
class vtkPolyData;

#include <vector>
#include "VE_Xplorer/cfdConfig.h"

class WXPLUGIN_DECLSPEC cfdModelHandler //: public vpr::Singleton< cfdModelHandler >
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
      void Initialize( char* );
      void CleanUp( void );
      void InitScene( void );
      void PreFrameUpdate( void );
      cfdDataSet* GetActiveDataSet( void );

      void SetCommandArray( cfdCommandArray* );
      void CreateObjects( void );      
      void LoadSurfaceFiles( char* );
      vtkPolyData* GetArrow( void );
      cfdModel* GetModel( int );
      void AddModel( cfdModel* );
      void RemoveModel( cfdModel* );
      cfdModel* GetActiveModel( void );
      int GetNumberOfModels( void );
      void ReadNNumberOfDataSets(  char*, char* );

      //texture manager access
#ifdef _OSG
#ifdef VE_PATENTED
      cfdTextureDataSet* GetActiveTextureDataSet( void );
#endif
#endif
      bool GetVisOption();
   private:
      char* _param;
      cfdDataSet* activeDataset;
      cfdCommandArray* commandArray;
      cfdReadParam* _readParam;
      cfdScalarBarActor* _scalarBar;
      cfdModel* _activeModel;

#ifdef _OSG
#ifdef VE_PATENTED
      cfdTextureDataSet* _activeTDSet;
#endif
#endif
      bool tbased;
      vtkPolyData* arrow;
      std::vector< cfdModel* > _modelList;
      // Used to store data for multi-dataset functions
      char oldDatasetName[256];
};

#endif
