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
 * File:          $RCSfile: cfdObjects.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_OBJECTS_H
#define CFD_OBJECTS_H

namespace VE_Xplorer
{
   class cfdDataSet;
   class cfdReadParam;
   class cfdCommandArray;
}

namespace VE_SceneGraph
{
   class cfdGeode;
   class cfdDCS;
   class cfdSequence;
   class cfdNode;
   class cfdTempAnimation;
   class cfdGeode;
}

// VTK Classes
class vtkPolyData;
class vtkActor;
class vtkGlyph3D;
class vtkMaskPoints;

#include <vector>
#include <vpr/Sync/Mutex.h>

#include "VE_Xplorer/cfdGlobalBase.h"
namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdObjects : public cfdGlobalBase
   {
      public:
         cfdObjects( const cfdObjects& src );
         cfdObjects( void );
         virtual ~cfdObjects( void );

         // pure virtual functions to be specified in concrete implementations

         // compare VjObs_i commandArray with its child's value
         virtual bool CheckCommandId( cfdCommandArray* commandArray );

         // in future, multi-threaded apps will make a copy of VjObs_i commandArray
         virtual void UpdateCommand();

         // update the actor
         virtual void Update() = 0;

         std::vector< VE_SceneGraph::cfdGeode* > GetGeodes( void );
         void ClearGeodes( void );

         void SetObjectType( int );
         int GetObjectType( void ) { return this->objectType; }
         void SetOrigin( float [3] );
         double * GetOrigin();
         void GetOrigin( double [3] );
         void SetNormal( double [3] );
         void SetBoxSize( double [6] );
         void SetRequestedValue( int x ) { this->requestedValue = x; }
         void SetCursorType( int x ) { this->cursorType = x; }
         void SetPreCalcFlag( int x ) { this->usePreCalcData = x; }
         void SetUpdateFlag( bool x ) { this->updateFlag = x; }
         bool GetUpdateFlag( void ) { return ( this->updateFlag ); }
         void DeletecfdGeode( void );

         //void SetSequence( cfdTempAnimation* );
         //cfdTempAnimation* GetSequence( void );
      
         void SetSourcePoints( vtkPolyData * );

         void AddGeodesToSequence(void);

         void SetGeodeFlag( bool x );
         bool GetGeodeFlag( void );

         bool GetTransientGeodeFlag(void);
         void SetTransientGeodeFlag(bool x);

         void SetActiveDataSet( cfdDataSet * dataset );
         cfdDataSet * GetActiveDataSet( void );

         //void ClearTransientVector( void );
         //static void SetVectorScale( float );
         //static float GetVectorScale();
      
      protected:
         cfdDataSet* activeDataSet;

         // used by vectors and intended for warped contours
         //static float vectorScale;

         std::vector< VE_SceneGraph::cfdGeode* > geodes;
         vtkPolyData* pointSource;

         bool updateFlag;
         int vtkToPFDebug;
         int objectType;
         int requestedValue;
         int cursorType;
         int usePreCalcData;
         double origin[ 3 ];
         double center[ 3 ];
         double normal[ 3 ];
         double box_size[ 6 ];
         float scale;
      private:
   };
}
#endif
