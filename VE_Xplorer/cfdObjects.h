/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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

class cfdDataSet;
class cfdReadParam;

class pfDCS;
class pfNode;
class pfGeode;
#ifndef _USE_CFD_SEQUENCE
class pfSequence;
#else
class cfdSequence;
#endif

// VTK Classes
class vtkPolyData;
class vtkActor;
class vtkPolyDataSource;
class vtkGlyph3D;
class vtkMaskPoints;

#include <vector>

#ifdef _CFDCOMMANDARRAY
class cfdApp;
#include "cfdGlobalBase.h"
#endif //_CFDCOMMANDARRAY

class cfdObjects
#ifdef _CFDCOMMANDARRAY
                  : public cfdGlobalBase
#endif //_CFDCOMMANDARRAY

{
   public:
      cfdObjects( pfGeode *, int );
      cfdObjects( const cfdObjects& src );
      cfdObjects( void );
      virtual ~cfdObjects( void );

#ifdef _CFDCOMMANDARRAY
      // pure virtual functions to be specified in concrete implementations

      // compare VjObs_i commandArray with its child's value
      virtual bool CheckCommandId( cfdApp * _cfdApp );

      // in future, multi-threaded apps will make a copy of VjObs_i commandArray
      virtual void UpdateCommand();
#endif //_CFDCOMMANDARRAY

      // update the actor
      virtual void Update() = 0;

      void SetGeode( pfGeode * );
      void SetObjectType( int );
      //void UpdateObject( void );
      pfGeode *GetGeode( void );
      //void GetGeoSet( pfGeoSet *[] );
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
      void DeleteGeode( void );

#ifndef _USE_CFD_SEQUENCE
      void SetpfSequence( pfSequence * );
      pfSequence* GetpfSequence( void );
#else
      void SetpfSequence( cfdSequence * );
      cfdSequence* GetpfSequence( void );
#endif

      //void DeletepfSequence( void );
      void StoppfSequence( void );
      void StartpfSequence( void );
      void PausepfSequence( void );
      void ResumepfSequence( void );
      void ClearpfSequence( void );
      void AddTopfSequence( void );
      void ReversepfSequence( void );
      void ForwardpfSequence( void );
      int  GetFrameOfpfSequence( void );
      
      void SetSourcePoints( vtkPolyDataSource * );

      void UpdateGeode( void );
      void AddGeodeToDCS( void );
      void RemoveGeodeFromDCS( void );
      void CreateGeode( void );

      void SetDCS( pfDCS * );
      pfDCS *GetDCS( void );
      void SetcfdReadParam( cfdReadParam * );

      void SetGeodeFlag( bool x );
      bool GetGeodeFlag( void );

      static void SetActiveDataSet( cfdDataSet * dataset );
      static cfdDataSet * GetActiveDataSet();

      static void SetActiveMeshedVolume( cfdDataSet * dataset );
      static cfdDataSet * GetActiveMeshedVolume();

      static void SetActiveParticleData( cfdDataSet * dataset );
      static cfdDataSet * GetActiveParticleData();

      static void SetActiveSurfaceData( cfdDataSet * dataset );
      static cfdDataSet * GetActiveSurfaceData();

      static void SetVectorScale( float );
      static float GetVectorScale();

      static bool GetTimeToUpdateFlag( void );
      static void SetTimeToUpdateFlag( bool );

      static void SetSphereScale( float );
      static float GetSphereScale();

   protected:

      static cfdDataSet *activeDataSet;
      static cfdDataSet *activeMeshedVolume;
      static cfdDataSet *activeParticleData;
      static cfdDataSet *activeSurfaceData;
      static float vectorScale;	// used by vectors and intended for warped contours
      static float sphereScale;	// used by cfdPolydata for setting the size of sphere particles

      static bool timeToUpdate;

      pfGeode *geode;
      pfGeode* tempGeode;

#ifndef _USE_CFD_SEQUENCE
      pfSequence *sequence;
#else
      cfdSequence* sequence;
#endif

      pfDCS *dcs;
      std::vector< pfNode * > geodes;

      vtkActor *actor;
      vtkActor *PDactor;
      vtkPolyDataSource *pointSource;

      cfdReadParam *paramFile;
   
      float scale;
      int objectType;
      int requestedValue;
      double origin[ 3 ];
      double center[ 3 ];
      double normal[ 3 ];
      double box_size[ 6 ];
      int cursorType;
      int usePreCalcData;
      bool updateFlag;
      bool addGeode;
      int vtkToPFDebug;

   private:
};

#endif
