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
 * File:          $RCSfile: cfdEnvironmentHandler.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_ENVIRONMENTHANDLER_H
#define CFD_ENVIRONMENTHANDLER_H

#include <vpr/Util/Singleton.h>
#include "VE_Installer/include/VEConfig.h"

namespace VE_Xplorer
{
	class cfdNavigate;
	class cfdTrackball;
   class cfdCursor;
   class cfdCommandArray;
   class cfdReadParam;
   class cfdSoundHandler;
   class cfdTeacher;
   class cfdSoundHandler;
   class cfdQuatCamHandler;
   class cfdDisplaySettings;
#ifdef VE_PATENTED
#ifdef _OSG
   class cfdObjectHandler;
#endif // _OSG
#endif // VE_PATENTED
}

namespace VE_SceneGraph
{
   class cfdDCS;
   class cfdGroup;
}

class vtkPolyData;

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdEnvironmentHandler //: public vpr::Singleton< cfdEnvironmentHandler >
{
private:
   // Required so that vpr::Singleton can instantiate this class.
   //friend class vpr::Singleton< cfdEnvironmentHandler >;
   //cfdEnvironmentHandler(const cfdEnvironmentHandler& o) { ; }
   //cfdEnvironmentHandler& operator=(const cfdEnvironmentHandler& o) { ; }
   cfdEnvironmentHandler( void );
   ~cfdEnvironmentHandler( void ){ ; }// Never gets called, don't implement
   vprSingletonHeader( cfdEnvironmentHandler );   

public:
   void Initialize( std::string );
   void CleanUp( void );
   void InitScene( void );
   void PreFrameUpdate( void );
   void SetCommandArray( cfdCommandArray* );
   void CreateObjects( void );
	
   ///Accessor for cfdNavigate
   cfdNavigate* GetNavigate( void );
	///Accessor for cfdTrackball
	cfdTrackball* GetTrackball( void );
   ///Accessor for cfdCursor
   cfdCursor* GetCursor( void );
   ///Accessor for cfdSoundHandler
   cfdSoundHandler* GetSoundHandler( void );
   ///Accessor for cfdTeacher
   cfdTeacher* GetTeacher( void );
   ///Accessor for cfdQuatCamHandler
   cfdQuatCamHandler* GetQuatCamHandler( void );
   ///Accessor for cfdDisplaySettings
   cfdDisplaySettings* GetDisplaySettings( void );
   ///Accessor to set desktop size information for
   /// runtime reconfiguration of desktop windows
   void SetDesktopSize( int width, int height );
	void SetWindowDimensions( unsigned int width, unsigned int height );
	void PostFrameUpdate();
#ifdef _OSG 
#ifdef VE_PATENTED 
   void ActivateGeometryPicking();
   void DeactivateGeometryPicking();
#endif // VE_PATENTED
#endif //_OSG 
private:
   cfdNavigate* nav;
	cfdTrackball* trackball;
   cfdTeacher* _teacher;
   cfdSoundHandler* _soundHandler;
   cfdQuatCamHandler* _camHandler;

#ifdef _OSG 
#ifdef VE_PATENTED 
   cfdObjectHandler* objectHandler;
   bool _activeGeomPicking;
#endif // VE_PATENTED
#endif //_OSG
   cfdCursor* cursor;
   std::string _param;
   cfdCommandArray* _commandArray;
   cfdReadParam* _readParam;
   // cur_box will eventually be used to define bounding box
   // for data interagation
   double cur_box[6];
   vtkPolyData * arrow;
   float worldScale[ 3 ];
   float worldTrans[ 3 ];
   float worldRot[ 3 ];
   ///<The class used to change juggler configuration settings during runtime
   cfdDisplaySettings* displaySettings;
   ///<Desktop width
   int desktopWidth;
   ///<Desktop height
   int desktopHeight;

	int _windowWidth;
	int _windowHeight;
};
}
#endif
