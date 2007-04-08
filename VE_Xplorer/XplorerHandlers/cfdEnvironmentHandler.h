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
#ifndef CFD_ENVIRONMENTHANDLER_H
#define CFD_ENVIRONMENTHANDLER_H
/*!\file cfdEnvironmentHandler.h
cfdEnvironmentHandler API
*/
/*!\class VE_Xplorer::cfdEnvironmentHandler
* 
*/
#include <vpr/Util/Singleton.h>
#include "VE_Installer/include/VEConfig.h"
#include "VE_Xplorer/XplorerHandlers/SeedPoints.h"
#include "VE_Xplorer/SceneGraph/DCS.h"
#include <map>
#include <vector>
#include <osg/ref_ptr>
namespace VE_Xplorer
{
	class cfdNavigate;
   class cfdCursor;
   class cfdCommandArray;
   class cfdReadParam;
   class cfdSoundHandler;
   class cfdTeacher;
   class cfdSoundHandler;
   class cfdQuatCamHandler;
   class cfdDisplaySettings;

   class DisplayInformation;

   #ifdef VE_PATENTED
   #ifdef _OSG
      class cfdObjectHandler;
   #endif //_OSG
   #endif //VE_PATENTED
}
namespace VE_EVENTS
{
   class EventHandler;
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
   void Initialize();
   void CleanUp( void );
   void InitScene( void );
   void PreFrameUpdate( void );
   void LatePreFrameUpdate( void );
   void SetCommandArray( cfdCommandArray* );

   //void CreateObjects( void );
   ///Update the background color
   ///\param color The background color
   void SetBackgroundColor(std::vector<double> color);

   ///Accessor for cfdNavigate
   //cfdNavigate* GetNavigate( void );

   ///Accessor for cfdCursor
   cfdCursor* GetCursor( void );

   ///Accessor for cfdSoundHandler
   cfdSoundHandler* GetSoundHandler( void );

   ///Accessor for cfdTeacher
   cfdTeacher* GetTeacher( void );

   ///Accessor for cfdQuatCamHandler
   //cfdQuatCamHandler* GetQuatCamHandler( void );

   ///Accessor for cfdDisplaySettings
   cfdDisplaySettings* GetDisplaySettings( void );

   ///Get the background color
   std::vector<float> GetBackgroundColor( void );

   ///Accessor to set desktop size information for
   /// runtime reconfiguration of desktop windows
   void SetDesktopSize( int width, int height );
	void SetWindowDimensions( unsigned int width, unsigned int height );
	void SetFrustumValues( float _top, float _bottom, float _near, float _far );

   unsigned int GetWindowWidth( void );
   unsigned int GetWindowHeight( void );

	void SetFrameRate( float value );
	float GetFrameRate();

	void PostFrameUpdate();

   ///Check if the background color has changed
   bool BackgroundColorChanged();

   ///Reset the background changed color flag
   void ResetBackgroundColorUpdateFlag();
  
   #ifdef _OSG
   ///Accessor for DisplayInformation
   DisplayInformation* GetDisplayInformation( void );

   #ifdef VE_PATENTED 
   void ActivateGeometryPicking( void );
   void DeactivateGeometryPicking( void );
   #endif //VE_PATENTED
   #endif //_OSG 
   ///Get the seed points drawable
   SeedPoints* GetSeedPoints();

   ///Get the seed points drawable
   VE_SceneGraph::DCS* GetSeedPointsDCS();

private:
   osg::ref_ptr<VE_Xplorer::SeedPoints> _seedPoints;///<The seed points for this dataset
   osg::ref_ptr<VE_SceneGraph::DCS> _seedPointsDCS;///<The DCS for the seed points
   cfdNavigate* nav;
   cfdTeacher* _teacher;
   cfdSoundHandler* _soundHandler;
   cfdQuatCamHandler* _camHandler;

   #ifdef _OSG
   DisplayInformation* display_information;

   #ifdef VE_PATENTED 
   cfdObjectHandler* objectHandler;
   bool _activeGeomPicking;
   #endif // VE_PATENTED
   #endif //_OSG

   std::vector<float> _clearColor;///<The background color;
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

   int desktopWidth;                   ///<Desktop width
   int desktopHeight;                  ///<Desktop height

	int _windowWidth;
	int _windowHeight;

	float _frustumTop;
	float _frustumBottom;
	float _frustumNear;
   float _frustumFar;

	float framerate;

   bool _updateBackgroundColor;        ///<The flag for updating the background color in xplorer
   
   std::map< std::string, VE_EVENTS::EventHandler* > _eventHandlers;///<The event handler for commands.
};
}

#endif //CFD_ENVIRONMENTHANDLER_H
