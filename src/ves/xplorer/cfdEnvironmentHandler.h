/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
#include <ves/VEConfig.h>
#include <ves/xplorer/event/data/SeedPoints.h>
#include <ves/xplorer/scenegraph/DCS.h>
#include <map>
#include <vector>

#include <vrj/vrjParam.h>
#include <osgEphemeris/EphemerisModel>
#include <osg/ref_ptr>
namespace VE_Xplorer
{
    class cfdCursor;
    class cfdCommandArray;
    class cfdTeacher;
    class cfdQuatCamHandler;
    class cfdDisplaySettings;

    class DisplayInformation;
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

   ///Constructor
   cfdEnvironmentHandler( void );

   ///Destructor
   ~cfdEnvironmentHandler( void );
   vprSingletonHeader( cfdEnvironmentHandler );   

public:
   ///Initialize environment.
   void Initialize();

   ///Clean up environment.
   //void CleanUp( void );

   ///Initialize scene.
   void InitScene( void );

   ///Pre frame update.
   void PreFrameUpdate( void );

   ///Late pre-frame update ???
   void LatePreFrameUpdate( void );

   ///Set the command array ?
   ///\param input
   void SetCommandArray( cfdCommandArray* input );
   ///Accessor for cfdCursor
   cfdCursor* GetCursor( void );
   ///Accessor for cfdTeacher
   cfdTeacher* GetTeacher( void );

   ///Accessor for cfdQuatCamHandler
   //cfdQuatCamHandler* GetQuatCamHandler( void );

   ///Accessor for cfdDisplaySettings
   cfdDisplaySettings* GetDisplaySettings( void );
   ///Accessor to set desktop size information for
   /// runtime reconfiguration of desktop windows
   ///\param width The desktop width
   ///\param height The desktop height
   void SetDesktopSize( int width, int height );
   ///Accessor to get desktop size information 
   ///\param width The desktop width
   ///\param height The desktop height
   void GetDesktopSize( int &width, int &height );
   
   ///Set the window dimension
   ///\param width The window width
   ///\param height The window height
   void SetWindowDimensions( unsigned int width, unsigned int height );
 
   ///Set up the frustum values
   ///\param _left
   ///\param _right
   ///\param _top
   ///\param _bottom
   ///\param _near
   ///\param _far
   void SetFrustumValues( float _left, float _right, float _top, float _bottom, float _near, float _far );
  
   ///Get the window width 
   unsigned int GetWindowWidth( void );

   ///Get the window height
   unsigned int GetWindowHeight( void );

   ///Set the frame rate 	
   void SetFrameRate( float value );

   ///Get the frame rate 
   float GetFrameRate();

   ///Post frame update (look this up)
   void PostFrameUpdate();
   ///Accessor for DisplayInformation
   DisplayInformation* GetDisplayInformation( void );

   ///Activate geometry picking functionality.
   void ActivateGeometryPicking( void );

   ///Deactivate geoometry picking functionality.
   void DeactivateGeometryPicking( void );

   ///Get the seed points drawable
   ///\return Get the seed points
   SeedPoints* GetSeedPoints();

   ///Get the seed points drawable
   ///\return Return the DCS for the seed points
   ves::xplorer::scenegraph::DCS* GetSeedPointsDCS();

   ///Get the ephemeris data
   osgEphemeris::EphemerisModel* GetEphemerisModel();

private:
   osg::ref_ptr<VE_Xplorer::SeedPoints> _seedPoints;///<The seed points for this dataset
   osg::ref_ptr<ves::xplorer::scenegraph::DCS> _seedPointsDCS;///<The DCS for the seed points
   cfdTeacher* _teacher;///<Handle teacher functionality.
   //cfdSoundHandler* _soundHandler;///<Handle the sound.
   cfdQuatCamHandler* _camHandler;///<Handle quat cam functionality.

#ifdef _OSG
   DisplayInformation* display_information;///<???

   ///Flag for active geometry picking.
   bool _activeGeomPicking;
#endif //_OSG

   cfdCursor* cursor;///<The cursor.
   std::string _param;///<Store parameters.
   cfdCommandArray* _commandArray;///<Command array.

   // cur_box will eventually be used to define bounding box
   // for data interagation
   double cur_box[6];///<???
   vtkPolyData * arrow;///<???
   float worldScale[ 3 ];///<World scale.
   float worldTrans[ 3 ];///<World translation
   float worldRot[ 3 ];///<World rotation.

   ///<The class used to change juggler configuration settings during runtime
   cfdDisplaySettings* displaySettings;

   int desktopWidth;///<Desktop width.
   int desktopHeight;///<Desktop height.

   int _windowWidth;///<Window width.
   int _windowHeight;///<Window height.

   float _frustumLeft;///<Left frustum.
   float _frustumRight;///<Right frustum.
   float _frustumTop;///<Top frustum.
   float _frustumBottom;///<Bottom frustum.
   float _frustumNear;///<Near frustum.
   float _frustumFar;///<Far frustum.

   float framerate;///<Frame rate.
    std::map< std::string, VE_EVENTS::EventHandler* > _eventHandlers;///<The event handler for commands.

    osg::ref_ptr<osgEphemeris::EphemerisModel> m_ephemerisModel;///<The model containing ephemeris data
};
}

#endif //CFD_ENVIRONMENTHANDLER_H
