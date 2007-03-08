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
 * Date modified: $Date:  $
 * Version:       $Rev:  $
 * Author:        $Author:  $
 * Id:            $Id:  $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef DISPLAY_INFORMATION_H
#define DISPLAY_INFORMATION_H
/*!\file DisplayInformation.h
DisplayInformation API
*/
/*!\class VE_Xplorer::DisplayInformation
* 
*/
#include "VE_Installer/include/VEConfig.h"

#include "VE_Xplorer/SceneGraph/Switch.h"

#ifdef _OSG
#include <osg/ref_ptr>
#include <osg/CameraNode>
#include <osgText/Text>
#endif

//C/C++ Libraries
#include <vector>

namespace VE_SceneGraph
{
	class Switch;
	class CADEntity;
}

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS DisplayInformation
   {
      public:
         DisplayInformation();
         ~DisplayInformation();

         void LatePreFrame();

         ///Set the display flags
         void SetFrameRateFlag( bool val );
         void SetCoordSysFlag( bool val );

			void SetTextColor( std::vector< double > color );

			void SetDisplayPositions( unsigned int width, unsigned int height );

      private:
         ///Initialize the framerate display
         void InitFrameRateDisplay();

         ///Initialize the world coordinate system display
         void InitCoordSysDisplay();

			osg::ref_ptr< VE_SceneGraph::Switch > display_switch;

			osg::ref_ptr< osg::CameraNode > framerate;
			osg::ref_ptr< osg::CameraNode > wcs;

			osg::ref_ptr< osgText::Text > framerate_text;
			osg::ref_ptr< osgText::Text > wcs_x_text;
			osg::ref_ptr< osgText::Text > wcs_y_text;
			osg::ref_ptr< osgText::Text > wcs_z_text;

			VE_SceneGraph::CADEntity* wcs_model;
   };
}

#endif //DISPLAY_INFORMATION
