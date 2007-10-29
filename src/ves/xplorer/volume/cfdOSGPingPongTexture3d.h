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
#ifndef CFD_OSG_PING_PONG_TEXTURE_3D_H
#define CFD_OSG_PING_PONG_TEXTURE_3D_H
/*!\file cfdOSGPingPongTexture3D.h
* cfdOSGPingPongTexture3D API
*/

/*!\class ves::xplorer::volume::cfdOSGPingPongTexture3D
*
*/
#ifdef _PERFORMER
#elif _OSG
#include <osg/Node>
namespace osg
{
   class Texture3D;
}
#include <ves/VEConfig.h>
namespace ves
{
namespace xplorer
{
namespace volume
{
   class VE_TEXTURE_BASED_EXPORTS cfdOSGPingPongTexture3D{
      public:
         cfdOSGPingPongTexture3D();
         cfdOSGPingPongTexture3D(const cfdOSGPingPongTexture3D& pp);
         virtual ~cfdOSGPingPongTexture3D();

         void SetPingTexture(unsigned int tunit,osg::Node* ping);
         void SetPongTexture(unsigned int tunit,osg::Node* pong);
         void PingPongTextures();
         osg::Texture3D* GetCurrentTexture();
   
         cfdOSGPingPongTexture3D& operator=(const cfdOSGPingPongTexture3D& pp);
      protected:
         osg::ref_ptr<osg::Node> _previous;
         osg::ref_ptr<osg::Node> _current;
         unsigned int _pingUnit;
         unsigned int _pongUnit;
   };
}
}
}
#endif //_OSG
#endif // CFD_OSG_PING_PONG_TEXTURE_3D_H
