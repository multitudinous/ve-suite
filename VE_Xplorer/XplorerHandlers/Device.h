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
#ifndef DEVICE_H
#define DEVICE_H
/*!\file Device.h
Device API
*/
/*!\class VE_XPlorer::Device
* 
*/
#include "VE_Installer/include/VEConfig.h"

#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"

#include <osg/ref_ptr>

namespace osg 
{
  class Vec3f;
}

namespace VE_XML
{
   class Command;
}

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS Device : public cfdGlobalBase
{
public:
   Device();
   virtual ~Device(){;}

	virtual void UpdateNavigation();
   virtual void UpdateSelection();
   virtual void SetVECommand( VE_XML::Command* command=0){;}
   virtual void UpdateCommand(){;}
   virtual bool CheckCommandId( VE_Xplorer::cfdCommandArray * _cfdCommandArray= 0 ){;}
protected:
   virtual void ProcessSelection();
   virtual void SetStartEndPoint( osg::Vec3f* startPoint, osg::Vec3f* endPoint );
   virtual void DrawLine( osg::Vec3f startPoint, osg::Vec3f endPoint );
};
}

#endif //DEVICE_H
