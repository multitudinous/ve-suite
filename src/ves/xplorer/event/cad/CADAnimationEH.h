/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 * Date modified: $Date: 2009-01-19 15:39:10 -0600 (Mon, 19 Jan 2009) $
 * Version:       $Rev: 12099 $
 * Author:        $Author: mccdo $
 * Id:            $Id: CADAnimationEH.h 12099 2009-01-19 21:39:10Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef VE_CAD_ANIMATION_EVENT_HANDLER_H
#define VE_CAD_ANIMATION_EVENT_HANDLER_H

#include <ves/xplorer/event/cad/CADEventHandler.h>
#include <ves/open/xml/XMLObjectPtr.h>

#include <vector>
#include <string>
#include <map>

#include <osg/Vec3>
#include <osg/ref_ptr>

namespace osg
{
   class Node;
   class Image;
   class Drawable;
   class AnimationPath;
   class Geode;
   class Geometry;
}

namespace ves
{
namespace xplorer
{
namespace event
{
/*!\file CADAnimationEventHandler.h
  CADTransfomrEventHandler API
  */
/*!\class CADTransfomrEventHandler
 * Class for handling CADNode Animations.
 */
class VE_XPLORER_EXPORTS CADAnimationEventHandler : public CADEventHandler
{
public:
    ///Constructor
    CADAnimationEventHandler();

    ///Copy Constructor
    CADAnimationEventHandler( const CADAnimationEventHandler& rhs );

    ///Destructor
    virtual ~CADAnimationEventHandler();

    ///Equal operator
    CADAnimationEventHandler& operator=( const CADAnimationEventHandler& rhs );

    osg::ref_ptr< osg::AnimationPath > createAnimationPath( std::string component );
    
    std::vector< double > offDirx;
    std::map< std::string, std::vector< float > > objectOne;

protected:
    ///Update a transform on the CADNode.
    ///\param command The Command containing the udpated animation.
    void _operateOnNode( ves::open::xml::XMLObjectPtr command );
    void _readData( std::string );
};

}
}
}

#endif// VE_EVENT_HANDLER_H
