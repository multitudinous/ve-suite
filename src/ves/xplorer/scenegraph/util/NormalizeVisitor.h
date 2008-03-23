/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 * Date modified: $Date: 2008-03-13 21:07:47 -0500 (Thu, 13 Mar 2008) $
 * Version:       $Rev: 10811 $
 * Author:        $Author: dshipton $
 * Id:            $Id: OpacityVisitor.h 10811 2008-03-14 02:07:47Z dshipton $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef VES_XPLORER_NORMALIZE_VISITOR_H
#define VES_XPLORER_NORMALIZE_VISITOR_H
/*!\file NormalizeVisitor.h
NormalizeVisitor API
*/
/*!\class NormalizeVisitor
*
*/
#include <ves/VEConfig.h>

#include <osg/ref_ptr>
#include <osg/NodeVisitor>
namespace osg
{
    class StateSet;
}
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace util
{
class VE_SCENEGRAPH_UTILS_EXPORTS NormalizeVisitor : public osg::NodeVisitor
{
public:
    NormalizeVisitor( osg::Node* osg_node, bool state );
    virtual ~NormalizeVisitor();

    virtual void apply( osg::Group& node );

private:
    //setup the normalize state for the node
    ///\param stateset The stateset to modify
    void SetupNormalizeForStateSet( osg::StateSet* stateset);
    ///Decide whether to set normalize on the nodes
    bool mNormalize;
};
}
}
}
}

#endif //VES_XPLORER_NORMALIZE_VISITOR_H
